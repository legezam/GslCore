namespace GslCore.Ast.Process.VariableResolution

open GslCore.Ast.Types
open GslCore.Ast.ErrorHandling
open GslCore.Ast.Algorithms
open GslCore.GslResult



// ===================
// variable resolution
// ===================

type VariableResolutionMode =
    | Strict
    | AllowUnresolvedFunctionLocals

/// Wrapper type for variable resolution.  Node helper functions below.
[<RequireQualifiedAccess>]
type VariableResolutionWrapper =
    /// Classic let binding to a variable
    | VariableBinding of Node<VariableBinding>
    /// Local variable binding in a function
    | FunctionLocal

/// Map to keep track of what variables and function locals are in scope.
/// Function locals are items in the map with None rather that an explicit binding.
/// Shadowing is allowed, and the latest declared name takes precedence.
type CapturedVariableBindings = Map<string, VariableResolutionWrapper>

module VariableCapturing =
    let private captureVariableBinding (capturedBindings: CapturedVariableBindings)
                                       (variableBinding: Node<VariableBinding>)
                                       : CapturedVariableBindings =
        capturedBindings
        |> Map.add variableBinding.Value.Name (VariableResolutionWrapper.VariableBinding variableBinding)

    let private captureFunctionLocalBinding (bindings: CapturedVariableBindings) (name: string): CapturedVariableBindings =
        bindings
        |> Map.add name VariableResolutionWrapper.FunctionLocal

    /// Given an AST node, update the variable resolution state.
    /// We need to be a little careful here due to a tricky issue.
    /// Namely, we need to ensure that the construction let bar = &bar
    /// doesn't wipe out the upstream binding to bar, lest we end up
    /// in an infinite recursive loop trying to resolve a self-reference.
    let private captureVariableBindingsInner (capturedBindings: CapturedVariableBindings)
                                             : AstNode -> CapturedVariableBindings =
        function
        | SelfReferentialVariable _ ->
            // variable that aliases itself from an outer scope.  Ignore this.
            capturedBindings
        | VariableBinding variableBinding -> captureVariableBinding capturedBindings variableBinding
        | FunctionLocals functionLocals ->
            functionLocals.Value.Names
            |> List.fold captureFunctionLocalBinding capturedBindings
        | _ -> capturedBindings

    let internal captureVariableBindings =
        StateUpdateMode.pretransformOnly captureVariableBindingsInner

type TypeCheckResult =
    | InternalTypeMismatch of boundValue: AstNode * targetType: GslVariableType
    | VariableTypeMismatch of variableName: string * elidedType: GslVariableType * targetType: GslVariableType

type VariableResolutionError =
    | TypeCheckError of TypeCheckResult * node: AstNode
    | IllegalFunctionLocal of variableName: string * node: AstNode
    | UnresolvedVariable of variableName: string * node: AstNode

module VariableResolution =

    /// Elide a type for a value node, if it corresponds to a valid GslVarType.
    let internal elideType: AstNode -> GslVariableType option =
        function
        | Part _ -> Some(PartType)
        | Int _ -> Some(IntType)
        | Float _ -> Some(FloatType)
        | String _ -> Some(StringType)
        | _ -> None


    /// Perform type checking on a variable.
    /// If the variable is untyped but has a real payload, try to elide its type.
    let internal typeCheck (varName: string)
                           (targetType: GslVariableType)
                           (boundValueType: GslVariableType)
                           (boundValue: AstNode)
                           : GslResult<AstNode, TypeCheckResult> =
        if targetType = NotYetTyped
           || targetType = boundValueType then
            // exact type check or destination is not strongly typed
            GslResult.ok boundValue
        elif boundValueType = NotYetTyped then
            // our value doesn't have type information, see if we can elide it
            match elideType boundValue with
            | Some elidedType when elidedType = targetType -> // elides to correct type
                GslResult.ok boundValue
            | Some elidedType -> // elides to incorrect type
                GslResult.err (VariableTypeMismatch(varName, elidedType, targetType))
            | None -> // whatever this thing is, it shouldn't be inside a variable
                GslResult.err (InternalTypeMismatch(boundValue, targetType))
        else
            // type mismatch
            GslResult.err (VariableTypeMismatch(varName, boundValueType, targetType))


    module VariableResolutionError =
        let makeTypeCheckError node tcResult = TypeCheckError(tcResult, node)

    /// Resolve a typed variable to a variable declaration.
    /// If that declaration itself was a variable aliasing (let foo = &bar), recurse
    /// down until we resolve to a fully typed variable.
    let rec private resolveVariableRecursive (mode: VariableResolutionMode)
                                             (capturedBindings: CapturedVariableBindings)
                                             (targetType: GslVariableType)
                                             (typeWrapper: Node<string * GslVariableType>)
                                             (node: AstNode)
                                             : GslResult<AstNode, VariableResolutionError> =
        let varName, _ = typeWrapper.Value
        // first see if we have this guy in our bindings at all
        match capturedBindings.TryFind(varName) with
        | Some (VariableResolutionWrapper.VariableBinding variableBinding) -> // this name is resolves to a bound variable
            // does it have the right type in this context?
            let declaredType = variableBinding.Value.Type

            match declaredType, variableBinding.Value.Value with
            | NotYetTyped, TypedVariable typedVariableInner ->
                // if this variable is just a reference to another variable, we need to recurse on it.
                resolveVariableRecursive mode capturedBindings targetType typedVariableInner node
            | _, boundValue ->
                // otherwise, perform type checking and resolve the variable if it type checks
                typeCheck varName targetType declaredType boundValue
                |> GslResult.mapError (VariableResolutionError.makeTypeCheckError node)
        | Some VariableResolutionWrapper.FunctionLocal -> // This name resolves to a function local variable.  If we're allowing them, continue.
            match mode with
            | AllowUnresolvedFunctionLocals -> GslResult.ok node
            | Strict -> GslResult.err (IllegalFunctionLocal(varName, node))
        | None -> GslResult.err (UnresolvedVariable(varName, node))

    ///Given resolution state and an AST node, possibly resolve a reference.
    let internal resolveVariable (mode: VariableResolutionMode)
                                 (capturedBindings: CapturedVariableBindings)
                                 (node: AstNode)
                                 : GslResult<AstNode, VariableResolutionError> =
        match node with
        | TypedVariable typedVariable ->
            let targetType = snd typedVariable.Value
            // might resolve to another variable, so we need to do this recursively
            resolveVariableRecursive mode capturedBindings targetType typedVariable node
        | x -> GslResult.ok x

    /// Transform an AST with unresolved scoped variables into a tree with resolved scoped variables.
    /// Variables that resolve to function arguments are left untouched in this phase.
    let resolveVariables =
        let foldMapParameters =
            { FoldMapParameters.Direction = TopDown
              Mode = Serial
              StateUpdate = VariableCapturing.captureVariableBindings
              Map = resolveVariable AllowUnresolvedFunctionLocals }

        FoldMap.foldMap Map.empty foldMapParameters

    /// Transform an AST with unresolved scoped variables into a tree with resolved scoped variables.
    /// Fails on unresolved function locals.
    let resolveVariablesStrict =
        let foldMapParameters =
            { FoldMapParameters.Direction = TopDown
              Mode = Serial
              StateUpdate = VariableCapturing.captureVariableBindings
              Map = resolveVariable Strict }

        FoldMap.foldMap Map.empty foldMapParameters
