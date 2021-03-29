namespace GslCore.Ast.Process.VariableResolution

open GslCore.Ast.Types
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
        | AstNode.VariableBinding variableBinding -> captureVariableBinding capturedBindings variableBinding
        | AstNode.FunctionLocals functionLocals ->
            functionLocals.Value.Names
            |> List.fold captureFunctionLocalBinding capturedBindings
        | _ -> capturedBindings

    let internal captureVariableBindings =
        StateUpdateMode.pretransformOnly captureVariableBindingsInner

[<RequireQualifiedAccess>]
type TypeCheckError =
    /// Value that is passed is not possible to elide
    | CannotElide of boundValue: AstNode * targetType: GslVariableType
    /// Elision resolves to a different type than the target type
    | ElisionConflict of elidedType: GslVariableType * targetType: GslVariableType

[<RequireQualifiedAccess>]
type VariableResolutionError =
    /// Variable resolution failed during type check
    | TypeCheck of result: TypeCheckError * node: AstNode * variableName: string
    /// Found a FunctionLocal while it is disallowed
    | IllegalFunctionLocal of variableName: string * node: AstNode
    /// Couldn't resolve the variable after all attempts
    | UnresolvedVariable of variableName: string * node: AstNode

module VariableResolution =

    /// Elide a type for a value node, if it corresponds to a valid GslVarType.
    let internal elideType: AstNode -> GslVariableType option =
        function
        | AstNode.Part _ -> Some(PartType)
        | AstNode.Int _ -> Some(IntType)
        | AstNode.Float _ -> Some(FloatType)
        | AstNode.String _ -> Some(StringType)
        | _ -> None


    /// Perform type checking on a variable.
    /// If the variable is untyped but has a real payload, try to elide its type.
    let internal typeCheck (targetType: GslVariableType)
                           (boundValueType: GslVariableType)
                           (boundValue: AstNode)
                           : GslResult<AstNode, TypeCheckError> =
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
                GslResult.err (TypeCheckError.ElisionConflict(elidedType, targetType))
            | None -> // whatever this thing is, it shouldn't be inside a variable
                GslResult.err (TypeCheckError.CannotElide(boundValue, targetType))
        else
            // type mismatch
            GslResult.err (TypeCheckError.ElisionConflict(boundValueType, targetType))


    module VariableResolutionError =
        let makeTypeCheckError node variableName tcResult =
            VariableResolutionError.TypeCheck(tcResult, node, variableName)

    /// Resolve a typed variable to a variable declaration.
    /// If that declaration itself was a variable aliasing (let foo = &bar), recurse
    /// down until we resolve to a fully typed variable.
    let rec private resolveVariableRecursive (mode: VariableResolutionMode)
                                             (capturedBindings: CapturedVariableBindings)
                                             (targetType: GslVariableType)
                                             (typeWrapper: Node<string * GslVariableType>)
                                             (topNode: AstNode)
                                             : GslResult<AstNode, VariableResolutionError> =
        let variableName, _ = typeWrapper.Value
        // first see if we have this guy in our bindings at all
        match capturedBindings |> Map.tryFind variableName with
        | Some (VariableResolutionWrapper.VariableBinding capturedBinding) -> // this name is resolves to a bound variable
            // does it have the right type in this context?
            let declaredType = capturedBinding.Value.Type
            let declaredValue = capturedBinding.Value.Value

            match declaredType, declaredValue with
            | NotYetTyped, AstNode.TypedVariable declaredValueInner ->
                // if this variable is just a reference to another variable, we need to recurse on it.
                resolveVariableRecursive mode capturedBindings targetType declaredValueInner topNode
            | _, boundValue ->
                // otherwise, perform type checking and resolve the variable if it type checks
                typeCheck targetType declaredType boundValue
                |> GslResult.mapError (VariableResolutionError.makeTypeCheckError topNode variableName)
        | Some VariableResolutionWrapper.FunctionLocal -> // This name resolves to a function local variable.  If we're allowing them, continue.
            match mode with
            | AllowUnresolvedFunctionLocals -> GslResult.ok topNode
            | Strict -> GslResult.err (VariableResolutionError.IllegalFunctionLocal(variableName, topNode))
        | None -> GslResult.err (VariableResolutionError.UnresolvedVariable(variableName, topNode))

    ///Given resolution state and an AST node, possibly resolve a reference.
    let internal resolveVariable (mode: VariableResolutionMode)
                                 (capturedBindings: CapturedVariableBindings)
                                 (node: AstNode)
                                 : GslResult<AstNode, VariableResolutionError> =
        match node with
        | AstNode.TypedVariable typedVariable ->
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
              StateUpdate = FoldMapParameters.alwaysOkUpdate VariableCapturing.captureVariableBindings
              Map = resolveVariable AllowUnresolvedFunctionLocals }

        FoldMap.foldMap Map.empty foldMapParameters

    /// Transform an AST with unresolved scoped variables into a tree with resolved scoped variables.
    /// Fails on unresolved function locals.
    let resolveVariablesStrict =
        let foldMapParameters =
            { FoldMapParameters.Direction = TopDown
              Mode = Serial
              StateUpdate = FoldMapParameters.alwaysOkUpdate VariableCapturing.captureVariableBindings
              Map = resolveVariable Strict }

        FoldMap.foldMap Map.empty foldMapParameters
