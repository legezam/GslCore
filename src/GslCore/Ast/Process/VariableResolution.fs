namespace GslCore.Ast.Process

open Amyris.ErrorHandling
open GslCore.AstTypes
open GslCore.AstErrorHandling
open GslCore.AstAlgorithms



// ===================
// variable resolution
// ===================

type VariableResolutionMode =
    | Strict
    | AllowUnresolvedFunctionLocals

/// Wrapper type for variable resolution.  Node helper functions below.
type VariableResolutionWrapper =
    | VBinding of Node<VariableBinding>
    | FLocal

/// Map to keep track of what variables and function locals are in scope.
/// Function locals are items in the map with None rather that an explicit binding.
/// Shadowing is allowed, and the latest declared name takes precedence.
type VariableBindings = Map<string, VariableResolutionWrapper>

module VariableResolution =
    let private addBinding (bindings: VariableBindings) (variableBinding: Node<VariableBinding>) =
        bindings.Add(variableBinding.Value.Name, VBinding variableBinding)

    let private addFuncLocal (bindings: VariableBindings) (name: string) = bindings.Add(name, FLocal)

    /// Given an AST node, update the variable resolution state.
    /// We need to be a little careful here due to a tricky issue.
    /// Namely, we need to ensure that the construction let bar = &bar
    /// doesn't wipe out the upstream binding to bar, lest we end up
    /// in an infinite recursive loop trying to resolve a self-reference.
    let private updateVariableResolutionInner (variableBindings: VariableBindings) (node: AstNode): VariableBindings =
        match node with
        | SelfReferentialVariable _ ->
            // variable that aliases itself from an outer scope.  Ignore this.
            variableBindings
        | VariableBinding variableBinding -> addBinding variableBindings variableBinding
        | FunctionLocals functionLocals ->
            functionLocals.Value.Names
            |> List.fold addFuncLocal variableBindings
        | _ -> variableBindings

    let internal updateVariableResolution =
        pretransformOnly updateVariableResolutionInner

    /// Elide a type for a value node, if it corresponds to a valid GslVarType.
    let private elideType (node: AstNode): GslVariableType option =
        match node with
        | Part _ -> Some(PartType)
        | Int _ -> Some(IntType)
        | Float _ -> Some(FloatType)
        | String _ -> Some(StringType)
        | _ -> None

    /// Perform type checking on a variable.
    /// If the variable is untyped but has a real payload, try to elide its type.
    let private typeCheck (varName: string)
                          (node: AstNode)
                          (targetType: GslVariableType)
                          (boundValueType: GslVariableType)
                          (boundValue: AstNode)
                          : Result<AstNode, AstMessage> =
        if targetType = NotYetTyped
           || targetType = boundValueType then
            // exact type check or destination is not strongly typed
            ok boundValue
        elif boundValueType = NotYetTyped then
            // our value doesn't have type information, see if we can elide it
            match elideType boundValue with
            | Some elidedType when elidedType = targetType -> // elides to correct type
                ok boundValue
            | Some elidedType -> // elides to incorrect type
                AstMessage.variableTypeMismatch varName elidedType targetType node
            | None -> // whatever this thing is, it shouldn't be inside a variable
                AstMessage.internalTypeMismatch (Some("variable type checking")) (targetType.ToString()) boundValue
        else
            // type mismatch
            AstMessage.variableTypeMismatch varName boundValueType targetType node


    /// Resolve a typed variable to a variable declaration.
    /// If that declaration itself was a variable aliasing (let foo = &bar), recurse
    /// down until we resolve to a fully typed variable.
    let rec private resolveVariableRecursive (mode: VariableResolutionMode)
                                             (variableBindings: VariableBindings)
                                             (targetType: GslVariableType)
                                             (typeWrapper: Node<string * GslVariableType>)
                                             (node: AstNode)
                                             : Result<AstNode, AstMessage> =
        let varName, _ = typeWrapper.Value
        // first see if we have this guy in our bindings at all
        match variableBindings.TryFind(varName) with
        | Some (VBinding variableBinding) -> // this name is resolves to a bound variable
            // does it have the right type in this context?
            let declaredType = variableBinding.Value.Type

            match declaredType, variableBinding.Value.Value with
            | NotYetTyped, TypedVariable typedVariableInner ->
                // if this variable is just a reference to another variable, we need to recurse on it.
                resolveVariableRecursive mode variableBindings targetType typedVariableInner node
            | _, boundValue ->
                // otherwise, perform type checking and resolve the variable if it type checks
                typeCheck varName node targetType declaredType boundValue
        | Some FLocal -> // This name resolves to a function local variable.  If we're allowing them, continue.
            match mode with
            | AllowUnresolvedFunctionLocals -> ok node
            | Strict ->
                AstMessage.createErrorf
                    (InternalError(UnresolvedVariable))
                    "A variable resolved to a function local during strict variable resolution: %s"
                    varName
                    node
        | None ->
            // unresolved variable!
            AstMessage.createError UnresolvedVariable varName node

    ///Given resolution state and an AST node, possibly resolve a reference.
    let internal resolveVariable (mode: VariableResolutionMode)
                                 (variableBindings: VariableBindings)
                                 (node: AstNode)
                                 : Result<AstNode, AstMessage> =
        match node with
        | TypedVariable typedVariable ->
            let targetType = snd typedVariable.Value
            // might resolve to another variable, so we need to do this recursively
            resolveVariableRecursive mode variableBindings targetType typedVariable node
        | x -> ok x

    /// Transform an AST with unresolved scoped variables into a tree with resolved scoped variables.
    /// Variables that resolve to function arguments are left untouched in this phase.
    let resolveVariables =
        let foldMapParameters =
            { FoldMapParameters.Direction = TopDown
              Mode = Serial
              StateUpdate = updateVariableResolution
              Map = resolveVariable AllowUnresolvedFunctionLocals }

        FoldMap.foldMap Map.empty foldMapParameters

    /// Transform an AST with unresolved scoped variables into a tree with resolved scoped variables.
    /// Fails on unresolved function locals.
    let resolveVariablesStrict =
        let foldMapParameters =
            { FoldMapParameters.Direction = TopDown
              Mode = Serial
              StateUpdate = updateVariableResolution
              Map = resolveVariable Strict }

        FoldMap.foldMap Map.empty foldMapParameters
