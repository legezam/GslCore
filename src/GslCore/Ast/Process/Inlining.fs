namespace GslCore.Ast.Process


open GslCore.Ast.Process
open GslCore.Ast.Types
open GslCore.Ast.ErrorHandling
open GslCore.Ast.Algorithms
open GslCore.GslResult

// =====================
// inlining function calls
// =====================

type CollectedFunctionDefs = Map<string, ParseFunction>

type FunctionInliningState =
    { Definitions: CollectedFunctionDefs
      Variables: CapturedVariableBindings
      Depth: int }

module FunctionInliningState =
    let empty =
        { Definitions = Map.empty
          Variables = Map.empty
          Depth = 0 }

module Inlining =
    /// Capture a function definition.
    /// Also keep track of whether or not we are inside a function declaration, as we don't inline
    /// function calls inside other declarations, only at the final expanded call sites.
    let private collectFunctionDef (mode: StateUpdateMode)
                                   (state: FunctionInliningState)
                                   (node: AstNode)
                                   : FunctionInliningState =
        match node with
        | FunctionDef functionWrapper ->
            match mode with
            | PreTransform ->
                { state with
                      Definitions = state.Definitions.Add(functionWrapper.Value.Name, functionWrapper.Value)
                      Depth = state.Depth + 1 }
            | PostTransform -> { state with Depth = state.Depth - 1 }
        | _ -> state

    let private updateFunctionInliningState (mode: StateUpdateMode)
                                            (state: FunctionInliningState)
                                            (node: AstNode)
                                            : FunctionInliningState =
        let sWithNewDefs = collectFunctionDef mode state node

        let updatedVars =
            VariableResolution.updateVariableResolution mode state.Variables node

        { sWithNewDefs with
              Variables = updatedVars }

    /// Check that a function call passed the right number of arguments.
    let private checkArgs (parseFunction: ParseFunction)
                          (functionCall: FunctionCall)
                          (functionCallNode: AstNode)
                          : AstResult<ParseFunction * FunctionCall> =
        let neededArgs, passedArgs =
            parseFunction.ArgumentNames.Length, functionCall.Arguments.Length
        // make sure we have the right number of arguments
        if passedArgs <> neededArgs then
            AstResult.errString
                TypeError
                (sprintf "Function '%s' expects %d arguments but received %d." functionCall.Name neededArgs passedArgs)
                functionCallNode
        else
            GslResult.ok (parseFunction, functionCall)

    /// Create a local variable from a typed value.
    let private localVarFromTypedValueAndName (variableBindings: CapturedVariableBindings)
                                              (name: string, node: AstNode)
                                              : AstResult<AstNode> =
        match node with
        | TypedValue typedValueWrapper ->
            let (varType, typedValue) = typedValueWrapper.Value
            // using the existing variable bindings, resolve any variables contained in this value
            // this ensures that function locals never resolve to each other.
            AstTreeHead(typedValue)
            |> FoldMap.map Serial TopDown (VariableResolution.resolveVariable Strict variableBindings)
            |> GslResult.map (fun (AstTreeHead newVal) ->
                VariableBinding
                    { Value =
                          { Name = name
                            Type = varType
                            Value = newVal }
                      Positions = typedValueWrapper.Positions })
        | x -> AstResult.internalTypeMismatch (Some "function call") "typed value" x


    /// Inline the passed function args in place of the FunctionLocals placeholder.
    /// Return a revised block.
    let private inlinePassedArgs (variableBindings: CapturedVariableBindings)
                                 (parseFunction: ParseFunction, functionCall: FunctionCall)
                                 : AstResult<AstNode> =
        match parseFunction.Body with
        | Block blockWrapper ->
            match blockWrapper.Value with
            // We require a block whose head is a FunctionLocal or something is fishy.
            | head :: tail when (match head with
                                 // We require a block whose head is a FunctionLocal or something is fishy.
                                 | FunctionLocals _ -> true
                                 // We require a block whose head is a FunctionLocal or something is fishy.
                                 | _ -> false) ->
                Seq.zip parseFunction.ArgumentNames functionCall.Arguments // zip up the args with the arg names
                |> Seq.map (localVarFromTypedValueAndName variableBindings) // map them to local variables
                |> Seq.toList
                |> GslResult.collectA
                // if unpacking and conversion succeeded, make a new block with the
                // variable declarations followed by the rest of the block
                |> GslResult.map (fun variableBindings ->
                    Block
                        { blockWrapper with
                              Value = variableBindings @ tail })
            | _ ->
                AstResult.errString
                    (InternalError(TypeError))
                    "No function locals node found in function defintion block."
                    parseFunction.Body
        | x -> AstResult.internalTypeMismatch (Some "function body") "Block" x

    /// Replace a function call with the contents of a function definition.
    let private inlineFunctionCall (state: FunctionInliningState) (node: AstNode): AstResult<AstNode> =
        match node with
        | FunctionCall functionCallWrapper when state.Depth = 0 -> // only do inlining if we're not inside a def
            let functionCall = functionCallWrapper.Value

            match state.Definitions.TryFind(functionCall.Name) with
            | Some functionDefinition ->
                // Helper function to add new position to an AST node
                let addPositions (node: AstNode) =
                    GslResult.ok (Utils.prependPositionsAstNode functionCallWrapper.Positions node)

                // inline the args into the function call block
                // this new block replaces the function call
                checkArgs functionDefinition functionCall node
                >>= inlinePassedArgs state.Variables
                |> GslResult.map AstTreeHead // needed to adapt to the map function
                >>= FoldMap.map Serial TopDown addPositions
                |> GslResult.map (fun treeHead -> treeHead.wrappedNode)

            | None -> AstResult.errString UnresolvedFunction functionCall.Name node
        | _ -> GslResult.ok node

    let inlineFunctionCalls =
        let foldMapParameters =
            { FoldMapParameters.Direction = TopDown
              Mode = Serial
              StateUpdate = updateFunctionInliningState
              Map = inlineFunctionCall }

        FoldMap.foldMap FunctionInliningState.empty foldMapParameters
