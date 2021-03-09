namespace GslCore.Ast.Process.Inlining


open GslCore.Ast.Process
open GslCore.Ast.Process.VariableResolution
open GslCore.Ast.Types
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

type FunctionInliningError =
    | VariableResolution of VariableResolutionError
    | ParameterNumberMismatchError of
        functionCallNode: AstNode *
        functionCall: FunctionCall *
        neededArgs: int *
        passedArgs: int
    | MissingFunctionLocalsError of parseFunction: ParseFunction
    | InternalFunctionCallTypeMismatch of node: AstNode
    | InternalFunctionBodyTypeMismatch of node: AstNode
    | MissingFunctionBody of node: ParseFunction
    | UnresolvedFunction of functionCall: FunctionCall * node: AstNode

module Inlining =
    /// Capture a function definition.
    /// Also keep track of whether or not we are inside a function declaration, as we don't inline
    /// function calls inside other declarations, only at the final expanded call sites.
    let internal collectFunctionDefinition (mode: StateUpdateMode)
                                           (state: FunctionInliningState)
                                           : AstNode -> FunctionInliningState =
        function
        | AstNode.FunctionDef functionWrapper ->
            match mode with
            | PreTransform ->
                { state with
                      Definitions =
                          state.Definitions
                          |> Map.add functionWrapper.Value.Name functionWrapper.Value
                      Depth = state.Depth + 1 }
            | PostTransform -> { state with Depth = state.Depth - 1 }
        | _ -> state

    let private updateFunctionInliningState (mode: StateUpdateMode)
                                            (state: FunctionInliningState)
                                            (node: AstNode)
                                            : FunctionInliningState =
        let stateWithNewDefinitions =
            collectFunctionDefinition mode state node

        let updatedVars =
            VariableCapturing.captureVariableBindings mode state.Variables node

        { stateWithNewDefinitions with
              Variables = updatedVars }

    /// Check that a function call passed the right number of arguments.
    let private checkArguments (parseFunction: ParseFunction)
                               (functionCall: FunctionCall)
                               (functionCallNode: AstNode)
                               : GslResult<ParseFunction * FunctionCall, FunctionInliningError> =
        let neededArgs, passedArgs =
            parseFunction.ArgumentNames.Length, functionCall.Arguments.Length
        // make sure we have the right number of arguments
        if passedArgs <> neededArgs then
            GslResult.err (ParameterNumberMismatchError(functionCallNode, functionCall, neededArgs, passedArgs))
        else
            GslResult.ok (parseFunction, functionCall)

    /// Create a local variable from a typed value.
    let private localVarFromTypedValueAndName (variableBindings: CapturedVariableBindings)
                                              (argumentName: string, argumentNode: AstNode)
                                              : GslResult<AstNode, FunctionInliningError> =
        match argumentNode with
        | AstNode.TypedValue typedValueWrapper ->
            let (varType, typedValue) = typedValueWrapper.Value
            // using the existing variable bindings, resolve any variables contained in this value
            // this ensures that function locals never resolve to each other.
            AstTreeHead(typedValue)
            |> FoldMap.map Serial TopDown (VariableResolution.resolveVariable Strict variableBindings)
            |> GslResult.mapError VariableResolution
            |> GslResult.map (fun (AstTreeHead newVal) ->
                AstNode.VariableBinding
                    { Node.Value =
                          { VariableBinding.Name = argumentName
                            Type = varType
                            Value = newVal }
                      Positions = typedValueWrapper.Positions })
        | x -> GslResult.err (InternalFunctionCallTypeMismatch x)



    /// Inline the passed function args in place of the FunctionLocals placeholder.
    /// Return a revised block.
    let private inlinePassedArgs (variableBindings: CapturedVariableBindings)
                                 (parseFunction: ParseFunction, functionCall: FunctionCall)
                                 : GslResult<AstNode, FunctionInliningError> =
        match parseFunction.Body with
        | AstNode.Block blockWrapper ->
            match blockWrapper.Value with
            // We require a block whose head is a FunctionLocal or something is fishy.
            | head :: tail ->
                match head with
                | AstNode.FunctionLocals _ ->
                    Seq.zip parseFunction.ArgumentNames functionCall.Arguments // zip up the args with the arg names
                    |> Seq.map (localVarFromTypedValueAndName variableBindings) // map them to local variables
                    |> Seq.toList
                    |> GslResult.collectA
                    // if unpacking and conversion succeeded, make a new block with the
                    // variable declarations followed by the rest of the block
                    |> GslResult.map (fun variableBindings ->
                        AstNode.Block
                            { blockWrapper with
                                  Value = variableBindings @ tail })
                | _ -> GslResult.err (MissingFunctionLocalsError parseFunction)
            | [] -> GslResult.err (MissingFunctionBody parseFunction)
        | x -> GslResult.err (InternalFunctionBodyTypeMismatch x)

    /// Replace a function call with the contents of a function definition.
    let private inlineFunctionCall (state: FunctionInliningState)
                                   (node: AstNode)
                                   : GslResult<AstNode, FunctionInliningError> =
        match node with
        | AstNode.FunctionCall functionCallWrapper when state.Depth = 0 -> // only do inlining if we're not inside a def
            let functionCall = functionCallWrapper.Value

            match state.Definitions.TryFind(functionCall.Name) with
            | Some functionDefinition ->
                // Helper function to add new position to an AST node
                let addPositions (node: AstNode) =
                    GslResult.ok (Utils.prependPositionsAstNode functionCallWrapper.Positions node)

                // inline the args into the function call block
                // this new block replaces the function call
                checkArguments functionDefinition functionCall node
                >>= inlinePassedArgs state.Variables
                |> GslResult.map AstTreeHead // needed to adapt to the map function
                >>= FoldMap.map Serial TopDown addPositions
                |> GslResult.map AstTreeHead.WrappedNode

            | None -> GslResult.err (FunctionInliningError.UnresolvedFunction(functionCall, node))
        | _ -> GslResult.ok node

    let inlineFunctionCalls =
        let foldMapParameters =
            { FoldMapParameters.Direction = TopDown
              Mode = Serial
              StateUpdate = updateFunctionInliningState
              Map = inlineFunctionCall }

        FoldMap.foldMap FunctionInliningState.empty foldMapParameters
