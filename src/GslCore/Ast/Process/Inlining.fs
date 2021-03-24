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

[<RequireQualifiedAccess>]
type FunctionValidationError = ParameterNumberMismatch of functionCall: FunctionCall * neededArgs: int * passedArgs: int

[<RequireQualifiedAccess>]
type InliningError =
    | VariableResolution of VariableResolutionError
    | Validation of node: AstNode * FunctionValidationError
    | MissingFunctionLocals of parseFunction: ParseFunction
    | FunctionCallTypeMismatch of node: AstNode
    | FunctionBodyTypeMismatch of node: AstNode
    | MissingFunctionBody of parseFunc: ParseFunction * node: AstNode
    | UnresolvedFunction of functionCall: FunctionCall * node: AstNode

module FunctionInliningError =
    let makeFunctionValidationError node err = InliningError.Validation(node, err)

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
    let internal checkArguments (parseFunction: ParseFunction)
                                (functionCall: FunctionCall)
                                : GslResult<unit, FunctionValidationError> =
        let neededArgs = parseFunction.ArgumentNames.Length
        let passedArgs = functionCall.Arguments.Length
        // make sure we have the right number of arguments
        if passedArgs <> neededArgs then
            GslResult.err (FunctionValidationError.ParameterNumberMismatch(functionCall, neededArgs, passedArgs))
        else
            GslResult.ok ()

    /// Create a local variable from a typed value.
    let internal createVariableBindingFromTypedValueAndName (resolveVariable: AstNode -> NodeTransformResult<VariableResolutionError>)
                                                            (name: string)
                                                            (maybeTypedValue: AstNode)
                                                            : GslResult<AstNode, InliningError> =
        match maybeTypedValue with
        | AstNode.TypedValue typedValueWrapper ->
            let (varType, typedValue) = typedValueWrapper.Value
            // using the existing variable bindings, resolve any variables contained in this value
            // this ensures that function locals never resolve to each other.
            AstTreeHead typedValue
            |> FoldMap.map Serial TopDown resolveVariable
            |> GslResult.mapError InliningError.VariableResolution
            |> GslResult.map (fun (AstTreeHead newVal) ->
                AstNode.VariableBinding
                    { Node.Value =
                          { VariableBinding.Name = name
                            Type = varType
                            Value = newVal }
                      Positions = typedValueWrapper.Positions })
        | x -> GslResult.err (InliningError.FunctionCallTypeMismatch x)

    /// Inline the passed function args in place of the FunctionLocals placeholder.
    /// Return a revised block.
    let internal inlinePassedArguments (createVariableBinding: string -> AstNode -> GslResult<AstNode, InliningError>)
                                      (parseFunction: ParseFunction)
                                      (functionCall: FunctionCall)
                                      : GslResult<AstNode, InliningError> =
        match parseFunction.Body with
        | AstNode.Block blockWrapper as block ->
            match blockWrapper.Value with
            // We require a block whose head is a FunctionLocal or something is fishy.
            | head :: tail ->
                match head with
                | AstNode.FunctionLocals _ ->
                    Seq.zip parseFunction.ArgumentNames functionCall.Arguments // zip up the args with the arg names
                    |> Seq.map (fun (argumentName, argument) -> createVariableBinding argumentName argument) // map them to local variables
                    |> Seq.toList
                    |> GslResult.collectA
                    // if unpacking and conversion succeeded, make a new block with the
                    // variable declarations followed by the rest of the block
                    |> GslResult.map (fun variableBindings ->
                        AstNode.Block
                            { blockWrapper with
                                  Value = variableBindings @ tail })
                | _ -> GslResult.err (InliningError.MissingFunctionLocals parseFunction)
            | [] -> GslResult.err (InliningError.MissingFunctionBody(parseFunction, block))
        | x -> GslResult.err (InliningError.FunctionBodyTypeMismatch x)

    /// Replace a function call with the contents of a function definition.
    let private inlineFunctionCall (checkArguments: ParseFunction -> FunctionCall -> GslResult<unit, FunctionValidationError>)
                                   (inlineArguments: CapturedVariableBindings -> ParseFunction -> FunctionCall -> GslResult<AstNode, InliningError>)
                                   (state: FunctionInliningState)
                                   (node: AstNode)
                                   : GslResult<AstNode, InliningError> =
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
                checkArguments functionDefinition functionCall
                |> GslResult.mapError (FunctionInliningError.makeFunctionValidationError node)
                >>= fun _ -> inlineArguments state.Variables functionDefinition functionCall
                |> GslResult.map AstTreeHead // needed to adapt to the map function
                >>= FoldMap.map Serial TopDown addPositions
                |> GslResult.map AstTreeHead.WrappedNode

            | None -> GslResult.err (InliningError.UnresolvedFunction(functionCall, node))
        | _ -> GslResult.ok node

    let internal inlineArgumentsUsingVariableResolution (capturedBindings: CapturedVariableBindings)
                                                        : ParseFunction -> FunctionCall -> GslResult<AstNode, InliningError> =
        let resolveVariable =
            VariableResolution.resolveVariable Strict capturedBindings

        let createVariableBinding =
            createVariableBindingFromTypedValueAndName resolveVariable

        inlinePassedArguments createVariableBinding

    let inlineFunctionCalls =
        let foldMapParameters =

            { FoldMapParameters.Direction = TopDown
              Mode = Serial
              StateUpdate = updateFunctionInliningState
              Map = inlineFunctionCall checkArguments inlineArgumentsUsingVariableResolution }

        FoldMap.foldMap FunctionInliningState.empty foldMapParameters
