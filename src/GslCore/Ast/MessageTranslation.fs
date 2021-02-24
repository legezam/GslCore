namespace GslCore.Ast.MessageTranslation

open GslCore.Ast.ErrorHandling

module RelativePositionTranslationMessage =
    open GslCore.Ast.Process.RelativePositionTranslation

    let toAstMessage: RelativePositionTranslationMessage -> AstMessage =
        function
        | CalculationError (node, calcError) ->
            match calcError with
            | NegativeLeftAminoAcidStartPosition position ->
                AstMessage.createErrorWithStackTrace
                    ValueError
                    (sprintf "Cannot begin with a negative amino acid offset: %d" position)
                    node

            | NegativeRightAminoAcidStartPosition position ->
                AstMessage.createErrorWithStackTrace
                    ValueError
                    (sprintf "Cannot offset negative amino acids from start: %d" position)
                    node
            | PositionCannotBeZero ->
                AstMessage.createErrorWithStackTrace ValueError (sprintf "Slice index cannot be zero") node

        | PositionIsNotInteger node -> AstResult.internalTypeMismatchMsg (Some "relative position building") "Int" node

module VariableResolutionMessage =
    open GslCore.Ast.Process.VariableResolution

    let toAstMessage: VariableResolutionError -> AstMessage =
        function
        | TypeCheckError (typeCheckResult, node, variableName) ->
            match typeCheckResult with
            | ElisionFailure (boundValue, targetType) ->
                AstResult.internalTypeMismatchMsg (Some("variable type checking")) (targetType.ToString()) boundValue
            | ElisionResolvesToDifferentTypeError (elidedType, targetType) ->
                AstResult.variableTypeMismatchMsg variableName elidedType targetType node

        | IllegalFunctionLocal (variableName, node) ->
            AstResult.errStringFMsg
                (InternalError(AstMessageType.UnresolvedVariable))
                "A variable resolved to a function local during strict variable resolution: %s"
                variableName
                node
        | UnresolvedVariable (variableName, node) ->
            AstResult.errStringMsg AstMessageType.UnresolvedVariable variableName node

module FunctionInliningMessage =
    open GslCore.Ast.Process.Inlining

    let toAstMessage: FunctionInliningError -> AstMessage =
        function
        | VariableResolution err -> err |> VariableResolutionMessage.toAstMessage
        | ParameterNumberMismatchError (functionCallNode, functionCall, neededArgs, passedArgs) ->
            AstResult.errStringMsg
                TypeError
                (sprintf "Function '%s' expects %d arguments but received %d." functionCall.Name neededArgs passedArgs)
                functionCallNode
        | MissingFunctionLocalsError parseFunction ->
            AstResult.errStringMsg
                (InternalError(TypeError))
                "No function locals node found in function definition block."
                parseFunction.Body
        | InternalFunctionCallTypeMismatch node ->
            AstResult.internalTypeMismatchMsg (Some "function call") "typed value" node
        | InternalFunctionBodyTypeMismatch node -> AstResult.internalTypeMismatchMsg (Some "function body") "Block" node
        | UnresolvedFunction (functionCall, node) ->
            AstResult.errStringMsg AstMessageType.UnresolvedFunction functionCall.Name node
