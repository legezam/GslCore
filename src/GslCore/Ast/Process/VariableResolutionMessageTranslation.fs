// TODO: this file should be here only until Inlining has been moved away from 
namespace GslCore.Ast.MessageTranslation

open GslCore.Ast.ErrorHandling

module VariableResolutionMessage =
    open GslCore.Ast.Process.VariableResolution

    let toAstMessage: VariableResolutionError -> AstMessage =
        function
        | TypeCheckError (typeCheckResult, node, variableName) ->
            match typeCheckResult with
            | InternalTypeMismatch (boundValue, targetType) ->
                AstResult.internalTypeMismatchMsg (Some("variable type checking")) (targetType.ToString()) boundValue
            | VariableTypeMismatch (elidedType, targetType) ->
                AstResult.variableTypeMismatchMsg variableName elidedType targetType node

        | IllegalFunctionLocal (variableName, node) ->
            AstResult.errStringFMsg
                (InternalError(AstMessageType.UnresolvedVariable))
                "A variable resolved to a function local during strict variable resolution: %s"
                variableName
                node
        | UnresolvedVariable (variableName, node) ->
            AstResult.errStringMsg AstMessageType.UnresolvedVariable variableName node