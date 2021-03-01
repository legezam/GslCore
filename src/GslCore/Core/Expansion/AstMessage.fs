namespace GslCore.Core.Expansion.AstMessage

open GslCore.Core.Expansion.Bootstrapping
open GslCore.Ast.ErrorHandling
open GslCore.Ast.Phase1
open GslCore.Ast.Types
open GslCore.Ast
open GslCore.Ast.Algorithms
open GslCore.Core.Expansion.Level2Expansion
open GslCore.Legacy.AstMessage

module BootstrapError =
    let toAstMessage (formatExternalError: 'a -> AstMessage): BootstrapError<'a> -> AstMessage =
        function
        | BootstrapError.LexParse innerError -> LexParseError.toAstMessage innerError
        | BootstrapError.LexParseSupplement node ->
            let contextMsg =
                sprintf "An error occurred while parsing this internally-generated GSL source code:\n%s" node.Value

            AstMessage.createErrorWithStackTrace (InternalError(ParserError)) contextMsg (AstNode.String node)
        | BootstrapError.ExternalOperation externalError -> formatExternalError externalError
        | BootstrapError.Unpack (expectedType, maybeNote, node) ->
            let extraText =
                match maybeNote with
                | Some n -> sprintf " %s" n
                | None -> ""

            let msg =
                sprintf "Unable to unpack as a '%s'.%s" expectedType extraText

            AstResult.errStringMsg (BootstrapError(Some(node))) msg node

module BootstrapExpandAssemblyError =
    let toAstMessage (formatBootstrapError: 'a -> AstMessage)
                     (formatExpansionError: 'b -> AstMessage)
                     : BootstrapExpandAssemblyError<'a, 'b> -> AstMessage =
        function
        | BootstrapExpandAssemblyError.AssemblyCreation innerError ->
            innerError
            |> LegacyAssemblyCreationError.toAstMessage
        | BootstrapExpandAssemblyError.Expansion innerError -> innerError |> formatExpansionError
        | BootstrapExpandAssemblyError.Bootstrapping innerError -> innerError |> formatBootstrapError


module BootstrapExecutionError =
    let toAstMessage (formatExternalError: 'a -> AstMessage): BootstrapExecutionError<'a> -> AstMessage =
        function
        | BootstrapExecutionError.AssemblyStuffing innerError -> innerError |> AssemblyStuffingError.toAstMessage
        | BootstrapExecutionError.ExternalOperation innerError -> innerError |> formatExternalError

module Level2ExpansionError =
    let toAstMessage: Level2ExpansionError -> AstMessage =
        function
        | ExpansionException (ex, node) -> AstResult.exceptionToError L2ExpansionError node ex
        | L2LineCreationError innerError -> LegacyL2LineCreationError.toAstMessage innerError
        | AssemblyStuffingFailure innerError -> AssemblyStuffingError.toAstMessage innerError
        | BoostrapError innerError -> BootstrapError.toAstMessage Phase1Error.toAstMessage innerError
