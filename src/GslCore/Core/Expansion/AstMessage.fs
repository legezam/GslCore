namespace GslCore.Core.Expansion.AstMessage

open GslCore.Core.Expansion.Bootstrapping
open GslCore.Ast.ErrorHandling
open GslCore.Ast.Phase1
open GslCore.Ast.Types
open GslCore.Ast
open GslCore.Ast.Algorithms
open GslCore.Core.Expansion.HeterologyExpansion
open GslCore.Core.Expansion.Level2Expansion
open GslCore.Core.Expansion.MutationExpansion
open GslCore.Core.Expansion.ProteinExpansion
open GslCore.Legacy.AstMessage

module BootstrapError =
    let toAstMessage (formatExternalError: 'a -> AstMessage): BootstrapError<'a> -> AstMessage =
        function
        | BootstrapError.LexParse innerError -> LexParseError.toAstMessage innerError
        | BootstrapError.LexParseSupplement node ->
            let contextMsg =
                sprintf "An error occurred while parsing this internally-generated GSL source code:\n%s" node.Value

            AstMessage.createErrorWithStackTrace
                (AstMessageType.InternalError(AstMessageType.ParserError))
                contextMsg
                (AstNode.String node)
        | BootstrapError.ExternalOperation externalError -> formatExternalError externalError
        | BootstrapError.Unpack (expectedType, maybeNote, node) ->
            let extraText =
                match maybeNote with
                | Some n -> sprintf " %s" n
                | None -> ""

            let msg =
                sprintf "Unable to unpack as a '%s'.%s" expectedType extraText

            AstResult.errStringMsg (AstMessageType.BootstrapError(Some(node))) msg node

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
        | ExpansionException (ex, node) -> AstResult.exceptionToError AstMessageType.L2ExpansionError node ex
        | L2LineCreationError innerError -> LegacyL2LineCreationError.toAstMessage innerError
        | AssemblyStuffingFailure innerError -> AssemblyStuffingError.toAstMessage innerError
        | BoostrapError innerError -> BootstrapError.toAstMessage Phase1Error.toAstMessage innerError

module ProteinExpansionError =
    let toAstMessage (node: AstNode): ProteinExpansionError -> AstMessage =
        function
        | ProteinExpansionError.IllegalAminoAcid aminoAcid ->
            AstResult.errStringFMsg
                AstMessageType.ProteinError
                "Protein sequence contains illegal amino acid '%c'"
                aminoAcid
                node
        | ProteinExpansionError.MissingReferenceGenome refGenome ->
            AstResult.errStringFMsg
                AstMessageType.ProteinError
                "Unable to load refgenome %s to determine environment"
                refGenome
                node
        | ProteinExpansionError.IllegalSeedPragmaArgument seed ->
            AstResult.errStringFMsg AstMessageType.ProteinError "#seed argument '%s' is not a valid integer" seed node

module HeterologyExpansionError =
    let toAstMessage (node: AstNode): HeterologyExpansionError -> AstMessage =
        function
        | HeterologyExpansionError.NonGeneNeighbourPart neighbourGene ->
            AstResult.errStringFMsg
                AstMessageType.HetBlockError
                "Heterology block must be adjacent to g part, %s not allowed"
                neighbourGene
                node
        | HeterologyExpansionError.ExternalPartResolutionFailure (partId, externalError) ->
            AstResult.errStringMsg
                AstMessageType.HetBlockError
                (sprintf "Fail fetching %s. %s" partId.Id externalError)
                node
        | HeterologyExpansionError.HeterologyBlockLeftInDesign source ->
            AstResult.errStringMsg
                AstMessageType.HetBlockError
                (sprintf "Attempt to expand heterology block in design %s left remaining hetblock" source.String)
                node
        | HeterologyExpansionError.InvalidLenPragma value ->
            AstResult.errStringMsg
                AstMessageType.HetBlockError
                (sprintf "Expected integer in #len pragma, not '%s'" value)
                node

module MutationExpansionError =
    let toAstMessage (node: AstNode): MutationExpansionError -> AstMessage =
        function
        | MutationExpansionError.ExternalProviderError err ->
            AstResult.errStringMsg
                AstMessageType.MutationError
                (sprintf "External Allele Swap Provider reported an error while expanding mutations: %s" err)
                node

        | MutationExpansionError.MultiPartAssemblyError parts ->
            AstResult.errStringMsg
                AstMessageType.MutationError
                (sprintf
                    "Tried to perform a mutation expansion on an assembly with more than one part: %A.  This is currently not supported."
                     parts)
                node

        | MutationExpansionError.MultipleMutationError mutations ->
            AstResult.errStringMsg
                AstMessageType.MutationError
                (sprintf "Internal error, found more than one mutation mod: %A" mutations)
                node

        | MutationExpansionError.IllegalSwapEnd swapend ->
            AstResult.errStringMsg
                AstMessageType.MutationError
                (sprintf "#swapend argument should be 5 or 3. Instead got: %s" swapend)
                node

        | MutationExpansionError.UnknownGene part ->
            AstResult.errStringMsg
                AstMessageType.MutationError
                (sprintf "Undefined gene '%s' %O\n" (part.Gene.[1..]) (part.Where))
                node

        | MutationExpansionError.UnknownAlleleSwapStyle style ->
            AstResult.errStringMsg
                AstMessageType.MutationError
                (sprintf "Undefined allele swap style '%s', options are long or short" style)
                node


        | MutationExpansionError.IllegalGenePartPrefix genePart ->
            AstResult.errStringMsg
                AstMessageType.MutationError
                (sprintf
                    "Allele swap gene must be g-type  e.g gABC1$x1234y.  '%c' is not a legal prefix for %s"
                     (genePart.Gene.[0])
                     genePart.Gene)
                node
