namespace GslCore.Legacy.AstMessage

open GslCore.Ast.Types
open GslCore.Ast.ErrorHandling
open GslCore.DesignParams
open GslCore.PcrParamParse
open GslCore.Legacy

module LegacyAssemblyCreationError =
    let makeDesignParamCreationError (node: AstNode) (originalError: DesignParameterError) =
        DesignParamCreationError(originalError, node)

    let toAstMessage: LegacyAssemblyCreationError -> AstMessage =
        function
        | DesignParamCreationError (paramError, node) ->

            match paramError with
            | PcrParameterRevisionError err ->
                let message = err |> PcrParameterParseError.toString
                AstMessage.createErrorWithStackTrace PragmaError message node
        | PartCreationError partError ->
            match partError with
            | IllegalSliceConstruction (left, right) ->
                let context =
                    sprintf "legacy slice construction; found [%s:%s]" left.TypeName right.TypeName

                AstResult.internalTypeMismatchMsg (Some context) "RelPos" left
            | IllegalModifierNode node ->
                let context = Some "legacy mod conversion"
                AstResult.internalTypeMismatchMsg context "Slice or Mutation or DotMod" node
            | IllegalLegacyBasePart node ->
                let context = Some "legacy part conversion"
                AstResult.internalTypeMismatchMsg context "legacy-compatible base part" node
            | IllegalPart node ->
                let context = Some "legacy part conversion"
                AstResult.internalTypeMismatchMsg context "Part" node

module LegacyL2LineCreationError =
    let toAstMessage: LegacyL2LineCreationError -> AstMessage =
        function
        | IllegalIncomingL2ElementContent (left, right, node) ->
            let contextStr =
                sprintf "L2 element construction; found [%s>%s]" left.TypeName right.TypeName

            AstResult.internalTypeMismatchMsg (Some(contextStr)) "L2Id" node
        | IllegalIncomingL2Element node ->
            AstResult.internalTypeMismatchMsg (Some("L2 element construction")) "L2Id" node
        | FailedLocusUnpacking node -> AstResult.internalTypeMismatchMsg (Some("L2 locus unpacking")) "L2Id" node
