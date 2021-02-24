namespace GslCore.Ast.Process.RelativePositionTranslation


open GslCore.Ast.Types
open GslCore.Ast.Algorithms
open GslCore.Constants
open GslCore.GslResult

// ======================
// computing relative positions
// ======================

/// Represents an arithmetic issue during the relative position calculation
type CalculationMessage =
    /// Occurs when a negative left value appears in amino acid slicing syntax (foo[A-20:15])
    | NegativeLeftAminoAcidStartPosition of position: int<OneOffset>
    /// Occurs when a negative right value appears in amino acid slicing syntax (foo[A20:-15])
    | NegativeRightAminoAcidStartPosition of position: int<OneOffset>
    | PositionCannotBeZero

/// Represents an overall issue during the relative positon calculation
type RelativePositionTranslationMessage =
    /// Arithmetic error happened during the calculation
    | CalculationError of node: AstNode * err: CalculationMessage
    /// The provided position cannot be parsed as a valid integer value
    | PositionIsNotInteger of node: AstNode

/// Functions related to <see cref="T:GslCore.Ast.Process.RelativePositionTranslation.RelativePositionTranslationMessage">RelativePositionTranslationMessage</see>
module RelativePositionTranslationMessage =
    let makeCalculationError (node: AstNode) (err: CalculationMessage) = CalculationError(node, err)

module RelativePositionTranslation =
    let private buildNode (positions: SourcePosition list) (position: int<OneOffset>) (relativeTo: GeneEnd): AstNode =
        AstNode.RelPos
            { Node.Value =
                  { RelativePosition.Position = position
                    RelativeTo = relativeTo }
              Positions = positions }

    let private defaultRelPosQualifier = S

    let internal calculatePosition (position: int<OneOffset>)
                                   (maybeQualifier: RelPosQualifier option)
                                   (relPosition: RelPosPosition)
                                   : GslResult<int<OneOffset> * GeneEnd, CalculationMessage> =
        if position = 0<OneOffset> then
            GslResult.err PositionCannotBeZero
        else
            let qualifier =
                maybeQualifier
                |> Option.defaultValue defaultRelPosQualifier

            match qualifier, relPosition with
            | S, _ -> (position, FivePrime) |> GslResult.ok
            | E, _ -> (position, ThreePrime) |> GslResult.ok
            | A, Left
            | AS, Left
            | SA, Left ->
                if position > 0<OneOffset> then
                    (position * 3 - 2<OneOffset>, FivePrime)
                    |> GslResult.ok
                else
                    GslResult.err (NegativeLeftAminoAcidStartPosition position)
            | AE, Left
            | EA, Left ->
                let aminoAcidIndex =
                    if position > 0<OneOffset> then position * 3 - 2<OneOffset> else position * 3

                (aminoAcidIndex, ThreePrime) |> GslResult.ok
            | A, Right
            | AS, Right
            | SA, Right ->
                if position > 0<OneOffset> then
                    ((position * 3), FivePrime) |> GslResult.ok
                else
                    GslResult.err (NegativeRightAminoAcidStartPosition position)
            | AE, Right
            | EA, Right ->
                let aminoAcidIndex =
                    if position > 0<OneOffset> then position * 3 else position * 3 + 2<OneOffset>

                (aminoAcidIndex, ThreePrime) |> GslResult.ok

    let internal getProvidedPosition (relativePosition: ParseRelativePosition)
                                     : GslResult<int, RelativePositionTranslationMessage> =
        match relativePosition.Item with
        | AstNode.Int ({ Node.Value = providedPosition
                         Positions = _ }) -> GslResult.ok providedPosition
        | otherNode -> PositionIsNotInteger otherNode |> GslResult.err

    /// Compute relative positions for slices.
    /// Replaces `ParseRelativePosition` items with `RelativePosition` items
    let private buildRelativePosition (node: AstNode): GslResult<AstNode, RelativePositionTranslationMessage> =
        match node with
        | AstNode.ParseRelPos relativePositionWrapper ->
            let parseRelativePosition = relativePositionWrapper.Value

            let buildNode =
                buildNode relativePositionWrapper.Positions
            // make sure we have a real value to work with
            parseRelativePosition
            |> getProvidedPosition
            |> GslResult.map (fun position -> position * 1<OneOffset>)
            >>= fun position ->
                    calculatePosition position parseRelativePosition.Qualifier parseRelativePosition.Position
                    |> GslResult.mapError (RelativePositionTranslationMessage.makeCalculationError node)
                    |> GslResult.map (fun (position, geneEnd) -> buildNode position geneEnd)
        | _ -> GslResult.ok node

    let compute =
        FoldMap.map Serial TopDown buildRelativePosition
