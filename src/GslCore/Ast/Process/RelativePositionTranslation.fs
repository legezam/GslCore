module GslCore.Ast.Process.RelativePositionTranslation


open GslCore.Ast.Types
open GslCore.Ast.ErrorHandling
open GslCore.Ast.Algorithms
open GslCore.Constants

// ======================
// computing relative positions
// ======================


let private buildNode (positions: SourcePosition list) (position: int<OneOffset>) (relativeTo: GeneEnd): AstNode =
    RelPos
        { Node.Value =
              { RelativePosition.Position = position
                RelativeTo = relativeTo }
          Positions = positions }


let private defaultRelPosQualifier = S

type private TransformationError =
    | NegativeLeftAminoAcidStartPosition
    | NegativeRightAminoAcidStartPosition

module private TransformationError =
    let toAstMessage (node: AstNode) (position: int<OneOffset>) (error: TransformationError): AstMessage =
        match error with
        | NegativeLeftAminoAcidStartPosition ->
            AstMessage.createErrorWithStackTrace
                ValueError
                (sprintf "Cannot begin with a negative amino acid offset: %d" position)
                node

        | NegativeRightAminoAcidStartPosition ->
            AstMessage.createErrorWithStackTrace
                ValueError
                (sprintf "Cannot offset negative amino acids from start: %d" position)
                node



let private calculatePosition (position: int<OneOffset>)
                              (maybeQualifier: RelPosQualifier option)
                              (relPosition: RelPosPosition)
                              : Result<int<OneOffset> * GeneEnd, TransformationError> =


    let qualifier =
        maybeQualifier
        |> Option.defaultValue defaultRelPosQualifier

    match qualifier, relPosition with
    | S, _ -> (position, FivePrime) |> Ok
    | E, _ -> (position, ThreePrime) |> Ok
    | A, Left
    | AS, Left
    | SA, Left ->
        if position > 0<OneOffset>
        then (position * 3 - 2<OneOffset>, FivePrime) |> Ok
        else Result.Error NegativeLeftAminoAcidStartPosition
    | AE, Left
    | EA, Left ->
        let aminoAcidIndex =
            if position > 0<OneOffset> then position * 3 - 2<OneOffset> else position * 3

        (aminoAcidIndex, ThreePrime) |> Ok
    | A, Right
    | AS, Right
    | SA, Right ->
        if position > 0<OneOffset>
        then ((position * 3), FivePrime) |> Ok
        else Result.Error NegativeRightAminoAcidStartPosition
    | AE, Right
    | EA, Right ->
        let aminoAcidIndex =
            if position > 0<OneOffset> then position * 3 else position * 3 + 2<OneOffset>

        (aminoAcidIndex, ThreePrime) |> Ok

let private getProvidedPosition (relativePosition: ParseRelativePosition): AstResult<int> =
    match relativePosition.Item with
    | Int ({ Node.Value = providedPosition
             Positions = _ }) -> AstResult.ok providedPosition
    | x -> AstResult.internalTypeMismatch (Some "relative position building") "Int" x

/// Compute relative positions for slices.
/// Replaces `ParseRelativePosition` items with `RelativePosition` items
let private buildRelativePosition (node: AstNode): AstResult<AstNode> =
    match node with
    | ParseRelPos relativePositionWrapper ->
        let parseRelativePosition = relativePositionWrapper.Value

        let buildNode =
            buildNode relativePositionWrapper.Positions
        // make sure we have a real value to work with
        parseRelativePosition
        |> getProvidedPosition
        |> AstResult.map (fun position -> position * 1<OneOffset>)
        >>= fun position ->
                calculatePosition position parseRelativePosition.Qualifier parseRelativePosition.Position
                |> AstResult.ofResult (TransformationError.toAstMessage node position)
                |> AstResult.map (fun (position, geneEnd) -> buildNode position geneEnd)
    | _ -> AstResult.ok node

let compute =
    FoldMap.map Serial TopDown buildRelativePosition
