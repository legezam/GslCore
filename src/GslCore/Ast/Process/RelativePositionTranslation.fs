module GslCore.Ast.Process.RelativePositionTranslation


open Amyris.ErrorHandling
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
    | S, _ -> (position, FivePrime) |> ok
    | E, _ -> (position, ThreePrime) |> ok
    | A, Left
    | AS, Left
    | SA, Left ->
        if position > 0<OneOffset>
        then (position * 3 - 2<OneOffset>, FivePrime) |> ok
        else fail NegativeLeftAminoAcidStartPosition
    | AE, Left
    | EA, Left ->
        let aminoAcidIndex =
            if position > 0<OneOffset> then position * 3 - 2<OneOffset> else position * 3

        (aminoAcidIndex, ThreePrime) |> ok
    | A, Right
    | AS, Right
    | SA, Right ->
        if position > 0<OneOffset>
        then ((position * 3), FivePrime) |> ok
        else fail NegativeRightAminoAcidStartPosition
    | AE, Right
    | EA, Right ->
        let aminoAcidIndex =
            if position > 0<OneOffset> then position * 3 else position * 3 + 2<OneOffset>

        (aminoAcidIndex, ThreePrime) |> ok

let private getProvidedPosition (relativePosition: ParseRelativePosition): Result<int, AstMessage> =
    match relativePosition.Item with
    | Int ({ Node.Value = providedPosition
             Positions = _ }) -> ok providedPosition
    | x -> AstMessage.internalTypeMismatch (Some "relative position building") "Int" x

/// Compute relative positions for slices.
/// Replaces `ParseRelativePosition` items with `RelativePosition` items
let private buildRelativePosition (node: AstNode): Result<AstNode, AstMessage> =
    match node with
    | ParseRelPos relativePositionWrapper ->
        let parseRelativePosition = relativePositionWrapper.Value

        let buildNode =
            buildNode relativePositionWrapper.Positions
        // make sure we have a real value to work with
        parseRelativePosition
        |> getProvidedPosition
        |> Trial.lift (fun position -> position * 1<OneOffset>)
        >>= fun position ->
                calculatePosition position parseRelativePosition.Qualifier parseRelativePosition.Position
                |> Trial.mapFailure (List.map (TransformationError.toAstMessage node position))
                |> Trial.lift (fun (position, geneEnd) -> buildNode position geneEnd)
    | _ -> ok node

let compute =
    FoldMap.map Serial TopDown buildRelativePosition
