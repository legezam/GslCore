module GslCore.Ast.Process.RelativePosition


open Amyris.ErrorHandling
open GslCore.Ast.Types
open GslCore.Ast.ErrorHandling
open GslCore.Ast.Algorithms
open GslCore.Constants

// ======================
// computing relative positions
// ======================

/// Compute relative positions for slices.
let private buildRelativePosition (node: AstNode): Result<AstNode, AstMessage> =
    match node with
    | ParseRelPos relativePositionWrapper ->
        let parseRelativePosition = relativePositionWrapper.Value

        let buildNode (position: int<OneOffset>) (relativeTo: GeneEnd): Result<AstNode, AstMessage> =
            RelPos
                { Node.Value =
                      { RelPos.Position = position
                        RelativeTo = relativeTo }
                  Positions = relativePositionWrapper.Positions }
            |> ok

        // make sure we have a real value to work with
        match parseRelativePosition.Item with
        | Int ({ Node.Value = integerPosition
                 Positions = _ }) -> ok integerPosition
        | x -> AstMessage.internalTypeMismatch (Some "relative position building") "Int" x
        >>= (fun position ->
            match parseRelativePosition.Qualifier with
            | None -> buildNode (position * 1<OneOffset>) FivePrime
            | Some qualifier ->
                match qualifier, parseRelativePosition.Position with
                | S, _ -> buildNode (position * 1<OneOffset>) FivePrime
                | E, _ -> buildNode (position * 1<OneOffset>) ThreePrime
                | A, Left
                | AS, Left
                | SA, Left ->
                    if position > 0 then
                        buildNode (position * 3<OneOffset> - 2<OneOffset>) FivePrime
                    else
                        AstMessage.createErrorf
                            ValueError
                            "Cannot begin with a negative amino acid offset: %d"
                            position
                            parseRelativePosition.Item
                | AE, Left
                | EA, Left ->
                    let ai =
                        if position > 0 then (position * 3<OneOffset> - 2<OneOffset>) else (position * 3<OneOffset>)

                    buildNode ai ThreePrime
                | A, Right
                | AS, Right
                | SA, Right ->
                    if position > 0 then
                        buildNode (position * 3<OneOffset>) FivePrime
                    else
                        AstMessage.createErrorf
                            ValueError
                            "Cannot offset negative amino acids from start: %d"
                            position
                            parseRelativePosition.Item
                | AE, Right
                | EA, Right ->
                    let ai =
                        if position > 0 then (position * 3<OneOffset>) else (position * 3<OneOffset> + 2<OneOffset>)

                    buildNode ai ThreePrime)
    | _ -> ok node

let compute =
    FoldMap.map Serial TopDown buildRelativePosition
