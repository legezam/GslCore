module GslCore.Ast.Process.ParsePart

open Amyris.ErrorHandling
open GslCore.AstTypes
open GslCore.AstErrorHandling
open GslCore.Pragma

// ==========================
// Helper functions to ease working with pragmas and parts.
// ==========================

/// Get a part's list of built pragmas, represented as a PragmaCollection.
/// This function will fail if it finds unbuilt pragmas.
let getPragmasStrict (part: Node<ParsePart>): Result<PragmaCollection, AstMessage> =
    let getBuiltPragma (node: AstNode) =
        match node with
        | Pragma pragmaWrapper -> ok (pragmaWrapper.Value)
        | ParsePragma parsePragmaWrapper -> AstMessage.unbuiltPragmaError None parsePragmaWrapper.Value.Name node
        | x -> AstMessage.internalTypeMismatch None "Pragma" x

    part.Value.Pragmas
    |> List.map getBuiltPragma
    |> collect
    >>= (fun pragmas ->
        PragmaCollection.empty
        |> PragmaCollection.mergeInPragmas pragmas
        |> ok)

/// Get a part's list of built pragmas, represented as a PragmaCollection.
/// Raises an exception if pragmas are not built.
let getPragmas (part: Node<ParsePart>): PragmaCollection =
    match getPragmasStrict part with
    | Ok (pc, _) -> pc
    | Bad (errs) ->
        failwith
            (errs
             |> List.map (fun m -> m.Summary)
             |> String.concat "\n")

///<summary>
/// Replace a part's pragmas with a converted version of a pragma collection.
/// Note that this conversion produces Ast nodes without source code positions.
/// We may want to eventually refactor pragmas to be able to remember this information,
/// or force pragma collections to use NodeWrappers instead.  We may never need this information,
/// though, so wait and see.
///</summary>
let replacePragmas (part: Node<ParsePart>) (pragmaCollection: PragmaCollection): Node<ParsePart> =
    let astPragmas =
        pragmaCollection
        |> PragmaCollection.values
        |> Seq.map (fun pragma -> Pragma(Node.wrapNode pragma))
        |> List.ofSeq

    { part with
          Value = { part.Value with Pragmas = astPragmas } }

/// Merge a pragma collection into a part, clobbering existing pragmas.
/// Add a warning if there are any collisions.
let mergePragmas (parsePart: Node<ParsePart>) (pragmaCollection: PragmaCollection): Result<Node<ParsePart>, AstMessage> =
    getPragmasStrict parsePart
    >>= (fun partPragmas ->
        let namesFromPragmaCollection =
            pragmaCollection |> PragmaCollection.names

        let namesFromParseParts = partPragmas |> PragmaCollection.names

        let collidingPragmas =
            Set.intersect namesFromPragmaCollection namesFromParseParts

        let newPart =
            let mergedPragmas =
                partPragmas
                |> PragmaCollection.mergeInCollection pragmaCollection

            replacePragmas parsePart mergedPragmas

        if collidingPragmas |> Set.isEmpty then
            ok newPart
        else
            let warning =
                AstMessage.createWarning
                    (sprintf "Pragma collision(s): %s" (collidingPragmas |> String.concat ", "))
                    (Part(parsePart))

            warn warning newPart)