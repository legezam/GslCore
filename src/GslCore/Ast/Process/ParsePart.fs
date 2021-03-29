namespace GslCore.Ast.Process.ParsePart


open GslCore.Ast.Types
open GslCore.GslResult
open GslCore.Pragma

// ==========================
// Helper functions to ease working with pragmas and parts.
// ==========================

[<RequireQualifiedAccess>]
type GetPragmaError =
    | UnBuiltPragma of Node<ParsePragma> * AstNode
    | UnknownPragmaNode of AstNode

[<RequireQualifiedAccess>]
type MergePragmaError =
    | GetPragmaError of GetPragmaError
    | PragmaCollision of collidingPragmas: Set<string> * AstNode

module ParsePart =

    /// Get a part's list of built pragmas, represented as a PragmaCollection.
    /// This function will fail if it finds unbuilt pragmas.
    let getPragmasStrict (part: Node<ParsePart>): GslResult<PragmaCollection, GetPragmaError> =
        let getBuiltPragma (node: AstNode): GslResult<Pragma, GetPragmaError> =
            match node with
            | AstNode.Pragma pragmaWrapper -> GslResult.ok pragmaWrapper.Value
            | AstNode.ParsePragma parsePragmaWrapper ->
                GslResult.err (GetPragmaError.UnBuiltPragma(parsePragmaWrapper, node))
            | _x -> GslResult.err (GetPragmaError.UnknownPragmaNode node)

        part.Value.Pragmas
        |> List.map getBuiltPragma
        |> GslResult.collectA
        |> GslResult.map (fun pragmas ->
            PragmaCollection.empty
            |> PragmaCollection.mergeInPragmas pragmas)

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
            |> Seq.map (fun pragma -> AstNode.Pragma(Node.wrapNode pragma))
            |> List.ofSeq

        { part with
              Value = { part.Value with Pragmas = astPragmas } }

    /// Merge a pragma collection into a part, clobbering existing pragmas.
    /// Add a warning if there are any collisions.
    let mergePragmas (parsePart: Node<ParsePart>)
                     (pragmaCollection: PragmaCollection)
                     : GslResult<Node<ParsePart>, MergePragmaError> =
        getPragmasStrict parsePart
        |> GslResult.mapError MergePragmaError.GetPragmaError
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
                GslResult.ok newPart
            else
                let warning =
                    MergePragmaError.PragmaCollision(collidingPragmas, AstNode.Part(parsePart))


                GslResult.warn warning newPart)
