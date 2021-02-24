namespace GslCore.Ast.Legacy

open Amyris.Dna
open GslCore.Ast.Algorithms
open GslCore.Ast.Process
open GslCore.Constants
open System
open GslCore.GslResult
open GslCore.Pragma
open GslCore.Ast.Types
open GslCore.Ast.ErrorHandling
open GslCore.Ast.Process.AssemblyStuffing
open GslCore.DesignParams
open GslCore.Ast.Legacy.Types

module LegacySliceContext =
    /// Return a tuple of OneOffset left/right slice bounds from a slice record.
    /// These bounds are both relative to the FivePrime end.
    /// Requires the length of the feature being sliced to be interpreted correctly.
    let getBoundsFromSlice (slice: Slice)
                           (featureLength: int)
                           (context: SliceContext)
                           : Result<int<OneOffset> * int<OneOffset>, string> =
        let left =
            match slice.Left.RelativeTo with
            | FivePrime -> slice.Left.Position
            | ThreePrime ->
                (featureLength + 1) * 1<OneOffset>
                + slice.Left.Position

        let right =
            match slice.Right.RelativeTo with
            | FivePrime -> slice.Right.Position
            | ThreePrime ->
                (featureLength + 1) * 1<OneOffset>
                + slice.Right.Position

        match context with
        | Genomic ->
            // no validation necessary
            Ok(left, right)
        | Library partId ->
            // the slice bounds are not allowed to fall outside the feature as we don't have
            // data on flanking regions in this context
            if left < 1<OneOffset>
               || right <= left
               || right > (featureLength * 1<OneOffset>) then
                Result.Error(sprintf "Illegal slice (%A) outside core gene range for library item %s." slice partId)
            else
                Ok(left, right)


// ========================
// pretty-printing legacy assemblies as GSL source code
// ========================
module LegacyPrettyPrint =
    /// Pretty print a RelPos
    let relativePosition (position: RelativePosition): string =
        sprintf
            "%A/%s"
            position.Position
            (match position.RelativeTo with
             | FivePrime -> "S"
             | ThreePrime -> "E")

    let slice (slice: Slice): string =
        sprintf
            "[%s%A%s:%s%A%s]"
            (if slice.LeftApprox then "~" else "")
            slice.Left.Position
            (match slice.Left.RelativeTo with
             | FivePrime -> "S"
             | ThreePrime -> "E")
            (if slice.RightApprox then "~" else "")
            slice.Right.Position
            (match slice.Right.RelativeTo with
             | FivePrime -> "S"
             | ThreePrime -> "E")

    let expandModifiers (modifiers: Modifier list): string =
        seq {
            for modifier in modifiers do
                match modifier with
                | Modifier.Mutation mutation ->
                    yield
                        sprintf
                            "%c%c%d%c"
                            (match mutation.Type with
                             | AA -> '$'
                             | NT -> '*')
                            mutation.From
                            mutation.Location
                            mutation.To
                | Modifier.Slice slicee -> yield slice slicee
                | Modifier.Dot dot -> yield sprintf ".%s" dot
        }
        |> fun x -> String.Join("", x)

    let rec partPlusPragma (partPlusPragma: PartPlusPragma): string =
        let partOut =
            match partPlusPragma.Part with
            | Part.HeterologyBlock -> "~ " // don't do anything at this level
            | Part.InlineDna (s) -> sprintf "/%O/ " s // inline DNA sequence
            | Part.InlineProtein (s) -> sprintf "/$%s/ " s // inline protein sequence
            | Part.MarkerPart -> "### "
            | Part.PartId (p) -> sprintf "@%s" p.Id + (expandModifiers p.Modifiers)
            | Part.SourceCode (s) -> s.String // Part that was already expanded into a string
            | Part.GenePart (gp) ->
                let lOut =
                    match gp.Linker with
                    | None -> ""
                    | Some (l) -> sprintf "%s-%s-%s-" l.Linker1 l.Linker2 l.Orient // Emit linker

                let p = gp.Part

                let gOut = p.Gene
                let modOut = expandModifiers p.Modifiers

                lOut + gOut + modOut // String.Join("",Array.ofSeq modOut)
        // Now add in any inline pragma part with braces, ; separated etc
        let prOut =
            if partPlusPragma.Pragma.Pragmas.Count = 0 then
                ""
            else
                partPlusPragma.Pragma.Pragmas
                |> Seq.map (fun pv -> sprintf "#%s %s" pv.Key (pv.Value.Arguments |> String.concat " "))
                |> fun ss -> String.Join(";", ss)
                |> sprintf "{%s}"

        (if partPlusPragma.IsForward then "" else "!")
        + partOut
        + prOut

    /// Pretty print a built GSL assembly
    let assembly (assembly: Assembly): GslSourceCode =
        [ for ppp in assembly.Parts -> partPlusPragma ppp ]
        |> String.concat ";"
        |> GslSourceCode

// =====================
// conversion from AST parts to legacy parts
// =====================
module LegacyConversion =
    let private sliceFromAstSlice (parseSlice: ParseSlice): AstResult<Modifier> =
        match parseSlice.Left, parseSlice.Right with
        | RelPos (lw), RelPos (rw) ->
            GslResult.ok
                (Modifier.Slice
                    ({ Left = lw.Value
                       LeftApprox = parseSlice.LeftApprox
                       Right = rw.Value
                       RightApprox = parseSlice.RightApprox }))
        | x, y ->
            let contextStr =
                sprintf "legacy slice construction; found [%s:%s]" x.TypeName y.TypeName

            AstResult.internalTypeMismatch (Some(contextStr)) "RelPos" x

    let private astNodeToLegacyMod (node: AstNode): AstResult<Modifier> =
        match node with
        | Slice sw -> sliceFromAstSlice sw.Value
        | Mutation mw -> GslResult.ok (Modifier.Mutation(mw.Value))
        | DotMod dm -> GslResult.ok (Modifier.Dot(dm.Value))
        | _ -> AstResult.internalTypeMismatch (Some "legacy mod conversion") "Slice or Mutation or DotMod" node

    let private convertModifiers (mods: AstNode list): AstResult<Modifier list> =
        mods
        |> List.map astNodeToLegacyMod
        |> GslResult.collectA

    /// Convert an AST base part into a legacy Part.
    let private createLegacyPart (part: Node<ParsePart>): AstResult<Part> =
        match part.Value.BasePart with
        | Gene geneWrapper ->
            convertModifiers part.Value.Modifiers
            |> GslResult.map (fun mods ->
                let genePart =
                    { Gene = geneWrapper.Value.Gene
                      Modifiers = mods
                      Where = geneWrapper.Positions }

                Part.GenePart
                    { Part = genePart
                      Linker = geneWrapper.Value.Linker })
        | Marker _ -> GslResult.ok Part.MarkerPart
        | InlineDna dnaSequence -> GslResult.ok (Part.InlineDna(Dna(dnaSequence.Value, true, AllowAmbiguousBases)))
        | InlineProtein proteinSequence -> GslResult.ok (Part.InlineProtein proteinSequence.Value)
        | HetBlock _ -> GslResult.ok Part.HeterologyBlock
        | PartId partId ->
            convertModifiers part.Value.Modifiers
            |> GslResult.map (fun mods -> Part.PartId { Id = partId.Value; Modifiers = mods })
        | x -> AstResult.internalTypeMismatch (Some "legacy part conversion") "legacy-compatible base part" x

    let private createPartPlusPragma (node: AstNode): AstResult<PartPlusPragma> =
        match node with
        | Part part ->
            createLegacyPart part
            |> GslResult.map (fun legacyPart ->
                { PartPlusPragma.Part = legacyPart
                  Pragma = ParsePart.getPragmas part
                  IsForward = part.Value.IsForward })
        | x -> AstResult.internalTypeMismatch (Some "legacy part conversion") "Part" x


    /// Accumulate both pragma and docstring context.
    let updateConversionContext (mode: StateUpdateMode)
                                (state: AssemblyConversionContext)
                                (node: AstNode)
                                : AssemblyConversionContext =
        let newPragmaEnv =
            AssemblyStuffing.updatePragmaEnvironment mode state.PragmaEnvironment node

        let newDocsEnv =
            Docstrings.updateDocstringEnvironment mode state.Docs node

        { state with
              PragmaEnvironment = newPragmaEnv
              Docs = newDocsEnv }

    /// Convert an AST assembly into a legacy assembly.
    let convertAssembly (context: AssemblyConversionContext)
                        (partWrapper: Node<ParsePart>, assemblyPartsWrapper: Node<AstNode list>)
                        : AstResult<Assembly> =
        let assemblyPragmas = ParsePart.getPragmas partWrapper

        let name =
            assemblyPragmas
            |> PragmaCollection.tryGetValue BuiltIn.namePragmaDef

        let uri =
            assemblyPragmas
            |> PragmaCollection.tryGetValue BuiltIn.uriPragmaDef

        let linkerHint =
            assemblyPragmas
            |> PragmaCollection.tryGetValues BuiltIn.linkersPragmaDef
            |> Option.map (String.concat "")
            |> Option.defaultValue ""


        let parameters =
            DesignParams.fromPragmas DesignParams.identity assemblyPragmas
            |> GslResult.mapError (fun message ->
                AstMessage.createErrorWithStackTrace PragmaError message (Part(partWrapper)))

        let parts =
            assemblyPartsWrapper.Value
            |> List.map createPartPlusPragma
            |> GslResult.collectA

        (parts, parameters)
        ||> GslResult.map2 (fun parts designParams ->
                { Assembly.Parts = parts
                  Name = name
                  Uri = uri
                  LinkerHint = linkerHint
                  Pragmas = assemblyPragmas
                  DesignParams = designParams
                  Capabilities = context.PragmaEnvironment.Capabilities
                  DocStrings = context.Docs.Assigned
                  SourcePosition = partWrapper.Positions })

// ======================
// conversion from L2 AST node to legacy L2 line type
// ======================
module LegacyL2Conversion =
    /// Build a concrete L2 element from an AST node.
    let private buildL2Element (node: AstNode): AstResult<BuiltL2Element> =
        match node with
        | L2Element (nw) ->
            match nw.Value.Promoter, nw.Value.Target with
            | L2Id _, L2Id (tw)
            | Part _, L2Id (tw) ->
                GslResult.ok
                    { Promoter = nw.Value.Promoter
                      Target = tw.Value }

            | x, y ->
                let contextStr =
                    sprintf "L2 element construction; found [%s>%s]" x.TypeName y.TypeName

                AstResult.internalTypeMismatch (Some(contextStr)) "L2Id" node
        | x -> AstResult.internalTypeMismatch (Some("L2 element construction")) "L2Id" x

    let private unpackLocus (maybeNode: AstNode option): AstResult<L2Id option> =
        match maybeNode with
        | Some (L2Id (lw)) -> GslResult.ok (Some(lw.Value))
        | Some (x) -> AstResult.internalTypeMismatch (Some("L2 locus unpacking")) "L2Id" x
        | None -> GslResult.ok None

    /// Build a concrete L2 expression from an AST node.
    let private buildL2Expression (expressionWrapper: Node<L2Expression>): AstResult<BuiltL2Expression> =
        let parts =
            expressionWrapper.Value.Parts
            |> List.map buildL2Element
            |> GslResult.collectA

        let locus =
            unpackLocus expressionWrapper.Value.Locus

        (locus, parts)
        ||> GslResult.map2 (fun locus parts -> { L2Locus = locus; Parts = parts })

    /// Build a L2Line from an AST node and pragma environment.
    let convertL2Line (pragmaEnv: PragmaEnvironment) (l2Expression: Node<L2Expression>): AstResult<L2Line> =
        let pragmas =
            pragmaEnv.Persistent
            |> PragmaCollection.mergeInCollection pragmaEnv.AssignedTransients

        let name =
            pragmas
            |> PragmaCollection.tryGetValue BuiltIn.namePragmaDef

        let uri =
            pragmas
            |> PragmaCollection.tryGetValue BuiltIn.uriPragmaDef

        buildL2Expression l2Expression
        |> GslResult.map (fun l2Design ->
            { L2Line.L2Design = l2Design
              Name = name
              Uri = uri
              Pragmas = pragmas
              Capabilities = pragmaEnv.Capabilities })
