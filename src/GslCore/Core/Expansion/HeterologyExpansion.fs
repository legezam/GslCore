module GslCore.Core.Expansion.HeterologyExpansion

open System
open GslCore.Ast.Types
open GslCore.Constants
open GslCore.Ast.ErrorHandling
open GslCore.Ast.Algorithms
open Amyris.ErrorHandling
open GslCore.Ast.LegacyParseTypes
open Amyris.Dna
open GslCore.Core.DnaCreation
open GslCore.Core.Expansion
open GslCore.Pragma
open GslCore.Core.AlleleSwaps
open Amyris.Bio.utils
open GslCore.Core
open GslCore.Core.Types
open GslCore.Reference
open GslCore.Core.ResolveExtPart
open GslCore.Core.PluginTypes


// ==========================
// expanding heterology blocks
// ==========================


/// Expand heterology blocks.
let private expandHB (parameters: Phase2Parameters) (assemblyIn: Assembly) =

    let modIsNotSlice m =
        match m with
        | SLICE _ -> false
        | _ -> true

    let getLenPragma (pr: PragmaCollection) =
        match pr
              |> PragmaCollection.tryGetValue BuiltIn.lenPragmaDef with
        | None -> None
        | Some (v) ->
            match Int32.TryParse(v) with
            | true, i -> Some(i)
            | _ -> failwithf "Expected integer in #len pragma, not '%s'" v

    let capUsing (a: Dna) (b: Dna) =
        Seq.zip a b
        |> Seq.map (fun (a, b) ->
            if a = b then
                b
            else
                match b with
                | 'G' -> 'g'
                | 'C' -> 'c'
                | 'A' -> 'a'
                | 'T' -> 't'
                | x -> x)
        |> Dna

    let rec scan (a: Assembly) (res: (Part * PragmaCollection * bool) list) (p: (Part * PragmaCollection * bool) list) =
        // Need to select a codon usage table
        let referenceGenome =
            GenomeDefinitions.chooseReferenceGenome a.pragmas

        let references =
            parameters.References |> GenomeDefinitions.get

        let reference = references.[referenceGenome]
        let references = parameters.References
        let verbose = parameters.Verbose

        let codonUsage =
            parameters.CodonTableCache.GetCodonLookupTable(reference)

        match p with
        // Lone heterology block with no inlined sequence adjacent
        | (GENEPART (gpUp), pr1, fwd1) :: (HETBLOCK, pr2, fwd2) :: (GENEPART (gp), pr3, fwd3) :: tl ->
            let rg' = getRG a references pr3
            let rg'' = getRG a references pr1

            let sliceSeq =
                realizeSequence verbose a.pragmas fwd3 rg' gp // Get DNA sequence for this slice

            let sliceSeqUp =
                realizeSequence verbose a.pragmas fwd1 rg'' gpUp // Get DNA sequence for upstream slice

            let targetAALen = getLenPragma pr2 // Get optional length spec for the HB

            // Generate an alternative prefix for the GENEPART on RHS
            let alt =
                generateRightHB
                    codonUsage
                    Default.MinHBCodonUsage
                    targetAALen
                    a.designParams
                    sliceSeqUp
                    (Dna(""))
                    sliceSeq
            // tricky part - need to slightly adjust the slice range of gp,
            // but that's embedded down in the mod list

            // Assume they must be using a gene part next to a het block.  Bad?
            if (not (gp.part.gene.StartsWith("g")))
            then failwithf "Heterology block must be adjacent to g part, %s not allowed" gp.part.gene

            let s =
                translateGenePrefix a.pragmas rg' StandardSlice.Gene // Start with standard slice

            let startSlice =
                ApplySlices.applySlices verbose gp.part.mods s // Apply modifiers

            let newSlice =
                { startSlice with
                      left =
                          { startSlice.left with
                                Position =
                                    startSlice.left.Position
                                    + (alt.Length * 1<OneOffset>) } }

            assert (alt <> sliceSeq.[0..alt.Length - 1])
            // Assemble new mod list by getting rid of existing slice mods and
            // putting in new consolidated slice.
            let newMods =
                SLICE(newSlice)
                :: (gp.part.mods |> List.filter modIsNotSlice)

            // Prepend backwards as we will flip list at end - watch out, pragmas are reversed as well
            scan
                a
                ((GENEPART
                    ({ gp with
                           part = { gp.part with mods = newMods } }),
                  pr3,
                  fwd3)
                 :: (INLINEDNA(alt),
                     (pr2
                      |> PragmaCollection.add
                          { Pragma.Definition = BuiltIn.inlinePragmaDef
                            Arguments = [] }),
                     fwd2)
                    :: (GENEPART(gpUp), pr1, fwd1) :: res)
                tl

        | (GENEPART (gpUp), pr1, fwd1) :: (INLINEDNA (i), pr2, fwd2) :: (HETBLOCK, pr3, _ (*fwd3*) ) :: (GENEPART (gp),
                                                                                                         pr4,
                                                                                                         fwd4) :: tl ->
            let rg' = getRG a references pr4
            let rg'' = getRG a references pr1

            // Get DNA sequence for this slice
            let sliceSeq =
                realizeSequence verbose a.pragmas fwd4 rg' gp
            // Get DNA sequence for upstream slice
            let sliceSeqUp =
                realizeSequence verbose a.pragmas fwd1 rg'' gpUp
            // Get optional length spec for the HB
            let targetAALen = getLenPragma pr3
            // Generate an alternative prefix for the GENEPART on RHS
            let alt =
                generateRightHB codonUsage Default.MinHBCodonUsage targetAALen a.designParams sliceSeqUp i sliceSeq

            assert (alt <> sliceSeq.[0..alt.Length - 1])

            if verbose
            then printf "// %O -> %O\n" alt sliceSeq.[0..alt.Length - 1]

            // tricky part - need to slightly adjust the slice range of gp,
            // but that's embedded down in the mod list

            // Assume they must be using a gene part next to a het block.  Bad?
            if not (gp.part.gene.[0] = 'g') then
                failwithf
                    "Slices adjacent to het block elements ~ must be gene slices - %s has '%c' gene part"
                    gp.part.gene
                    gp.part.gene.[0]

            let s =
                translateGenePrefix a.pragmas rg'' StandardSlice.Gene // Start with standard slice

            let startSlice =
                ApplySlices.applySlices verbose gp.part.mods s // Apply modifiers

            let newSlice =
                { startSlice with
                      left =
                          { startSlice.left with
                                Position =
                                    startSlice.left.Position
                                    + (alt.Length * 1<OneOffset>) } }

            let newInline = DnaOps.append i alt

            // Assemble new mod list by getting rid of existing slice mods and putting in new consolidated slice.
            let newMods =
                SLICE(newSlice)
                :: (gp.part.mods |> List.filter modIsNotSlice)
            // Prepend backwards as we will flip list at end
            // Note - currently destroy any pragmas attached to the heterology block itself
            scan
                a
                ((GENEPART
                    ({ gp with
                           part = { gp.part with mods = newMods } }),
                  pr4,
                  fwd4)
                 :: (INLINEDNA(newInline),
                     (pr2
                      |> PragmaCollection.add
                          { Pragma.Definition = BuiltIn.inlinePragmaDef
                            Arguments = [] }),
                     fwd2)
                    :: (GENEPART(gpUp), pr1, fwd1) :: res)
                tl

        | (GENEPART (gp), pr1, fwd1) :: (HETBLOCK, pr2, _ (*fwd2*) ) :: (INLINEDNA (ic), pr3, fwd3) :: (GENEPART (gpDown),
                                                                                                        pr4,
                                                                                                        fwd4) :: tl ->

            // get reference genomes warmed up
            let rg' = getRG a references pr1
            let rg'' = getRG a references pr4

            // get actual sequence for slices (with mods applied)
            let sliceSeq =
                realizeSequence verbose a.pragmas fwd1 rg' gp // Get DNA sequence for this slice

            let sliceSeqDown =
                realizeSequence verbose a.pragmas fwd4 rg'' gpDown // Get DNA sequence for this slice

            let targetAALen = getLenPragma pr2 // Get optional length spec for the HB

            // generate hetblock section off upstream slice
            // Generate an alternative prefix for the GENEPART on LHS
            let alt =
                generateLeftHB codonUsage Default.MinHBCodonUsage targetAALen a.designParams sliceSeq ic sliceSeqDown
            // tricky part - need to slightly adjust the slice range of gp,
            // but that's embedded down in the mod list

            // Assume they must be using a gene part next to a het block.  Bad?
            assert (gp.part.gene.[0] = 'g')

            // now build up the slice again and apply from scratch to the gene
            let s =
                translateGenePrefix a.pragmas rg' StandardSlice.Gene // Start with standard slice

            let startSlice =
                ApplySlices.applySlices verbose gp.part.mods s // Apply modifiers

            // modify slice to take into account the bit we chopped off
            let newSlice =
                { startSlice with
                      right =
                          { startSlice.right with
                                Position =
                                    startSlice.right.Position
                                    - (alt.Length * 1<OneOffset>) } }

            let newInline = DnaOps.append alt ic
            assert (alt <> sliceSeq.[sliceSeq.Length - alt.Length..])

            if verbose then
                //let fr = alt
                let t =
                    sliceSeq.[sliceSeq.Length - alt.Length..sliceSeq.Length - 1]

                let sim =
                    Array.map2 (fun a b -> if a = b then '.' else ' ') alt.arr t.arr

                let simProp =
                    (seq { for x in sim -> if x = '.' then 1.0 else 0.0 }
                     |> Seq.sum)
                    / float (alt.Length)
                    * 100.0

                printf "// From: %O \n// To  : %O\n//     : %s %3.0f%%\n" alt (t |> capUsing alt) (arr2seq sim) simProp
            // Assemble new mod list by getting rid of existing slice mods and
            // putting in new consolidated slice.
            let newMods =
                SLICE(newSlice)
                :: (gp.part.mods |> List.filter modIsNotSlice)
            // Prepend backwards as we will flip list at end
            // Note - currently destroy pr2 pragmas associated with the hetblock
            scan
                a
                ((GENEPART(gpDown), pr4, fwd4)
                 :: (INLINEDNA(newInline),

                     (pr3
                      |> PragmaCollection.add
                          { Pragma.Definition = BuiltIn.inlinePragmaDef
                            Arguments = [] }),
                     fwd3)
                    :: (GENEPART
                            ({ gp with
                                   part = { gp.part with mods = newMods } }),
                        pr1,
                        fwd1)
                       :: res)
                tl

        | (PARTID (pid1), pr1, fwd1) :: (HETBLOCK, pr2, _ (*fwd2*) ) :: (INLINEDNA (ic), pr3, fwd3) :: (PARTID (pid4),
                                                                                                        pr4,
                                                                                                        fwd4) :: tl ->
            // External part variation
            // ===============================================

            let part1 =
                ResolveExtPart.fetchFullPartSequence (verbose) (Map.empty) pid1
                |> Trial.mapFailure (fun messages -> sprintf "Fail fetching %s" pid1.id :: messages)
                |> returnOrFail

            let part4 =
                ResolveExtPart.fetchFullPartSequence verbose Map.empty pid4
                |> Trial.mapFailure (fun messages -> sprintf "Fail fetching %s" pid4.id :: messages)
                |> returnOrFail


            let s1 =
                ResolveExtPart.getExtPartSlice verbose pid1

            let s4 =
                ResolveExtPart.getExtPartSlice verbose pid4

            // Build up upstream and downstream DNA slice
            let sliceSeq1 =
                ResolveExtPart.applySliceToExtSequence verbose part1 pr1 fwd1 pid1 s1

            let sliceSeq4 =
                ResolveExtPart.applySliceToExtSequence verbose part4 pr4 fwd4 pid4 s4

            let targetAALen = getLenPragma pr2 // Get optional length spec for the HB

            // generate hetblock sequence by cutting into upstream sequence
            // Generate an alternative prefix for the GENEPART on LHS
            let alt =
                generateLeftHB
                    codonUsage
                    Default.MinHBCodonUsage
                    targetAALen
                    a.designParams
                    sliceSeq1.Dna
                    ic
                    sliceSeq4.Dna

            // Build up the slice mods for the upstream part from scratch
            // Modify upstream slice to account for the part we chopped off
            let newSlice: Slice =
                { s1 with
                      right =
                          { s1.right with
                                Position = s1.right.Position - (alt.Length * 1<OneOffset>) } }

            let newInline = DnaOps.append alt ic

            assert (alt
                    <> sliceSeq1.Dna.[sliceSeq1.Dna.Length - alt.Length..])

            if verbose then
                let t =
                    sliceSeq1.Dna.[sliceSeq1.Dna.Length - alt.Length..]

                let sim =
                    Array.map2 (fun a b -> if a = b then '.' else ' ') alt.arr t.arr

                let simProp =
                    (seq { for x in sim -> if x = '.' then 1.0 else 0.0 }
                     |> Seq.sum)
                    / float (alt.Length)
                    * 100.0

                printf "// From: %O \n// To  : %O\n//     : %s %3.0f%%\n" alt (t |> capUsing alt) (arr2seq sim) simProp
            // Assemble new mod list by getting rid of existing slice mods
            // and putting in new consolidated slice.
            let newMods =
                SLICE(newSlice)
                :: (pid1.mods |> List.filter modIsNotSlice)
            // Prepend backwards as we will flip list at end
            // Note - currently destroy pr2 pragmas associated with the hetblock
            scan
                a
                ((PARTID(pid4), pr4, fwd4)
                 :: (INLINEDNA(newInline),
                     (pr3
                      |> PragmaCollection.add
                          { Pragma.Definition = BuiltIn.inlinePragmaDef
                            Arguments = [] }),
                     fwd3)
                    :: (PARTID({ pid1 with mods = newMods }), pr1, fwd1)
                       :: res)
                tl
        | hd :: tl -> scan a (hd :: res) tl // do nothing
        | [] -> List.rev res
    /// Easier to process part/pragma pairs if they are explicit tuples
    let tuplePPP (ppp: PPP list) =
        ppp |> List.map (fun p -> p.part, p.pr, p.fwd)

    let untuplePPP ab =
        ab
        |> List.map (fun (a, b, c) -> { part = a; pr = b; fwd = c })

    let newParts =
        scan assemblyIn [] (tuplePPP assemblyIn.parts)
        |> untuplePPP

    let res = { assemblyIn with parts = newParts }

    let remainingHetblocks =
        res.parts
        |> List.exists (fun ppp ->
            match ppp.part with
            | HETBLOCK -> true
            | _ -> false)

    let newSource = prettyPrintAssembly res

    if remainingHetblocks
    then failwithf "Attempt to expand heterology block in design %s left remaining hetblock" newSource.String
    else newSource

/// Expand all heterology blocks in an AST.
let expandHetBlocks (parameters: Phase2Parameters) (tree: AstTreeHead): Result<AstTreeHead, AstMessage> =

    let assemblyExpansion = expandHB parameters

    let bootstrapOperation =
        let phase1Params =
            parameters |> Phase1Parameters.fromPhase2        
        Bootstrapping.bootstrapExpandLegacyAssembly
            HetBlockError
            assemblyExpansion
            (Bootstrapping.bootstrapPhase1 phase1Params)

    let expansionOnlyOnNodesWithHetBlocks =
        BoostrapSelection.maybeBypassBootstrap ExpandHetBlock bootstrapOperation

    Bootstrapping.executeBootstrap expansionOnlyOnNodesWithHetBlocks Serial tree
