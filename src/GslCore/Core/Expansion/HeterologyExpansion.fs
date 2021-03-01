namespace GslCore.Core.Expansion.HeterologyExpansion

open System
open FsToolkit.ErrorHandling
open GslCore.Ast.Phase1
open GslCore.Ast.Types
open GslCore.Constants
open GslCore.Ast.Algorithms
open GslCore.Core.Expansion.Bootstrapping
open GslCore.GslResult
open GslCore.Legacy.Types
open GslCore.Legacy
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


// ==========================
// expanding heterology blocks
// ==========================

[<RequireQualifiedAccess>]
type HeterologyExpansionError =
    | NonGeneNeighbourPart of neighbourGene: string
    | ExternalPartResolutionFailure of partId: LegacyPartId * externalError: string
    | HeterologyBlockLeftInDesign of GslSourceCode
    | InvalidLenPragma of value: string

module HeterologyExpansion =
    let private modIsNotSlice: Modifier -> bool =
        function
        | Modifier.Slice _ -> false
        | _ -> true

    let private getLenPragma (pr: PragmaCollection): GslResult<int option, HeterologyExpansionError> =
        match pr
              |> PragmaCollection.tryGetValue BuiltIn.lenPragmaDef with
        | None -> GslResult.ok None
        | Some v ->
            match Int32.TryParse v with
            | true, i -> GslResult.ok (Some(i))
            | _ -> GslResult.err (HeterologyExpansionError.InvalidLenPragma v)

    let private capUsing (a: Dna) (b: Dna): Dna =
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

    let rec private scan (parameters: Phase2Parameters)
                         (assembly: Assembly)
                         (res: PartPlusPragma list)
                         (input: PartPlusPragma list)
                         : GslResult<PartPlusPragma list, HeterologyExpansionError> =
        // Need to select a codon usage table
        let referenceGenome =
            GenomeDefinitions.chooseReferenceGenome assembly.Pragmas

        let references =
            parameters.References |> GenomeDefinitions.get

        let reference = references.[referenceGenome]
        let references = parameters.References
        let verbose = parameters.Verbose

        let codonUsage =
            parameters.CodonTableCache.GetCodonLookupTable(reference)

        match input with
        // Lone heterology block with no inlined sequence adjacent
        | { Part = Part.GenePart gpUp
            Pragma = pr1
            IsForward = fwd1 } :: { Part = Part.HeterologyBlock
                                    Pragma = pr2
                                    IsForward = fwd2 } :: { Part = Part.GenePart gp
                                                            Pragma = pr3
                                                            IsForward = fwd3 } :: tl ->
            let rg' =
                getReferenceGenome assembly references pr3

            let rg'' =
                getReferenceGenome assembly references pr1

            let sliceSeq =
                realizeSequence verbose assembly.Pragmas fwd3 rg' gp // Get DNA sequence for this slice

            let sliceSeqUp =
                realizeSequence verbose assembly.Pragmas fwd1 rg'' gpUp // Get DNA sequence for upstream slice

            getLenPragma pr2 // Get optional length spec for the HB
            >>= fun targetAALen ->
                    // Generate an alternative prefix for the GENEPART on RHS
                    let alt =
                        generateRightHB
                            codonUsage
                            Default.MinHBCodonUsage
                            targetAALen
                            assembly.DesignParams
                            sliceSeqUp
                            (Dna(""))
                            sliceSeq
                    // tricky part - need to slightly adjust the slice range of gp,
                    // but that's embedded down in the mod list

                    // Assume they must be using a gene part next to a het block.  Bad?
                    if (not (gp.Part.Gene.StartsWith("g"))) then
                        GslResult.err (HeterologyExpansionError.NonGeneNeighbourPart gp.Part.Gene)
                    else
                        let s =
                            translateGenePrefix assembly.Pragmas rg' StandardSlice.Gene // Start with standard slice

                        let startSlice =
                            ApplySlices.applySlices verbose gp.Part.Modifiers s // Apply modifiers

                        let newSlice =
                            { startSlice with
                                  Left =
                                      { startSlice.Left with
                                            Position =
                                                startSlice.Left.Position
                                                + (alt.Length * 1<OneOffset>) } }

                        assert (alt <> sliceSeq.[0..alt.Length - 1])
                        // Assemble new mod list by getting rid of existing slice mods and
                        // putting in new consolidated slice.
                        let newMods =
                            Modifier.Slice(newSlice)
                            :: (gp.Part.Modifiers |> List.filter modIsNotSlice)

                        let result =
                            { Part =
                                  Part.GenePart
                                      { gp with
                                            Part = { gp.Part with Modifiers = newMods } }
                              Pragma = pr3
                              IsForward = fwd3 }
                            :: { Part = Part.InlineDna alt
                                 Pragma =
                                     pr2
                                     |> PragmaCollection.add
                                         { Pragma.Definition = BuiltIn.inlinePragmaDef
                                           Arguments = [] }
                                 IsForward = fwd2 }
                               :: { Part = Part.GenePart gpUp
                                    Pragma = pr1
                                    IsForward = fwd1 }
                                  :: res
                        // Prepend backwards as we will flip list at end - watch out, pragmas are reversed as well
                        scan parameters assembly result tl

        | { Part = Part.GenePart gpUp
            Pragma = pr1
            IsForward = fwd1 } :: { Part = Part.InlineDna i
                                    Pragma = pr2
                                    IsForward = fwd2 } :: { Part = Part.HeterologyBlock
                                                            Pragma = pr3
                                                            IsForward = _ } :: { Part = Part.GenePart gp
                                                                                 Pragma = pr4
                                                                                 IsForward = fwd4 } :: tl ->
            let rg' =
                getReferenceGenome assembly references pr4

            let rg'' =
                getReferenceGenome assembly references pr1

            // Get DNA sequence for this slice
            let sliceSeq =
                realizeSequence verbose assembly.Pragmas fwd4 rg' gp
            // Get DNA sequence for upstream slice
            let sliceSeqUp =
                realizeSequence verbose assembly.Pragmas fwd1 rg'' gpUp
            // Get optional length spec for the HB
            getLenPragma pr3
            // Generate an alternative prefix for the GENEPART on RHS
            >>= fun targetAALen ->
                    let alt =
                        generateRightHB
                            codonUsage
                            Default.MinHBCodonUsage
                            targetAALen
                            assembly.DesignParams
                            sliceSeqUp
                            i
                            sliceSeq

                    assert (alt <> sliceSeq.[0..alt.Length - 1])

                    if verbose then
                        printf "// %O -> %O\n" alt sliceSeq.[0..alt.Length - 1]

                    // tricky part - need to slightly adjust the slice range of gp,
                    // but that's embedded down in the mod list

                    // Assume they must be using a gene part next to a het block.  Bad?
                    if not (gp.Part.Gene.[0] = 'g') then
                        GslResult.err (HeterologyExpansionError.NonGeneNeighbourPart gp.Part.Gene)
                    else

                        let s =
                            translateGenePrefix assembly.Pragmas rg'' StandardSlice.Gene // Start with standard slice

                        let startSlice =
                            ApplySlices.applySlices verbose gp.Part.Modifiers s // Apply modifiers

                        let newSlice =
                            { startSlice with
                                  Left =
                                      { startSlice.Left with
                                            Position =
                                                startSlice.Left.Position
                                                + (alt.Length * 1<OneOffset>) } }

                        let newInline = DnaOps.append i alt

                        // Assemble new mod list by getting rid of existing slice mods and putting in new consolidated slice.
                        let newMods =
                            Modifier.Slice(newSlice)
                            :: (gp.Part.Modifiers |> List.filter modIsNotSlice)

                        let result =
                            { Part =
                                  Part.GenePart
                                      { gp with
                                            Part = { gp.Part with Modifiers = newMods } }
                              Pragma = pr4
                              IsForward = fwd4 }
                            :: { Part = Part.InlineDna newInline
                                 Pragma =
                                     (pr2
                                      |> PragmaCollection.add
                                          { Pragma.Definition = BuiltIn.inlinePragmaDef
                                            Arguments = [] })
                                 IsForward = fwd2 }
                               :: { Part = Part.GenePart gpUp
                                    Pragma = pr1
                                    IsForward = fwd1 }
                                  :: res

                        // Prepend backwards as we will flip list at end
                        // Note - currently destroy any pragmas attached to the heterology block itself
                        scan parameters assembly result tl

        | { Part = Part.GenePart gp
            Pragma = pr1
            IsForward = fwd1 } :: { Part = Part.HeterologyBlock
                                    Pragma = pr2
                                    IsForward = _ } :: { Part = Part.InlineDna ic
                                                         Pragma = pr3
                                                         IsForward = fwd3 } :: { Part = Part.GenePart gpDown
                                                                                 Pragma = pr4
                                                                                 IsForward = fwd4 } :: tl ->

            // get reference genomes warmed up
            let rg' =
                getReferenceGenome assembly references pr1

            let rg'' =
                getReferenceGenome assembly references pr4

            // get actual sequence for slices (with mods applied)
            let sliceSeq =
                realizeSequence verbose assembly.Pragmas fwd1 rg' gp // Get DNA sequence for this slice

            let sliceSeqDown =
                realizeSequence verbose assembly.Pragmas fwd4 rg'' gpDown // Get DNA sequence for this slice

            getLenPragma pr2 // Get optional length spec for the HB
            >>= fun targetAALen ->
                    // generate hetblock section off upstream slice
                    // Generate an alternative prefix for the GENEPART on LHS
                    let alt =
                        generateLeftHB
                            codonUsage
                            Default.MinHBCodonUsage
                            targetAALen
                            assembly.DesignParams
                            sliceSeq
                            ic
                            sliceSeqDown
                    // tricky part - need to slightly adjust the slice range of gp,
                    // but that's embedded down in the mod list

                    // Assume they must be using a gene part next to a het block.  Bad?
                    if not (gp.Part.Gene.[0] = 'g') then
                        GslResult.err (HeterologyExpansionError.NonGeneNeighbourPart gp.Part.Gene)
                    else
                        // now build up the slice again and apply from scratch to the gene
                        let s =
                            translateGenePrefix assembly.Pragmas rg' StandardSlice.Gene // Start with standard slice

                        let startSlice =
                            ApplySlices.applySlices verbose gp.Part.Modifiers s // Apply modifiers

                        // modify slice to take into account the bit we chopped off
                        let newSlice =
                            { startSlice with
                                  Right =
                                      { startSlice.Right with
                                            Position =
                                                startSlice.Right.Position
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

                            printf "// From: %O \n// To  : %O\n//     : %s %3.0f%%\n" alt (t |> capUsing alt)
                                (arr2seq sim) simProp
                        // Assemble new mod list by getting rid of existing slice mods and
                        // putting in new consolidated slice.
                        let newMods =
                            Modifier.Slice(newSlice)
                            :: (gp.Part.Modifiers |> List.filter modIsNotSlice)

                        let result =
                            { Part = Part.GenePart gpDown
                              Pragma = pr4
                              IsForward = fwd4 }
                            :: { Part = Part.InlineDna newInline
                                 Pragma =
                                     pr3
                                     |> PragmaCollection.add
                                         { Pragma.Definition = BuiltIn.inlinePragmaDef
                                           Arguments = [] }
                                 IsForward = fwd3 }
                               :: { Part =
                                        Part.GenePart
                                            { gp with
                                                  Part = { gp.Part with Modifiers = newMods } }
                                    Pragma = pr1
                                    IsForward = fwd1 }
                                  :: res

                        // Prepend backwards as we will flip list at end
                        // Note - currently destroy pr2 pragmas associated with the hetblock
                        scan parameters assembly result tl

        | { Part = Part.PartId pid1
            Pragma = pr1
            IsForward = fwd1 } :: { Part = Part.HeterologyBlock
                                    Pragma = pr2
                                    IsForward = _ } :: { Part = Part.InlineDna ic
                                                         Pragma = pr3
                                                         IsForward = fwd3 } :: { Part = Part.PartId pid4
                                                                                 Pragma = pr4
                                                                                 IsForward = fwd4 } :: tl ->
            // External part variation
            // ===============================================


            let part1Result =
                ResolveExtPart.fetchFullPartSequence verbose Map.empty pid1
                |> GslResult.fromResult (fun message ->
                    HeterologyExpansionError.ExternalPartResolutionFailure(pid1, message))


            let part4Result =
                ResolveExtPart.fetchFullPartSequence verbose Map.empty pid4
                |> GslResult.fromResult (fun message ->
                    HeterologyExpansionError.ExternalPartResolutionFailure(pid4, message))

            (part1Result, part4Result)
            ||> GslResult.map2 (fun part1 part4 ->
                    let s1 =
                        ResolveExtPart.getExtPartSlice verbose pid1

                    let s4 =
                        ResolveExtPart.getExtPartSlice verbose pid4

                    // Build up upstream and downstream DNA slice
                    let sliceSeq1 =
                        ResolveExtPart.applySliceToExtSequence verbose part1 pr1 fwd1 pid1 s1

                    let sliceSeq4 =
                        ResolveExtPart.applySliceToExtSequence verbose part4 pr4 fwd4 pid4 s4

                    getLenPragma pr2 // Get optional length spec for the HB
                    >>= fun targetAALen ->
                            // generate hetblock sequence by cutting into upstream sequence
                            // Generate an alternative prefix for the GENEPART on LHS
                            let alt =
                                generateLeftHB
                                    codonUsage
                                    Default.MinHBCodonUsage
                                    targetAALen
                                    assembly.DesignParams
                                    sliceSeq1.Dna
                                    ic
                                    sliceSeq4.Dna

                            // Build up the slice mods for the upstream part from scratch
                            // Modify upstream slice to account for the part we chopped off
                            let newSlice: Slice =
                                { s1 with
                                      Right =
                                          { s1.Right with
                                                Position = s1.Right.Position - (alt.Length * 1<OneOffset>) } }

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

                                printf "// From: %O \n// To  : %O\n//     : %s %3.0f%%\n" alt (t |> capUsing alt)
                                    (arr2seq sim) simProp
                            // Assemble new mod list by getting rid of existing slice mods
                            // and putting in new consolidated slice.
                            let newMods =
                                Modifier.Slice(newSlice)
                                :: (pid1.Modifiers |> List.filter modIsNotSlice)

                            let result =
                                { Part = Part.PartId pid4
                                  Pragma = pr4
                                  IsForward = fwd4 }
                                :: { Part = Part.InlineDna newInline
                                     Pragma =
                                         pr3
                                         |> PragmaCollection.add
                                             { Pragma.Definition = BuiltIn.inlinePragmaDef
                                               Arguments = [] }
                                     IsForward = fwd3 }
                                   :: { Part = Part.PartId { pid1 with Modifiers = newMods }
                                        Pragma = pr1
                                        IsForward = fwd1 }
                                      :: res
                            // Prepend backwards as we will flip list at end
                            // Note - currently destroy pr2 pragmas associated with the hetblock
                            scan parameters assembly result tl)
            >>= id
        | hd :: tl -> scan parameters assembly (hd :: res) tl // do nothing
        | [] -> GslResult.ok (List.rev res)

    /// Expand heterology blocks.
    let private expandHB (parameters: Phase2Parameters)
                         (assemblyIn: Assembly)
                         : GslResult<GslSourceCode, HeterologyExpansionError> =

        scan parameters assemblyIn [] assemblyIn.Parts
        >>= fun newParts ->
                let assemblyOut = { assemblyIn with Parts = newParts }

                let remainingHetblocks =
                    assemblyOut.Parts
                    |> List.exists (fun ppp ->
                        match ppp.Part with
                        | Part.HeterologyBlock -> true
                        | _ -> false)

                let newSource = LegacyPrettyPrint.assembly assemblyOut

                if remainingHetblocks then
                    GslResult.err (HeterologyExpansionError.HeterologyBlockLeftInDesign newSource)
                else
                    newSource |> GslResult.ok

    /// Expand all heterology blocks in an AST.
    let expandHetBlocks (parameters: Phase2Parameters)
                        (tree: AstTreeHead)
                        : GslResult<AstTreeHead, BootstrapExecutionError<BootstrapExpandAssemblyError<BootstrapError<Phase1Error>, HeterologyExpansionError>>> =

        let assemblyExpansion = expandHB parameters

        let bootstrapOperation =
            let phase1Params =
                parameters |> Phase1Parameters.fromPhase2

            Bootstrapping.bootstrapExpandLegacyAssembly assemblyExpansion (Bootstrapping.bootstrapPhase1 phase1Params)

        let expansionOnlyOnNodesWithHetBlocks =
            BoostrapSelection.maybeBypassBootstrap ExpandHetBlock bootstrapOperation

        Bootstrapping.executeBootstrap expansionOnlyOnNodesWithHetBlocks Serial tree
