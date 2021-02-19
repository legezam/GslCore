/// Top-level compiler operations.
module GslCore.GslcProcess

open System
open Amyris.Bio
open GslCore.Core.AstExpansion
open GslCore.Ast.LegacyParseTypes
open GslCore.Core.Types
open GslCore.Core.DnaCreation
open GslCore.Pragma
open GslCore.Primer
open GslCore.ProcessCmdLineArgs
open GslCore.Core.PluginTypes
open GslCore.Ast
open Amyris.ErrorHandling

/// Run GSLC on string input.
let rec processGSL (s: ConfigurationState) gslText =

    let opts, plugins, ga = s.Options, s.Plugins, s.GlobalAssets

    let verbose = opts.Verbose
    let pragmaCache = ga.PragmaBuilder
    /// Build up all legal capabilities by going through plugins
    // FIXME: need to inject this and validate legal capabilities
    // Should also eliminate global pragma state while we're at it, if possible.
    let legalCapas =
        plugins
        |> List.map (fun pi -> pi.ProvidesCapas)
        |> List.concat
        |> set

    let alleleSwapAlgs =
        plugins
        |> getAllProviders getAlleleSwapAAProviders

    let l2Providers =
        plugins
        |> getAllProviders getL2KOTitrationProviders

    let phase2WithData =
        phase2
            (not opts.Iter)
            (Some(10))
            opts.DoParallel
            verbose
            legalCapas
            pragmaCache
            alleleSwapAlgs
            ga.ReferenceGenomes
            ga.CodonProvider

    /// Main compiler pipeline.
    let phase1Result =
        LexAndParse.lexAndParse verbose gslText
        >>= Phase1.phase1 legalCapas pragmaCache

    if opts.OnlyPhase1 then
        phase1Result >>= convertAndGatherAssemblies
    else
        phase1Result
        //>>= failOnAssemblyInL2Promoter
        >>= expandLevel2 legalCapas pragmaCache l2Providers ga.ReferenceGenomes
        >>= Phase1.postPhase1 ga.ReferenceGenomes ga.SequenceLibrary
        >>= phase2WithData
        >>= convertAndGatherAssemblies // collect the assemblies in the tree and return them

/// Convert all assemblies to DnaAssemblies.
let materializeDna (s: ConfigurationState) (assem: seq<Assembly>) =
    let opts, library, rgs = s.Options, s.GlobalAssets.SequenceLibrary, s.GlobalAssets.ReferenceGenomes

    let markerProviders =
        s.Plugins |> getAllProviders getMarkerProviders

    if opts.Verbose
    then printf "Processing %d assemblies\n" (Seq.length assem)

    assem
    |> Seq.mapi (fun i a ->
        try
            expandAssembly opts.Verbose markerProviders rgs library i a
            |> ok
        with e -> fail (exceptionToAssemblyMessage a e))
    |> collect
    >>= (fun assemblies ->

        if opts.Verbose then
            printf "log: dnaParts dump\n"

            for a in assemblies do
                printf "log: dnaPart: %s\n" a.name

                for p in a.dnaParts do
                    printf "log:      %s\n" p.description
                    printf "%s\n" (utils.format60 p.dna.arr)

        // Check for reused pieces and number them accordingly
        // Make a list of all parts, determine unique set and assign ids
        let partIDs =
            seq {
                for a in assemblies do
                    for p in a.dnaParts do
                        yield p.dna
            }
            |> Set.ofSeq
            |> Seq.mapi (fun i dna -> (dna, i))
            |> Map.ofSeq // TODO - could be faster with checksums
        // Relabel the pieces with IDs  - tedious, we have to reconstruct the tree
        List.map (fun a ->
            { a with
                  dnaParts = List.map (fun (p: DNASlice) -> { p with id = Some(partIDs.[p.dna]) }) a.dnaParts })
            assemblies
        |> ok)


/// Promote long slices to regular rabits to avoid trying to build
/// impossibly long things with oligos.
let cleanLongSlicesInPartsList (p: PragmaCollection) (l: DNASlice list) =
    l
    |> List.map (fun s ->
        if (s.sliceType = INLINEST
            && s.dna.Length > 30
            && not
                (s.pragmas
                 |> PragmaCollection.contains BuiltIn.inlinePragmaDef)) then
            { s with
                  sliceType = REGULAR
                  dnaSource =
                      match s.pragmas
                            |> PragmaCollection.tryGetValue BuiltIn.dnaSrcPragmaDef with
                      | Some (x) -> x
                      | None ->
                          match p
                                |> PragmaCollection.tryGetValue BuiltIn.refGenomePragmaDef with
                          | None -> "synthetic"
                          | Some (x) -> x
                  // add in an amp tag on this guy too, since we are now comitting to
                  // not placing it inline using primers
                  pragmas =
                      match s.pragmas
                            |> PragmaCollection.tryFind BuiltIn.ampPragmaDef with
                      | Some _ -> s.pragmas // already there
                      | None ->
                          s.pragmas
                          |> PragmaCollection.add
                              { Pragma.Definition = BuiltIn.ampPragmaDef
                                Arguments = [] } }
        else
            s)

/// Promote long slices to regular rabits to avoid trying to build
/// impossibly long things with oligos.
let cleanLongSlices _ (a: DnaAssembly) =
    ok
        { a with
              dnaParts = cleanLongSlicesInPartsList a.pragmas a.dnaParts }


/// we run into trouble during primer generation if a virtual part (fuse) gets between two parts that
/// would otherwise get fused anyway (a dna slice and a linker for example).   Strip out the fuse diective
/// in this case, otherwise primer doesn't get built against the real target
let preProcessFuse _ (a: DnaAssembly) =
    let rec proc (l: DNASlice list) res =
        match l with
        | [] -> List.rev res
        | hd :: middle :: tl when hd.sliceType = SliceType.FUSIONST
                                  && middle.sliceType = SliceType.INLINEST -> proc tl (middle :: res)
        | hd :: tl -> proc tl (hd :: res)

    ok
        { a with
              dnaParts = (proc a.dnaParts []) }

/// Once GSL is expanded as far as possible,
/// go into more target-specific activities like assigning parts, reusing parts, etc.
let transformAssemblies (s: ConfigurationState) (assemblies: DnaAssembly list) =

    let atContext: ATContext = { Options = s.Options; GlobalAssets = s.GlobalAssets }

    let builtinAssemblyTransforms = [ cleanLongSlices; preProcessFuse ]

    let assemblyTransformers =
        builtinAssemblyTransforms
        @ (getAllProviders getAssemblyTransformers s.Plugins)

    // do all the assembly transformation steps
    // the transformations are done in the order in which plugins were passed in, and in order
    // inside each plugin if it provides multiple transformers.
    // TODO: should force transformers to provide a description, then provide command line args for
    // listing all available transformations and showing which transformations will run given the
    // provided args.
    /// use all assembly transformers to transform an assembly
    let transformAssembly a =
        assemblyTransformers
        |> List.fold (fun r transformer -> r >>= (transformer atContext)) (ok a)

    /// Attempt to transform all of the assemblies
    assemblies
    |> List.map transformAssembly
    |> collect

let doPrimerDesign opts assemblyOuts =
    if opts.NoPrimers then
        None, assemblyOuts
    else
        let p, t = PrimerCreation.designPrimers opts assemblyOuts
        Some(p), t



let doOutputGeneration (s: ConfigurationState) primers assemblies =
    let outputData =
        { GlobalAssets = s.GlobalAssets
          Options = s.Options
          Assemblies = assemblies
          Primers = primers }

    if outputData.Options.Verbose then printfn "ok"

    // Use any output providers provided by plugins
    // They have already been configured to run or not during command line arg parsing.
    for op in getAllProviders getOutputProviders s.Plugins do
        op.ProduceOutput(outputData)
