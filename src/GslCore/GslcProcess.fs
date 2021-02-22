/// Top-level compiler operations.
module GslCore.GslcProcess

open System
open GslCore.Ast.ErrorHandling
open Amyris.Bio
open GslCore.Constants
open GslCore.Core
open GslCore.Ast.LegacyParseTypes
open GslCore.Core.Expansion
open GslCore.Core.Types
open GslCore.Core.DnaCreation
open GslCore.Pragma
open GslCore.Primer
open GslCore.ProcessCmdLineArgs
open GslCore.Core.PluginTypes
open GslCore.Ast
open GslCore.GslResult

/// Run GSLC on string input.
let rec processGSL (s: ConfigurationState) gslText =

    let options = s.Options
    let plugins = s.Plugins
    let globalAssets = s.GlobalAssets

    let verbose = options.Verbose
    let pragmaBuilder = globalAssets.PragmaBuilder
    /// Build up all legal capabilities by going through plugins
    // FIXME: need to inject this and validate legal capabilities
    // Should also eliminate global pragma state while we're at it, if possible.
    let legalCapas =
        plugins
        |> List.map (fun pi -> pi.ProvidesCapas)
        |> List.concat
        |> Set.ofList

    let alleleSwapAlgs =
        plugins
        |> Behavior.getAllProviders Behavior.getAlleleSwapAAProviders

    let l2Providers =
        plugins
        |> Behavior.getAllProviders Behavior.getL2KOTitrationProviders

    let phase2Params =
        { Phase2Parameters.Parallel = options.DoParallel
          OneShot = not options.Iter
          MaxPasses = Default.maxPhase2Passes
          Verbose = verbose
          LegalCapas = legalCapas
          PragmaBuilder = pragmaBuilder
          AlleleSwapProviders = alleleSwapAlgs
          References = globalAssets.ReferenceGenomes
          CodonTableCache = globalAssets.CodonProvider }

    let phase1Params =
        phase2Params |> Phase1Parameters.fromPhase2
    /// Main compiler pipeline.
    let phase1Result =
        LexAndParse.lexAndParse verbose gslText
        >>= Phase1.phase1 phase1Params

    if options.OnlyPhase1 then
        phase1Result
        >>= AssemblyGathering.convertAndGatherAssemblies
    else
        phase1Result
        //>>= failOnAssemblyInL2Promoter
        >>= Level2Expansion.expandLevel2 phase1Params l2Providers globalAssets.ReferenceGenomes
        >>= Phase1.postPhase1 globalAssets.ReferenceGenomes globalAssets.SequenceLibrary
        >>= (Phase2.phase2 phase2Params)
        >>= AssemblyGathering.convertAndGatherAssemblies // collect the assemblies in the tree and return them

/// Convert all assemblies to DnaAssemblies.
let materializeDna (s: ConfigurationState) (assem: seq<Assembly>) =
    let opts, library, rgs =
        s.Options, s.GlobalAssets.SequenceLibrary, s.GlobalAssets.ReferenceGenomes

    let markerProviders =
        s.Plugins
        |> Behavior.getAllProviders Behavior.getMarkerProviders

    if opts.Verbose
    then printf "Processing %d assemblies\n" (Seq.length assem)

    assem
    |> Seq.mapi (fun index dnaAssembly ->
        try
            expandAssembly opts.Verbose markerProviders rgs library index dnaAssembly
            |> GslResult.ok
        with ex -> GslResult.err (AssemblyTransformationMessage.exceptionToAssemblyMessage dnaAssembly ex))
    |> Seq.toList
    |> GslResult.collectA
    >>= (fun assemblies ->

        if opts.Verbose then
            printf "log: dnaParts dump\n"

            for a in assemblies do
                printf "log: dnaPart: %s\n" a.Name

                for p in a.DnaParts do
                    printf "log:      %s\n" p.Description
                    printf "%s\n" (utils.format60 p.Dna.arr)

        // Check for reused pieces and number them accordingly
        // Make a list of all parts, determine unique set and assign ids
        let partIDs =
            seq {
                for a in assemblies do
                    for p in a.DnaParts do
                        yield p.Dna
            }
            |> Set.ofSeq
            |> Seq.mapi (fun i dna -> (dna, i))
            |> Map.ofSeq // TODO - could be faster with checksums
        // Relabel the pieces with IDs  - tedious, we have to reconstruct the tree
        List.map (fun a ->
            { a with
                  DnaParts = List.map (fun (p: DNASlice) -> { p with Id = Some(partIDs.[p.Dna]) }) a.DnaParts })
            assemblies
        |> GslResult.ok)


/// Promote long slices to regular rabits to avoid trying to build
/// impossibly long things with oligos.
let cleanLongSlicesInPartsList (p: PragmaCollection) (l: DNASlice list) =
    l
    |> List.map (fun s ->
        if (s.Type = SliceType.Inline
            && s.Dna.Length > 30
            && not
                (s.Pragmas
                 |> PragmaCollection.contains BuiltIn.inlinePragmaDef)) then
            { s with
                  Type = SliceType.Regular
                  DnaSource =
                      match s.Pragmas
                            |> PragmaCollection.tryGetValue BuiltIn.dnaSrcPragmaDef with
                      | Some (x) -> x
                      | None ->
                          match p
                                |> PragmaCollection.tryGetValue BuiltIn.refGenomePragmaDef with
                          | None -> "synthetic"
                          | Some (x) -> x
                  // add in an amp tag on this guy too, since we are now comitting to
                  // not placing it inline using primers
                  Pragmas =
                      match s.Pragmas
                            |> PragmaCollection.tryFind BuiltIn.ampPragmaDef with
                      | Some _ -> s.Pragmas // already there
                      | None ->
                          s.Pragmas
                          |> PragmaCollection.add
                              { Pragma.Definition = BuiltIn.ampPragmaDef
                                Arguments = [] } }
        else
            s)

/// Promote long slices to regular rabits to avoid trying to build
/// impossibly long things with oligos.
let cleanLongSlices _ (a: DnaAssembly): GslResult<DnaAssembly, _> =
    GslResult.ok
        { a with
              DnaParts = cleanLongSlicesInPartsList a.Pragmas a.DnaParts }


/// we run into trouble during primer generation if a virtual part (fuse) gets between two parts that
/// would otherwise get fused anyway (a dna slice and a linker for example).   Strip out the fuse diective
/// in this case, otherwise primer doesn't get built against the real target
let preProcessFuse _ (a: DnaAssembly): GslResult<DnaAssembly, _> =
    let rec proc (l: DNASlice list) res =
        match l with
        | [] -> List.rev res
        | hd :: middle :: tl when hd.Type = SliceType.Fusion
                                  && middle.Type = SliceType.Inline -> proc tl (middle :: res)
        | hd :: tl -> proc tl (hd :: res)

    GslResult.ok
        { a with
              DnaParts = (proc a.DnaParts []) }

/// Once GSL is expanded as far as possible,
/// go into more target-specific activities like assigning parts, reusing parts, etc.
let transformAssemblies (configurationState: ConfigurationState)
                        (assemblies: DnaAssembly list)
                        : GslResult<DnaAssembly list, AssemblyTransformationMessage<DnaAssembly>> =

    let atContext: ATContext =
        { Options = configurationState.Options
          GlobalAssets = configurationState.GlobalAssets }

    let builtinAssemblyTransforms = [ cleanLongSlices; preProcessFuse ]

    let assemblyTransformers =
        builtinAssemblyTransforms
        @ (Behavior.getAllProviders Behavior.getAssemblyTransformers configurationState.Plugins)

    // do all the assembly transformation steps
    // the transformations are done in the order in which plugins were passed in, and in order
    // inside each plugin if it provides multiple transformers.
    // TODO: should force transformers to provide a description, then provide command line args for
    // listing all available transformations and showing which transformations will run given the
    // provided args.
    /// use all assembly transformers to transform an assembly
    let transformAssembly a =
        assemblyTransformers
        |> List.fold (fun r transformer -> r >>= (transformer atContext)) (GslResult.ok a)

    /// Attempt to transform all of the assemblies
    assemblies
    |> List.map transformAssembly
    |> GslResult.collectA

let doPrimerDesign opts assemblyOuts =
    if opts.NoPrimers then
        None, assemblyOuts
    else
        let p, t =
            PrimerCreation.designPrimers opts assemblyOuts

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
    for op in Behavior.getAllProviders Behavior.getOutputProviders s.Plugins do
        op.ProduceOutput(outputData)
