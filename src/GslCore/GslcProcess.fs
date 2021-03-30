/// Top-level compiler operations.
namespace GslCore.GslcProcess

open Amyris.Bio
open GslCore.Ast.Algorithms
open GslCore.Ast.ErrorHandling
open GslCore.Ast.Phase1
open GslCore.Ast.Phase1
open GslCore.Ast.Process.AssemblyStuffing
open GslCore.Ast.Process.Naming
open GslCore.Ast.Types
open GslCore.Core.ResolveExtPart
open GslCore.Legacy
open GslCore.Ast.Phase1
open GslCore.Constants
open GslCore.Core
open GslCore.Legacy.Types
open GslCore.Legacy.AstMessage
open GslCore.Core.Expansion
open GslCore.Core.Types
open GslCore.Core.DnaCreation
open GslCore.Pragma
open GslCore.Primer
open GslCore.ProcessCmdLineArgs
open GslCore.Core.PluginTypes
open GslCore.Core.AssemblyGathering
open GslCore.Core.Expansion.Level2Expansion
open GslCore.Core.Expansion.AstMessage
open GslCore.Ast
open GslCore.GslResult



[<RequireQualifiedAccess>]
type GslProcessError =
    | LexParseError of LexParseError
    | Phase1Error of Phase1Error
    | AssemblyGatheringError of FoldMapError<LegacyAssemblyCreationError, PragmaEnvironmentError>
    | L2ExpansionError of FoldMapError<Level2ExpansionError, PragmaEnvironmentError>
    | PostPhase1Error of PostPhase1Error
    | Phase2Error of Phase2Error

module GslProcessError =
    let toAstMessage: GslProcessError -> AstMessage =
        function
        | GslProcessError.LexParseError inner -> inner |> LexParseError.toAstMessage
        | GslProcessError.Phase1Error inner -> inner |> Phase1Error.toAstMessage
        | GslProcessError.AssemblyGatheringError inner ->
            match inner with
            | MapError assemblyCreationError ->
                assemblyCreationError
                |> LegacyAssemblyCreationError.toAstMessage
            | StateUpdateError (PragmaEnvironmentError.PragmaArgument (argError, node)) ->
                argError |> PragmaArgumentError.toAstMessage node
        | GslProcessError.L2ExpansionError inner ->
            match inner with
            | MapError expansionError ->
                expansionError
                |> Level2ExpansionError.toAstMessage
            | StateUpdateError (PragmaEnvironmentError.PragmaArgument (argError, node)) ->
                argError |> PragmaArgumentError.toAstMessage node
        | GslProcessError.PostPhase1Error inner ->
            match inner with
            | PostPhase1Error.Naming naming -> naming |> NamingError.toAstMessage
            | PostPhase1Error.NameCheck namecheck -> namecheck |> NameCheckError.toAstMessage
        | GslProcessError.Phase2Error inner -> inner |> Phase2Error.toAstMessage

module ExternalPartResolutionError =
    let toAssemblyTransformationMessage (assembly: Assembly)
                                        : ExternalPartResolutionError -> AssemblyTransformationMessage<Assembly> =
        function
        | ExternalPartResolutionError.UnresolvableReference pid ->
            AssemblyTransformationMessage.messageToAssemblyMessage
                assembly
                (sprintf "partId reference %s isn't a defined alias and doesn't start with r for rabit" pid)
        | ExternalPartResolutionError.UnprocessableRabitModifiers legacyPartId ->
            AssemblyTransformationMessage.messageToAssemblyMessage
                assembly
                (sprintf "could not process mods for rabit %s %A" legacyPartId.Id legacyPartId.Modifiers)
        | ExternalPartResolutionError.UnimplementedExternalPartSpace partSpace ->
            AssemblyTransformationMessage.messageToAssemblyMessage
                assembly
                (sprintf "unimplemented external partSpace %s" partSpace)

module MarkerPartExpansionError =
    let toAssemblyTransformationMessage (assembly: Assembly)
                                        : MarkerPartExpansionError -> AssemblyTransformationMessage<Assembly> =
        function
        | MarkerPartExpansionError.NoProvidersProvided ->
            AssemblyTransformationMessage.messageToAssemblyMessage
                assembly
                "Marker part substitution, no marker providers available"
        | MarkerPartExpansionError.UnknownMarkerSet (provided, markers) ->
            AssemblyTransformationMessage.messageToAssemblyMessage
                assembly
                (sprintf "unknown marker set %s, expected one of %s" provided (String.concat ", " markers))

module ModifierValidationError =
    let toAssemblyTransformationMessage (assembly: Assembly)
                                        : ModifierValidationError -> AssemblyTransformationMessage<Assembly> =
        function
        | ModifierValidationError.SliceCoordinateCollision (slice, positions) ->
            AssemblyTransformationMessage.messageToAssemblyMessage
                assembly
                (sprintf
                    "slice left %A greater than right %A %O"
                     slice.Left.Position
                     slice.Right.Position
                     (SourcePosition.formatSourcePositionList positions))

module GenePartExpansionError =
    let toAssemblyTransformationMessage (assembly: Assembly)
                                        : GenePartExpansionError -> AssemblyTransformationMessage<Assembly> =
        function
        | GenePartExpansionError.ModifierValidation inner ->
            inner
            |> ModifierValidationError.toAssemblyTransformationMessage assembly
        | GenePartExpansionError.UnknownGene (gene, positions) ->
            AssemblyTransformationMessage.messageToAssemblyMessage
                assembly
                (sprintf "undefined gene '%s' %O" gene positions)
        | GenePartExpansionError.UnsupportedApproximateSlices ->
            AssemblyTransformationMessage.messageToAssemblyMessage
                assembly
                (sprintf
                    "sorry, approximate slices of library genes not supported yet in %A"
                     (LegacyPrettyPrint.assembly assembly))
        | GenePartExpansionError.UnsupportedDotModifier modifier ->
            AssemblyTransformationMessage.messageToAssemblyMessage
                assembly
                (sprintf "Unimplemented dot modifier: %s" modifier)
        | GenePartExpansionError.SliceWithNegativeLength (slice, genePart) ->
            AssemblyTransformationMessage.messageToAssemblyMessage
                assembly
                (sprintf
                    "slice results in negatively lengthed DNA piece for %s -> %s"
                     genePart.Gene
                     (slice |> LegacyPrettyPrint.slice))


module PartExpansionError =
    let toAssemblyTransformationMessage (assembly: Assembly)
                                        : PartExpansionError -> AssemblyTransformationMessage<Assembly> =
        function
        | PartExpansionError.UnexpandedHeterologyBlock ->
            AssemblyTransformationMessage.messageToAssemblyMessage
                assembly
                "unexpanded heterology block encountered during DNA generation"

        | PartExpansionError.UnexpandedInlineProteinSequence ->
            AssemblyTransformationMessage.messageToAssemblyMessage
                assembly
                "unexpanded protein inline encountered during DNA generation"

        | PartExpansionError.ExternalPartResolution innerError ->
            innerError
            |> ExternalPartResolutionError.toAssemblyTransformationMessage assembly

        | PartExpansionError.MarkerPart innerError ->
            innerError
            |> MarkerPartExpansionError.toAssemblyTransformationMessage assembly
        | PartExpansionError.GenePart innerError ->
            innerError
            |> GenePartExpansionError.toAssemblyTransformationMessage assembly

module AssemblyExpansionError =
    let toAssemblyTransformationMessage: AssemblyExpansionError -> AssemblyTransformationMessage<Assembly> =
        function
        | AssemblyExpansionError.PartExpansion (assembly, innerError) ->
            innerError
            |> PartExpansionError.toAssemblyTransformationMessage assembly

module GslcProcess =
    let private lexAndParse verbose gslText =
        LexAndParse.lexAndParse verbose gslText
        |> GslResult.mapError GslProcessError.LexParseError

    let private phase1 phase1Params =
        Phase1.phase1 phase1Params
        >> GslResult.mapError GslProcessError.Phase1Error

    let private assemblyGathering =
        AssemblyGathering.convertAndGatherAssemblies
        >> GslResult.mapError GslProcessError.AssemblyGatheringError


    let private l2Expansion phase1Params l2Providers referenceGenomes =
        Level2Expansion.expandLevel2 phase1Params l2Providers referenceGenomes
        >> GslResult.mapError GslProcessError.L2ExpansionError

    let private postPhase1 referenceGenomes sequenceLibrary =
        Phase1.postPhase1 referenceGenomes sequenceLibrary
        >> GslResult.mapError GslProcessError.PostPhase1Error

    let private phase2 phase2Params =
        Phase2.phase2 phase2Params
        >> GslResult.mapError GslProcessError.Phase2Error

    /// Run GSLC on string input.
    let processGSL (s: ConfigurationState) gslText =

        let options = s.Options
        let plugins = s.Plugins
        let globalAssets = s.GlobalAssets

        let verbose = options.Verbose
        let pragmaBuilder = globalAssets.PragmaBuilder
        /// Build up all legal capabilities by going through plugins
        // TODO: need to inject this and validate legal capabilities
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

        let phase1Result =
            lexAndParse verbose gslText
            >>= phase1 phase1Params

        if options.OnlyPhase1 then
            phase1Result >>= assemblyGathering
        else
            phase1Result
            >>= l2Expansion phase1Params l2Providers globalAssets.ReferenceGenomes
            >>= postPhase1 globalAssets.ReferenceGenomes globalAssets.SequenceLibrary
            >>= phase2 phase2Params
            >>= assemblyGathering

    /// Convert all assemblies to DnaAssemblies.
    let materializeDna (s: ConfigurationState) (assemblies: Assembly list) =
        let opts, library, rgs =
            s.Options, s.GlobalAssets.SequenceLibrary, s.GlobalAssets.ReferenceGenomes

        let markerProviders =
            s.Plugins
            |> Behavior.getAllProviders Behavior.getMarkerProviders

        if opts.Verbose then
            printf "Processing %d assemblies\n" (List.length assemblies)

        assemblies
        |> List.mapi (fun index dnaAssembly ->
            try
                DnaCreation.expandAssembly opts.Verbose markerProviders rgs library index dnaAssembly
                |> GslResult.mapError AssemblyExpansionError.toAssemblyTransformationMessage
            with ex -> GslResult.err (AssemblyTransformationMessage.exceptionToAssemblyMessage dnaAssembly ex))
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
    let cleanLongSlicesInPartsList (pragmas: PragmaCollection)
                                   (slices: DNASlice list)
                                   : GslResult<DNASlice list, PragmaArgumentError> =
        slices
        |> List.map (fun slice ->
            let isInline = slice.Type = SliceType.Inline
            let isLong = slice.Dna.Length > 30

            let mustBeInline =
                slice.Pragmas
                |> PragmaCollection.contains BuiltIn.inlinePragmaDef

            if isInline && isLong && not mustBeInline then
                let source =
                    match slice.Pragmas
                          |> PragmaCollection.tryGetValue BuiltIn.dnaSrcPragmaDef with
                    | Some x -> x
                    | None ->
                        match pragmas
                              |> PragmaCollection.tryGetValue BuiltIn.refGenomePragmaDef with
                        | None -> "synthetic"
                        | Some x -> x


                // add in an amp tag on this guy too, since we are now comitting to
                // not placing it inline using primers
                match slice.Pragmas
                      |> PragmaCollection.tryFind BuiltIn.ampPragmaDef with
                | Some _ -> GslResult.ok slice.Pragmas // already there
                | None ->
                    slice.Pragmas
                    |> PragmaCollection.add
                        { Pragma.Definition = BuiltIn.ampPragmaDef
                          Arguments = [] }
                |> GslResult.map (fun pragmas ->
                    { slice with
                          Type = SliceType.Regular
                          DnaSource = source
                          Pragmas = pragmas })

            else
                GslResult.ok slice)
        |> GslResult.collectA

    /// Promote long slices to regular rabits to avoid trying to build
    /// impossibly long things with oligos.
    let cleanLongSlices _ (assembly: DnaAssembly): GslResult<DnaAssembly, AssemblyTransformationMessage<DnaAssembly>> =
        cleanLongSlicesInPartsList assembly.Pragmas assembly.DnaParts
        |> GslResult.map (fun parts -> { assembly with DnaParts = parts })
        |> GslResult.mapError (fun error ->
            let message = sprintf "%A" error
            AssemblyTransformationMessage.messageToAssemblyMessage assembly message)


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
        let transformAssembly (assembly: DnaAssembly) =
            let transform (currentResult: DnaAssembly)
                          (transformer: ATContext -> DnaAssembly -> GslResult<DnaAssembly, AssemblyTransformationMessage<DnaAssembly>>)
                          : GslResult<DnaAssembly, AssemblyTransformationMessage<DnaAssembly>> =
                transformer atContext currentResult

            assemblyTransformers
            |> GslResult.foldM transform assembly

        /// Attempt to transform all of the assemblies
        assemblies
        |> List.map transformAssembly
        |> GslResult.collectA

    let doPrimerDesign (opts: ParsedOptions) (assemblyOuts: DnaAssembly list) =
        if opts.NoPrimers then
            None, assemblyOuts
        else
            let primers, assemblies =
                PrimerCreation.designPrimersForAssemblies opts assemblyOuts

            Some(primers), assemblies



    let doOutputGeneration (s: ConfigurationState) primers assemblies =
        let outputData =
            { GlobalAssets = s.GlobalAssets
              Options = s.Options
              Assemblies = assemblies
              Primers = primers }

        if outputData.Options.Verbose then
            printfn "ok"

        // Use any output providers provided by plugins
        // They have already been configured to run or not during command line arg parsing.
        for op in Behavior.getAllProviders Behavior.getOutputProviders s.Plugins do
            op.ProduceOutput(outputData)
