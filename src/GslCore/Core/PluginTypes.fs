/// Definitions of plug-in types and interfaces.
module GslCore.PluginTypes

open Amyris.ErrorHandling
open GslCore.Core.Types
open GslCore.CommandConfig
open Amyris.Bio
open GslCore.Constants
open GslCore.Ast.LegacyParseTypes
open GslCore.Pragma
open GslCore.Ast.Types
open Amyris.Bio.IO.CodonUsage
open GslCore
open Amyris.Dna
open GslCore.Reference

/// Interface specification for plugins that want to inject command line arguments and
/// be configured from the command line.
type IConfigurable<'T> =
    /// Return the list of the names of command line arguments this behavior accepts.
    abstract ProvidedArgs: unit -> CmdLineArgSpec list
    /// Configure this behavior and return a configured version, allowing for immutable behaviors.
    abstract Configure: ParsedCmdLineArg -> 'T
    /// Configure this behavior using the built-in compiler options.
    /// This enables sharing of items like the lib directory, verbose flag, etc.
    abstract ConfigureFromOptions: ParsedOptions -> 'T

// =======================
// plugin behavior defintion for codon cache and optimization
// =======================

type CodonOptTask =
    { IsVerbose: bool
      SeedOverride: int option
      RefGenome: GenomeDefinition
      AminoAcidSequence: string }

type ICodonProvider =
    /// Allow codon opt providers to add command line args and be configurable.
    inherit IConfigurable<ICodonProvider>

    /// Codon optimizers use the pragma environment to configure themselves locally.
    abstract Setup: PragmaCollection -> ICodonProvider

    ///<summary>Perform codon optimization using a particular reference genome on a string
    /// representing a protein sequence, returning a
    /// codon-optimized version.  Optionally override the RNG seed for this particular run, as well
    /// as set verbosity.
    ///</summary>
    // FIXME: this should both accept and return a domain type from Amyris.Bio.  May need to
    // define a domain type for AA sequences to match Amyris.Dna
    abstract DoCodonOpt: CodonOptTask -> Dna

    ///Provide a codon usage lookup table for the given ref genome.
    abstract GetCodonLookupTable: GenomeDefinition -> CodonLookup


/// Helpful wrapper type for handing around GSLC's static assets and caches.
type GlobalAssets =
    { SequenceLibrary: SequenceLibrary
      CodonProvider: ICodonProvider
      PragmaBuilder: PragmaBuilder
      ReferenceGenomes: GenomeDefinitions }
// =========================
// plugin behavior defintion for allele swaps
// =========================

type EndPref =
    | NTERM
    | CTERM
    | NONETERM

/// Amount of extra dna adjacent to the ORF to include
let orfPlusMargin = 100

type AlleleSwapJobAccept = Capabilities -> float<PluginScore> option

type AlleleSwapDesignParams =
    { IsVerbose: bool
      IsLongStyle: bool
      EndPref: EndPref
      CodonLookup: CodonLookup
      Gene: string
      Name: string
      ReferenceGenome: GenomeDefinition
      Feature: sgd.Feature
      Mutation: Mutation
      Length: int<ZeroOffset>
      MutOff: int<ZeroOffset>
      OrfDna: Dna
      OrfPlusDna: Dna
      Pragmas: PragmaCollection }

type AlleleSwapProvider =
    { JobScorer: AlleleSwapJobAccept
      Provider: AlleleSwapDesignParams -> GslSourceCode }



// ==================================================
// Marker handler for converting ### into sequence
// ==================================================

type MarkerProviderJobAccept = Capabilities -> float<PluginScore> option

/// Information provided when a marker materialization happens
type MarkerMaterializationTask =
    { MarkerSet: string
      DnaSource: string
      PartPlusPragma: PPP }

type IMarkerProvider =
    /// Allow marker providers to add command line args and be configurable.
    inherit IConfigurable<IMarkerProvider>

    /// Marker providers the pragma environment to configure themselves locally.
    abstract Setup: PragmaCollection -> IMarkerProvider
    /// When is comes time to convert a marker into a concrete DNA sequence (whole marker), this gets called
    abstract CreateDna: MarkerMaterializationTask -> DNASlice
    abstract IsLegal: string -> bool
    abstract ListMarkers: unit -> string list
    abstract ScoreJob: MarkerProviderJobAccept

// =======================
// plugin behavior definition for l2 expansion
// =======================

type L2JobAccept = Capabilities -> float<PluginScore> option

type L2DesignParams =
    { ReferenceGenomes: GenomeDefinitions
      IsMegastitch: bool
      ReferenceGenome: string
      Line: BuiltL2Expression
      Pragmas: PragmaCollection }

type L2Provider =
    { JobScorer: L2JobAccept
      ExplicitLocusProvider: L2Id -> L2DesignParams -> GslSourceCode
      ImplicitLocusProvider: L2DesignParams -> GslSourceCode }

// ======================
// plugin behavior definition for output assembly transformations
// ======================

type ATContext =
    { GlobalAssets: GlobalAssets
      Options: ParsedOptions }

type AssemblyTransformationMessageKind =
    | ATError
    | ATWarning
    override x.ToString() =
        match x with
        | ATError -> "Error"
        | ATWarning -> "Warning"

/// Domain type for errors encountered during transformation.
/// The type of assembly is left generic to permit re-use of this type during both
/// DNA materialization and transformation of DnaAssemblies.
type AssemblyTransformationMessage<'A when 'A :> ISourcePosition> =
    { Message: string
      Kind: AssemblyTransformationMessageKind
      Assembly: 'A
      StackTrace: System.Diagnostics.StackTrace option
      FromException: System.Exception option }
    member x.Format(phase, ?sourceCode, ?verbose) =
        let verbose = defaultArg verbose false

        seq {
            match (x.Assembly :> ISourcePosition)
                .OptionalSourcePosition with
            | [] ->
                yield sprintf "%O during %s:" x.Kind phase
                yield x.Message
            | hd :: tl ->
                yield sprintf "%O during %s %s:" x.Kind phase (SourcePosition.formatSourcePositionList (hd :: tl))
                yield x.Message
                yield "================================================================="

                match sourceCode with
                | Some source -> yield! hd |> SourcePosition.sourceContext source
                | None -> ()

            if verbose then
                yield sprintf "\n%+A" x.Assembly

                match x.StackTrace with
                | Some (s) -> yield s.ToString()
                | None -> ()
        }
        |> String.concat "\n"

/// Convert an exception during assembly transformation into a message.
let exceptionToAssemblyMessage assembly (exc: System.Exception) =
    { Message = exc.Message
      Kind = ATError
      Assembly = assembly
      StackTrace = Some(System.Diagnostics.StackTrace(exc))
      FromException = Some exc }

/// Interface specification for output assembly transformations.
type IAssemblyTransform =
    inherit IConfigurable<IAssemblyTransform>
    /// Perform a transformation of an assembly.
    abstract TransformAssembly: ATContext
     -> DnaAssembly -> Result<DnaAssembly, AssemblyTransformationMessage<DnaAssembly>>

// =======================
// plugin behavior defintion for output file generation
// =======================

type OutputGenerationData =
    { GlobalAssets: GlobalAssets
      Options: ParsedOptions
      Assemblies: DnaAssembly list
      Primers: DivergedPrimerPair list list option }

/// Interface specification for output file format providers.
type IOutputFormat =
    inherit IConfigurable<IOutputFormat>
    /// Possibly produce output if this provider has been configured to run.
    abstract ProduceOutput: OutputGenerationData -> unit

/// Each GSL plugin can provide customized behaviors from this list.
/// This type ensures that individual plugins will not have to update their source code
/// as additional pluggable behaviors are added, as they will simply appear as new options
/// in this type.
type PluginBehavior =
    | AlleleSwapAA of AlleleSwapProvider
    | L2KOTitration of L2Provider
    | OutputFormat of IOutputFormat
    | AssemblyTransform of IAssemblyTransform
    | CodonProvider of ICodonProvider
    | MarkerProvider of IMarkerProvider
    member b.ProvidedArgs() =
        match b with
        | OutputFormat (f) -> f.ProvidedArgs()
        | AssemblyTransform (a) -> a.ProvidedArgs()
        | CodonProvider (c) -> c.ProvidedArgs()
        | _ -> []

/// Wrapper around behavior to allow giving individual behaviors names and descriptions.
type PluginBehaviorWrapper =
    { Name: string option
      Description: string option
      Behavior: PluginBehavior }
    /// Provide a sequence of strings describing this behavior.
    member x.Info =
        seq {
            match x.Name with
            | Some (n) -> yield sprintf "Name: %s" n
            | None -> ()

            match x.Description with
            | Some (d) -> yield sprintf "Description: %s" d
            | None -> ()

            yield sprintf "Type: %s" (Utils.getUnionCaseName x.Behavior)
        }
        |> String.concat "\n"

let configureBehavior arg b =
    match b.Behavior with
    | OutputFormat (f) ->
        { b with
              Behavior = OutputFormat(f.Configure(arg)) }
    | AssemblyTransform (a) ->
        { b with
              Behavior = AssemblyTransform(a.Configure(arg)) }
    | CodonProvider (c) ->
        { b with
              Behavior = CodonProvider(c.Configure(arg)) }
    | AlleleSwapAA _
    | MarkerProvider _
    | L2KOTitration _ -> b

let configureBehaviorFromOpts opts b =
    match b.Behavior with
    | OutputFormat (f) ->
        { b with
              Behavior = OutputFormat(f.ConfigureFromOptions(opts)) }
    | AssemblyTransform (a) ->
        { b with
              Behavior = AssemblyTransform(a.ConfigureFromOptions(opts)) }
    | CodonProvider (c) ->
        { b with
              Behavior = CodonProvider(c.ConfigureFromOptions(opts)) }
    | MarkerProvider (c) ->
        { b with
              Behavior = MarkerProvider(c.ConfigureFromOptions(opts)) }
    | AlleleSwapAA _
    | L2KOTitration _ -> b

/// Data structure specifying one or more behaviors
type Plugin =
    { /// short name
      Name: string
      /// longer description
      Description: string option
      /// behaviors provided by this plugin
      Behaviors: PluginBehaviorWrapper list
      /// new pragmas provided by this plugin
      ProvidesPragmas: PragmaDefinition list
      /// new capabilities enabled by this plugin
      ProvidesCapas: string list }
    /// Return specs for any command line args this plugin provides.
    member x.ProvidedArgs() =
        x.Behaviors
        |> List.map (fun b -> b.Behavior.ProvidedArgs())
        |> List.concat
    /// Given parsed command line args, update any behaviors that need them, returning a configured
    /// plugin.
    member x.Configure(args, opts) =
        let configuredBehaviors =
            args
            |> List.fold (fun behaviors arg ->  // each iteration of fold uses one arg and updates all behaviors
                behaviors |> List.map (configureBehavior arg)) x.Behaviors
            |> List.map (configureBehaviorFromOpts opts)

        { x with
              Behaviors = configuredBehaviors }
    /// Provide a extended description of this plugin and capabilities it provides.
    member x.Info =
        let indent (s: string) =
            s.Split('\n')
            |> Array.map (sprintf "    %s")
            |> String.concat "\n"

        let args = x.ProvidedArgs()

        seq {
            yield sprintf "Name: %s" x.Name

            match x.Description with
            | Some (d) -> yield sprintf "Description:\n    %s" d
            | None -> ()

            if not (x.Behaviors.IsEmpty) then
                yield "Behaviors:"

                yield
                    x.Behaviors
                    |> List.map (fun b -> indent b.Info)
                    |> String.concat "\n\n"

            if not (x.ProvidesPragmas.IsEmpty) then
                yield "Provides pragmas:"
                for p in x.ProvidesPragmas -> indent (PragmaDefinition.format p)

            if not (x.ProvidesCapas.IsEmpty) then
                yield "Provides capas:"
                for c in x.ProvidesCapas -> indent c

            if not (args.IsEmpty) then
                yield "Provides command line arguments:"

                for a in args do
                    yield! (printCmdLineArg a)

        }
        |> String.concat "\n"

/// Get all of the marker providers from a plugin.
let getMarkerProviders (plugin: Plugin) =
    plugin.Behaviors
    |> List.choose (fun b ->
        match b.Behavior with
        | MarkerProvider (m) -> Some(m)
        | _ -> None)


/// Get all of the allele swap providers from a plugin.
let getAlleleSwapAAProviders (plugin: Plugin) =
    plugin.Behaviors
    |> List.choose (fun b ->
        match b.Behavior with
        | AlleleSwapAA (a) -> Some(a)
        | _ -> None)

let getL2KOTitrationProviders (plugin: Plugin) =
    plugin.Behaviors
    |> List.choose (fun b ->
        match b.Behavior with
        | L2KOTitration (a) -> Some(a)
        | _ -> None)

let getAssemblyTransformers (plugin: Plugin) =
    plugin.Behaviors
    |> List.choose (fun b ->
        match b.Behavior with
        | AssemblyTransform (a) -> Some(a.TransformAssembly)
        | _ -> None)

let getOutputProviders (plugin: Plugin) =
    plugin.Behaviors
    |> List.choose (fun b ->
        match b.Behavior with
        | OutputFormat (a) -> Some(a)
        | _ -> None)

let getCodonProviders (plugin: Plugin) =
    plugin.Behaviors
    |> List.choose (fun b ->
        match b.Behavior with
        | CodonProvider (a) -> Some(a)
        | _ -> None)

/// Use a provider extraction function to get every provider from a list of plugins.
let getAllProviders mode plugins = plugins |> List.map mode |> List.concat
