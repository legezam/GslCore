/// Command line arguments, parsing, and command defaults.
module GslCore.Core.CommandConfig

open System
open GslCore.Core.Types
open Amyris.Bio.utils


/// Base type for a command line argument specification.
type CmdLineArgSpec =
    { Name: string
      Parameters: string list
      Aliases: string list
      Description: string }

/// Combination of a command line arg defintion and a behavior to update some options
/// structure based on passed arguments.
type CmdLineArg<'cfg> =
    { Specification: CmdLineArgSpec
      Process: string list -> 'cfg -> 'cfg }

type BuiltinCmdLineArg = CmdLineArg<ParsedOptions>

type CollectedCommandLineArgs =
    { Builtins: Map<string, CmdLineArgSpec>
      BuiltinsWithProcess: Map<string, BuiltinCmdLineArg>
      FromPlugins: Map<string, CmdLineArgSpec> }
    member x.Specs =
        let builtinSpecs = x.Builtins |> Map.toSeq |> Seq.map snd

        let fromPlugins =
            x.FromPlugins |> Map.toSeq |> Seq.map snd

        Seq.append builtinSpecs fromPlugins

    member x.TryFind(name) =
        match x.Builtins.TryFind(name) with
        | Some (a) -> Some(a)
        | None -> x.FromPlugins.TryFind(name)


/// Literal command line argument parsed from input.
type ParsedCmdLineArg =
    { Specification: CmdLineArgSpec
      Values: string list }

let informalVersion =
    AssemblyVersionInformation.AssemblyInformationalVersion // git hash

let version =
    AssemblyVersionInformation.AssemblyVersion

let libRoot =
    match Environment.GetEnvironmentVariable("GSL_LIB") with
    | null -> "gslc_lib"
    | x -> x
    |> smashSlash

module CommandLine =

    let libCmdArg =
        { Specification =
              { Name = "lib"
                Parameters = [ "directory" ]
                Aliases = []
                Description = "directory in which genome definitions reside\nDefault: GSL_LIB var, or 'lib' in current directory" }
          Process = fun p opts -> { opts with LibDir = smashSlash p.[0] } }

    let deterministicCmdArg =
        let processParameters (_parameters: string list) (parsedOptions: ParsedOptions): ParsedOptions =
            { parsedOptions with
                  IsDeterministic = true }

        { CmdLineArg.Specification =
              { CmdLineArgSpec.Name = "deterministic"
                Parameters = []
                Aliases = []
                Description =
                    "Produce deterministic output (= if rerun with same input then will produce identical output)" }
          Process = processParameters }

    /// Define all GSLC command line arguments here.
    /// An argument consists of its name, the names of its parameters, a description,
    /// and a function that takes a list of passed parameters and an options record
    /// and returns a modified options record.
    let builtinCmdLineArgs =
        [ { CmdLineArg.Specification =
                { CmdLineArgSpec.Name = "reflist"
                  Parameters = []
                  Aliases = []
                  Description = "list available reference genomes" }
            Process = fun _ opts -> { opts with RefList = true } }

          { CmdLineArg.Specification =
                { CmdLineArgSpec.Name = "refdump"
                  Parameters = [ "refname" ]
                  Aliases = []
                  Description = "dump available loci in reference genome" }
            Process = fun p opts -> { opts with RefDump = Some(p.[0]) } }

          { CmdLineArg.Specification =
                { CmdLineArgSpec.Name = "step"
                  Parameters = []
                  Aliases = []
                  Description = "expand GSL just one round, and emit intermediate GSL" }
            Process = fun _ opts -> { opts with Iter = false } }

          { CmdLineArg.Specification =
                { CmdLineArgSpec.Name = "verbose"
                  Parameters = []
                  Aliases = []
                  Description = "print debugging info" }
            Process = fun _ opts -> { opts with Verbose = true } }

          { CmdLineArg.Specification =
                { CmdLineArgSpec.Name = "version"
                  Parameters = []
                  Aliases = []
                  Description = "print version information" }
            Process =
                fun _ opts ->
                    printfn "GSL core compiler version %s (%s)" version informalVersion
                    opts }

          { CmdLineArg.Specification =
                { CmdLineArgSpec.Name = "helpPragmas"
                  Parameters = []
                  Aliases = []
                  Description = "print available pragmas" }
            Process = fun _ opts -> { opts with DoHelpPragmas = true } }

          { CmdLineArg.Specification =
                { CmdLineArgSpec.Name = "quiet"
                  Parameters = []
                  Aliases = []
                  Description = "suppress any non-essential output" }
            Process = fun _ opts -> { opts with Quiet = true } }

          { CmdLineArg.Specification =
                { CmdLineArgSpec.Name = "noprimers"
                  Parameters = []
                  Aliases = []
                  Description = "do not attempt to generate primers" }
            Process = fun _ opts -> { opts with NoPrimers = true } }

          libCmdArg

          { CmdLineArg.Specification =
                { CmdLineArgSpec.Name = "serial"
                  Parameters = []
                  Aliases = []
                  Description = "don't run parallel operations, useful for debugging" }
            Process = fun _ opts -> { opts with DoParallel = false } }

          { CmdLineArg.Specification =
                { CmdLineArgSpec.Name = "lextest"
                  Parameters = []
                  Aliases = [ "tokentest"; "tokenize" ]
                  Description = "for debugging only, show stream of parsed tokens from input file" }
            Process = fun _ opts -> { opts with LexOnly = true } }

          { CmdLineArg.Specification =
                { CmdLineArgSpec.Name = "only_phase1"
                  Parameters = []
                  Aliases = []
                  Description = "expand GSL just through the phase 1 pipeline, and emit intermediate GSL" }
            Process = fun _ opts -> { opts with OnlyPhase1 = true } }

          { CmdLineArg.Specification =
                { CmdLineArgSpec.Name = "plugins"
                  Parameters = []
                  Aliases = []
                  Description = "List all plugins installed in this build of the compiler." }
            Process = fun _ opts -> { opts with ListPlugins = true } }

          deterministicCmdArg ]
        |> Seq.map (fun a ->
            seq {
                yield (a.Specification.Name, a)
                for alias in a.Specification.Aliases -> (alias, a)
            })
        |> Seq.concat
        |> Map.ofSeq



    /// Format a command line argument as a sequence of strings.
    let printCmdLineArg a =
        let padSize = 35

        let p =
            Seq.map (sprintf " <%s>") a.Parameters
            |> Seq.fold (+) ""

        let larg = sprintf "       --%s%s" a.Name p

        let rPad =
            String.replicate (padSize - larg.Length) " "

        let descLines = a.Description.Split [| '\n' |]
        let firstLine = larg + rPad + "-" + descLines.[0]

        let formatOtherLine l = (String.replicate (padSize + 1) " ") + l

        let otherLines =
            descLines.[1..] |> Array.map formatOtherLine

        seq {
            yield firstLine
            for l in otherLines -> l

            if not a.Aliases.IsEmpty then
                let aliases =
                    a.Aliases
                    |> List.map (sprintf "--%s")
                    |> String.concat ", "

                yield
                    (sprintf "(aliases: %s)" aliases)
                    |> formatOtherLine
        }

    /// Format arg usage and help text.
    let usageText (args: CollectedCommandLineArgs) =
        let argLines =
            args.Specs
            |> Set.ofSeq
            |> Set.toList
            |> List.sortBy (fun s -> s.Name)
            |> List.map printCmdLineArg
            |> Seq.concat

        Seq.append [ "Usage:  gscl [args] input.gsl" ] argLines
        |> String.concat "\n"

    let defaultOpts: ParsedOptions =
        { Quiet = false
          LibDir = libRoot
          RefStrain = "cenpk"
          Iter = true
          OnlyPhase1 = false
          DoParallel = true
          Verbose = false
          NoPrimers = false
          LexOnly = false
          RefList = false
          RefDump = None
          ListPlugins = false
          DoHelpPragmas = false
          IsDeterministic = false }
