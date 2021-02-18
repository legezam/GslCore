﻿/// Command line arguments, parsing, and command defaults.
module GslCore.CommandConfig

open System
open GslCore.Core.Types
open Amyris.Bio.utils

let informalVersion =
    AssemblyVersionInformation.AssemblyInformationalVersion // git hash

let version =
    AssemblyVersionInformation.AssemblyVersion

let libRoot =
    match Environment.GetEnvironmentVariable("GSL_LIB") with
    | null -> "gslc_lib"
    | x -> x
    |> smashSlash

/// Base type for a command line argument specification.
type CmdLineArgSpec =
    { name: string
      param: string list
      alias: string list
      desc: string }

/// Combination of a command line arg defintion and a behavior to update some options
/// structure based on passed arguments.
type CmdLineArg<'cfg> =
    { spec: CmdLineArgSpec
      proc: string list -> 'cfg -> 'cfg }

type BuiltinCmdLineArg = CmdLineArg<ParsedOptions>

type CollectedCommandLineArgs =
    { builtins: Map<string, CmdLineArgSpec>
      builtinsWithProc: Map<string, BuiltinCmdLineArg>
      fromPlugins: Map<string, CmdLineArgSpec> }
    member x.Specs =
        let builtinSpecs = x.builtins |> Map.toSeq |> Seq.map snd

        let fromPlugins =
            x.fromPlugins |> Map.toSeq |> Seq.map snd

        Seq.append builtinSpecs fromPlugins

    member x.TryFind(name) =
        match x.builtins.TryFind(name) with
        | Some (a) -> Some(a)
        | None -> x.fromPlugins.TryFind(name)


/// Literal command line argument parsed from input.
type ParsedCmdLineArg =
    { spec: CmdLineArgSpec
      values: string list }

let libCmdArg =
    { spec =
          { name = "lib"
            param = [ "directory" ]
            alias = []
            desc = "directory in which genome definitions reside\nDefault: GSL_LIB var, or 'lib' in current directory" }
      proc = fun p opts -> { opts with LibDir = smashSlash p.[0] } }

let deterministicCmdArg =
    let processParameters (_parameters: string list) (parsedOptions: ParsedOptions): ParsedOptions =
        { parsedOptions with
              IsDeterministic = true }

    { CmdLineArg.spec =
          { CmdLineArgSpec.name = "deterministic"
            param = []
            alias = []
            desc = "Produce deterministic output (= if rerun with same input then will produce identical output)" }
      proc = processParameters }

/// Define all GSLC command line arguments here.
/// An argument consists of its name, the names of its parameters, a description,
/// and a function that takes a list of passed parameters and an options record
/// and returns a modified options record.
let builtinCmdLineArgs =
    [ { spec =
            { name = "reflist"
              param = []
              alias = []
              desc = "list available reference genomes" }
        proc = fun _ opts -> { opts with RefList = true } }

      { spec =
            { name = "refdump"
              param = [ "refname" ]
              alias = []
              desc = "dump available loci in reference genome" }
        proc = fun p opts -> { opts with RefDump = Some(p.[0]) } }

      { spec =
            { name = "step"
              param = []
              alias = []
              desc = "expand GSL just one round, and emit intermediate GSL" }
        proc = fun _ opts -> { opts with Iter = false } }

      { spec =
            { name = "verbose"
              param = []
              alias = []
              desc = "print debugging info" }
        proc = fun _ opts -> { opts with Verbose = true } }

      { spec =
            { name = "version"
              param = []
              alias = []
              desc = "print version information" }
        proc =
            fun _ opts ->
                printfn "GSL core compiler version %s (%s)" version informalVersion
                opts }

      { spec =
            { name = "helpPragmas"
              param = []
              alias = []
              desc = "print available pragmas" }
        proc = fun _ opts -> { opts with DoHelpPragmas = true } }

      { spec =
            { name = "quiet"
              param = []
              alias = []
              desc = "suppress any non-essential output" }
        proc = fun _ opts -> { opts with Quiet = true } }

      { spec =
            { name = "noprimers"
              param = []
              alias = []
              desc = "do not attempt to generate primers" }
        proc = fun _ opts -> { opts with NoPrimers = true } }

      libCmdArg

      { spec =
            { name = "serial"
              param = []
              alias = []
              desc = "don't run parallel operations, useful for debugging" }
        proc = fun _ opts -> { opts with DoParallel = false } }

      { spec =
            { name = "lextest"
              param = []
              alias = [ "tokentest"; "tokenize" ]
              desc = "for debugging only, show stream of parsed tokens from input file" }
        proc = fun _ opts -> { opts with LexOnly = true } }

      { spec =
            { name = "only_phase1"
              param = []
              alias = []
              desc = "expand GSL just through the phase 1 pipeline, and emit intermediate GSL" }
        proc = fun _ opts -> { opts with OnlyPhase1 = true } }

      { spec =
            { name = "plugins"
              param = []
              alias = []
              desc = "List all plugins installed in this build of the compiler." }
        proc = fun _ opts -> { opts with ListPlugins = true } }

      deterministicCmdArg ]
    |> Seq.map (fun a ->
        seq {
            yield (a.spec.name, a)
            for alias in a.spec.alias -> (alias, a)
        })
    |> Seq.concat
    |> Map.ofSeq



/// Format a command line argument as a sequence of strings.
let printCmdLineArg a =
    let padSize = 35

    let p =
        Seq.map (sprintf " <%s>") a.param
        |> Seq.fold (+) ""

    let larg = sprintf "       --%s%s" a.name p

    let rPad =
        String.replicate (padSize - larg.Length) " "

    let descLines = a.desc.Split [| '\n' |]
    let firstLine = larg + rPad + "-" + descLines.[0]

    let formatOtherLine l = (String.replicate (padSize + 1) " ") + l

    let otherLines =
        descLines.[1..] |> Array.map formatOtherLine

    seq {
        yield firstLine
        for l in otherLines -> l

        if not a.alias.IsEmpty then
            let aliases =
                a.alias
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
        |> List.sortBy (fun s -> s.name)
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
