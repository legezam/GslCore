﻿module GslCore.Plugin.CoreOutput.Provider

open GslCore.Core.PluginTypes
open GslCore.Core.CommandConfig
open GslCore.Plugin.CoreOutput
open GslCore.Plugin.CoreOutput.CloneManager
open GslCore.Plugin.CoreOutput.Ape
open GslCore.Plugin.CoreOutput.Snapgene
open GslCore.Plugin.CoreOutput.DumpFlat
open GslCore.Core.Types
open System.IO

// =============================
// standard output format providers
// =============================

/// Basic output provider, switched using a single command line parameter.
/// Boilerplate removal base class for basic, configurationless output generation.
[<AbstractClass>]
type ConfigurableOutputProvider<'T>(param: 'T option) =
    abstract ArgSpec: CmdLineArgSpec
    abstract UseArg: ParsedCmdLineArg -> IOutputFormat
    abstract DoOutput: 'T * OutputGenerationData -> unit

    interface IOutputFormat with
        member x.ProvidedArgs() = [ x.ArgSpec ]

        member x.Configure(parsedArg) =
            if parsedArg.Specification = x.ArgSpec then x.UseArg(parsedArg) else x :> IOutputFormat

        member x.ConfigureFromOptions(_) = x :> IOutputFormat

        member x.ProduceOutput(data) =
            match param with
            | Some (p) -> x.DoOutput(p, data)
            | None -> ()

/// Create a basic plug and play output plugin.
let outputPlugin name desc provider =
    { Name = name
      Description = desc
      Behaviors =
          [ { Name = None
              Description = None
              Behavior = OutputFormat(provider) } ]
      ProvidesPragmas = []
      ProvidesCapas = [] }

type GslFlatFileOutputProvider(outPath) =
    inherit ConfigurableOutputProvider<string>(outPath)
    with
        override x.ArgSpec =
            { Name = "flat"
              Parameters = [ "outfile" ]
              Aliases = []
              Description = "write a flat file format for results to outputfile" }

        override x.UseArg(arg) =
            GslFlatFileOutputProvider(Some(arg.Values.[0])) :> IOutputFormat

        override x.DoOutput(path, data) = dumpFlat path data.Assemblies

let flatFileOutputPlugin =
    outputPlugin "flat_file" (Some "GSL flat file output provider.") (GslFlatFileOutputProvider(None))


type CloneManagerOutputProvider(outParams) =
    inherit ConfigurableOutputProvider<(string * string)>(outParams)
    with
        override x.ArgSpec =
            { Name = "cm"
              Parameters = [ "outDir"; "prefix" ]
              Aliases = []
              Description = "write clone manager output to prefix_##.cx5 to output directory outDir" }

        override x.UseArg(arg) =
            CloneManagerOutputProvider(Some(arg.Values.[0], arg.Values.[1])) :> IOutputFormat

        override x.DoOutput((path, tag), data) =
            dumpCM path tag data.Assemblies data.Primers

let cloneManagerOutputPlugin =
    outputPlugin "clone_manager" (Some "Clone Manager output format provider.") (CloneManagerOutputProvider(None))

type ApeOutputProvider(outParams) =
    inherit ConfigurableOutputProvider<(string * string)>(outParams)
    with
        override x.ArgSpec =
            { Name = "ape"
              Parameters = [ "outDir"; "prefix" ]
              Aliases = []
              Description = "write APE output to prefix_##.ape to outDir\n(http://biologylabs.utah.edu/jorgensen/wayned/ape/)" }

        override x.UseArg(arg) =
            ApeOutputProvider(Some(arg.Values.[0], arg.Values.[1])) :> IOutputFormat

        override x.DoOutput((path, tag), data) = dumpAPE path tag data.Assemblies

let apeOutputPlugin =
    outputPlugin "APE" (Some "APE (A Plasmid Editor) output format provider.") (ApeOutputProvider(None))


type SnapGeneOutputProvider(outParams) =
    inherit ConfigurableOutputProvider<(string * string)>(outParams)
    with
        override x.ArgSpec =
            { Name = "snapgene"
              Parameters = [ "outDir"; "prefix" ]
              Aliases = []
              Description = "write Snapgene output to prefix_##.dna to outDir\n(http://www.snapgene.com/products/snapgene_viewer/)" }

        override x.UseArg(arg) =
            SnapGeneOutputProvider(Some(arg.Values.[0], arg.Values.[1])) :> IOutputFormat

        override x.DoOutput((path, tag), data) =
            dumpSnapgene path tag data.Assemblies data.Primers

let snapGeneOutputPlugin =
    outputPlugin "Snapgene" (Some "Snapgene output format provider.") (SnapGeneOutputProvider(None))

/// Create output file with user or algorithm documentation of the designs
let private dumpDocStrings (path: string) (assemblies: DnaAssembly list) =
    use outF = new StreamWriter(path)

    for a in assemblies do
        outF.WriteLine(sprintf "@name=%s" a.Name)

        for line in a.DocStrings do
            outF.WriteLine(line)

        outF.WriteLine("")

type DocstringOutputProvider(outPath) =
    inherit ConfigurableOutputProvider<string>(outPath)
    with
        override x.ArgSpec =
            { Name = "docstring"
              Parameters = [ "outfile" ]
              Aliases = [ "docstrings" ]
              Description = "log emitted documentation for each design to outfile" }

        override x.UseArg(arg) =
            DocstringOutputProvider(Some(arg.Values.[0])) :> IOutputFormat

        override x.DoOutput(path, data) = dumpDocStrings path data.Assemblies

let docstringOutputPlugin =
    outputPlugin
        "docstring_file"
        (Some "GSL docstring output format provider.  Enables dumping of assembly docstrings.")
        (DocstringOutputProvider(None))


type PrimerOutputProvider(outPath) =
    inherit ConfigurableOutputProvider<string>(outPath)
    with
        override x.ArgSpec =
            { Name = "primers"
              Parameters = [ "primerfile" ]
              Aliases = []
              Description = "emit raw primer details (see also thumper output format)" }

        override x.UseArg(arg) =
            PrimerOutputProvider(Some(arg.Values.[0])) :> IOutputFormat

        override x.DoOutput(path, data) =
            match data.Primers with
            | Some (primers) -> PrimerDump.simplePrimerDump path primers data.Assemblies
            | None -> failwithf "--primers was selected but no primers were produced.  Did you also pass --noprimers?"

let primerOutputPlugin =
    outputPlugin
        "primer_file"
        (Some "Primer description file format output provider.  Enables dumping of assembly primer data.")
        (PrimerOutputProvider(None))


let basicOutputPlugins =
    [ flatFileOutputPlugin
      cloneManagerOutputPlugin
      apeOutputPlugin
      snapGeneOutputPlugin
      docstringOutputPlugin
      primerOutputPlugin ]