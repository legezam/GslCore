module GslCore.Plugin.CoreOutput.Ape

open System.IO
open System
open GslCore.Core.Types
open GslCore.Constants
open Amyris.Bio.utils
open GslCore
open GslCore.Pragma
open GslCore.Plugin.CoreOutput.Genbank

/// Emit APE (genbank) format
///  outDir : string   tag: string  prefix for files  assemblies : List of AssemblyOut
let dumpAPE (outDir: string) (tag: string) (assemblies: DnaAssembly list) =
    for a in assemblies do
        let path =
            sprintf
                "%s.%d.ape"
                tag
                (match a.Id with
                 | None -> failwith "ERROR: unassigned assembly id"
                 | Some (i) -> i)
            |> opj outDir

        printf "Writing ape output to dir=%s tag=%s path=%s\n" outDir tag path
        use outF = new StreamWriter(path)
        let w (s: string) = outF.Write(s)

        let locusName = sprintf "%s_ape_output" tag

        let totLength =
            a.DnaParts
            |> List.map (fun p -> p.Dna.Length)
            |> Seq.sum

        let now = DateTime.Now
        let topology = a.Topology |> Topology.toString

        sprintf "LOCUS                 %15s%5d bp ds-DNA   %s       %2d-%s-%d
DEFINITION  .
ACCESSION
VERSION
SOURCE      .
  ORGANISM  .
COMMENT
COMMENT     ApEinfo:methylated:1
FEATURES             Location/Qualifiers
"        locusName totLength topology now.Day (mon.[now.Month - 1]) now.Year
        |> w

        for p in a.DnaParts do
            let colorFwd =
                match p.Type with
                | SliceType.Regular -> "#0000FF"
                | SliceType.Linker -> "#FF0000"
                | SliceType.Marker -> "yellow"
                | SliceType.Inline -> "green"
                | SliceType.Fusion -> "red"

            let colorRev =
                match p.Type with
                | SliceType.Regular -> "#0000F0"
                | SliceType.Linker -> "#D00000"
                | SliceType.Marker -> "yellow"
                | SliceType.Inline -> "green"
                | SliceType.Fusion -> "red"

            let range =
                sprintf (if p.DestinationForward then "%A..%A" else "complement(%A..%A)") (ZeroOffset.toOne p.DestinationFrom) (ZeroOffset.toOne p.DestinationTo)

            sprintf "     misc_feature    %s
                     /label=%s
                     /ApEinfo_fwdcolor=%s
                     /ApEinfo_revcolor=%s\n" range
                (if p.SliceName <> "" then p.SliceName
                 else if p.Description <> "" then p.Description
                 else (Utils.ambId p.Id)) colorFwd colorRev
            |> w

        sprintf "ORIGIN\n" |> w

        a.Sequence() |> formatGB |> w
