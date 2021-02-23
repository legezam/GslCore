/// Code for flat file output format
module GslCore.Plugin.CoreOutput.DumpFlat

open System
open System.IO
open Amyris.Bio.utils
open GslCore
open GslCore.Core.Types

/// Text representation of the assemblies to stdout
/// dumpFlat file format
let dumpFlat (outFile: string) (assembliesIn: DnaAssembly list) =
    if outFile <> "-"
    then printfn "Writing flat format to %s" outFile

    let outF =
        if outFile = "-" then None else Some(new StreamWriter(outFile))

    /// Assign ids to any linkers present
    let assignLinkPartNumbers (al: DnaAssembly list) =
        // get linker list
        let unlabeled =
            seq {
                for a in al do
                    for d in a.DnaParts do
                        match d.Id with
                        | Some _ -> ()
                        | None -> yield d.Description
            }
            |> Set.ofSeq

        let map =
            Seq.zip unlabeled { 100000 .. 99999 + unlabeled.Count }
            |> Map.ofSeq

        al
        |> List.map (fun a ->
            { a with
                  DnaParts =
                      a.DnaParts
                      |> List.map (fun d ->
                          match d.Id with
                          | Some _ -> d
                          | None ->
                              { d with
                                    Id = Some(map.[d.Description]) }) })

    let assemblies = assignLinkPartNumbers assembliesIn

    let w (s: string) =
        match outF with
        | None -> stdout.WriteLine(s)
        | Some (x) -> x.WriteLine(s)

    for a in assemblies do
        let aId = Utils.ambId a.Id
        sprintf "##### Assembly %s #######" aId |> w
        sprintf "A# %s" aId |> w
        sprintf "NA %s" a.Name |> w

        if not a.Tags.IsEmpty then
            sprintf "TA %s" (String.Join(" ", [ for tag in a.Tags -> sprintf "%s:%s" tag.Namespace tag.Tag ]))
            |> w

        match a.Uri with
        | Some (u) -> sprintf "NU %s" u |> w
        | None -> ()

        sprintf "NP %d" (a.DnaParts.Length) |> w
        sprintf "AS %s" (a.Sequence().str) |> w
        sprintf "" |> w

        for p in a.DnaParts do
            sprintf "P# %s" (Utils.ambId p.Id) |> w
            if p.SliceName <> "" then sprintf "SN %s" p.SliceName |> w

            match p.Uri with
            | Some (u) -> sprintf "NU %s" u |> w
            | None -> ()

            sprintf "DE %s" p.Description |> w
            sprintf "ST %s" (SliceType.toString p.Type) |> w
            sprintf "PA %s" aId |> w
            sprintf "DS %s" p.DnaSource |> w
            sprintf "CH %s" p.SourceChromosome |> w
            sprintf "LT %A" p.SourceFrom |> w
            sprintf "RT %A" p.SourceTo |> w
            sprintf "LE %d" p.Dna.Length |> w

            sprintf "FW %s" (if p.SourceForward then "+" else "-")
            |> w

            sprintf "SQ\n%s\n//\n" (format60 p.Dna.arr) |> w

        sprintf "" |> w

    // Now emit a summary of the assembly parts
    sprintf "######### Assembly summary ##############"
    |> w

    for a in assemblies do
        let s = sprintf "AI %s -> " (Utils.ambId a.Id)

        match outF with
        | None -> stdout.Write(s)
        | Some (x) -> x.Write(s)

        sprintf
            "%s"
            (a.DnaParts
             |> List.map (fun p -> Utils.ambId p.Id)
             |> Array.ofList
             |> (fun x -> String.Join(",", x)))
        |> w

    match outF with
    | None -> ()
    | Some (f) -> f.Close()
