module GslCore.Plugin.JsonAssembly

open System.IO
open System
open GslCore.Core.Types
open GslCore.Plugin.CoreOutput.Provider
open Newtonsoft.Json
open GslCore.Core.PluginTypes

/// A few key fields of a DNA slice for JSON representation.
type DNASliceJson =
    { id: string
      extId: string
      dna: string
      sourceChr: string
      sourceFr: string
      sourceTo: string
      sourceFwd: bool
      destFr: string
      destTo: string
      destFwd: bool
      amplified: bool
      sliceName: string
      sliceType: string
      breed: string
      description: string }

/// A few key fields of Assembly output for JSON representation.
type AssemblyOutJson =
    { id: string
      name: string
      dnaSlices: DNASliceJson list }

let formatST (s: SliceType) =
    match s with
    | SliceType.Regular -> "REGULAR"
    | SliceType.Marker -> "MARKER"
    | SliceType.Linker -> "LINKER"
    | SliceType.Inline -> "INLINE"
    | SliceType.Fusion -> "FUSION"

let formatBreed (b: Breed) =
    match b with
    | Breed.Promoter -> "B_PROMOTER"
    | Breed.Terminator -> "B_TERMINATOR"
    | Breed.Marker -> "B_MARKER"
    | Breed.FusableOrf -> "B_FUSABLEORF"
    | Breed.Upstream -> "B_UPSTREAM"
    | Breed.Downstream -> "B_DOWNSTREAM"
    | Breed.GST -> "B_GST"
    | Breed.GS -> "B_GS"
    | Breed.Inline -> "B_INLINE"
    | Breed.X -> "B_X"
    | Breed.Virtual -> "B_VIRTUAL"
    | Breed.Linker -> "B_LINKER"

///  Write out a JSON file representing the output assembly list to a given path.
let dumpJsonAssemblies (outFile: string) (assemblies: DnaAssembly list) =

    use outF = new StreamWriter(outFile)

    let assemblyHash =
        assemblies
        |> List.map (fun a ->
            { id = a.Id.Value.ToString()
              name = a.Name.ToString()
              dnaSlices =
                  (a.DnaParts
                   |> List.map (fun d ->
                       { id =
                             (match d.Id with
                              | None -> "0"
                              | Some id -> id.ToString())
                         extId = ""
                         dna = String.Join("", d.Dna)
                         sourceChr = d.SourceChromosome
                         sourceFr = d.SourceFrom.ToString()
                         sourceTo = d.SourceTo.ToString()
                         sourceFwd = d.SourceForward
                         destFr = d.DestinationFrom.ToString()
                         destTo = d.DestinationTo.ToString()
                         destFwd = d.DestinationForward
                         amplified = d.IsAmplified
                         sliceName = d.SliceName.ToString()
                         sliceType = (formatST d.Type)
                         breed = (formatBreed d.Breed)
                         description = d.Description.ToString() })) })

    let jsonFileString: string =
        Newtonsoft.Json.JsonConvert.SerializeObject(assemblyHash, Formatting.Indented)

    outF.WriteLine(jsonFileString)

type AutodeskJsonOutputProvider(outPath: (string) option) =
    inherit ConfigurableOutputProvider<string>(outPath)
    with
        override x.ArgSpec =
            { Name = "json"
              Parameters = [ "outfile" ]
              Aliases = []
              Description = "write a json file format for the assembly results" }

        override x.UseArg(arg) =
            AutodeskJsonOutputProvider(Some(arg.Values.[0])) :> IOutputFormat

        override x.DoOutput(path, data) = dumpJsonAssemblies path data.Assemblies

let autodeskJsonOutputPlugin =
    outputPlugin
        "autodesk_json"
        (Some "Autodesk json output file format provider, for connecting to the Autodesk Genetic Constructor.")
        (AutodeskJsonOutputProvider(None))
