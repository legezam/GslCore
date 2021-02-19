﻿/// Assembly transforming plugin that implements seamless part assembly.
module GslCore.Plugin.SeamlessPlugin

open System
open GslCore.Core.Types
open GslCore.Core.CommandConfig
open GslCore.Core.PluginTypes
open GslCore.Constants
open Amyris.ErrorHandling
open GslCore.GslcProcess
open GslCore.Pragma
open GslCore.Uri

let mt =
    let mtLinkerUri = Uri.linkerUri "MT" |> returnOrFail
    { Id = None
      ExternalId = None
      SliceName = ""
      Uri = Some mtLinkerUri
      Dna = Amyris.Dna.Dna("")
      SourceChromosome = "linker"
      SourceFrom = 0<ZeroOffset>
      SourceTo = 0<ZeroOffset>
      Template = None
      IsAmplified = false
      SourceFromApprox = false
      SourceToApprox = false
      DestinationFrom = -999<ZeroOffset>
      DestinationTo = -999<ZeroOffset>
      SourceForward = true
      Description = sprintf "Linker_MT"
      Type = SliceType.Linker
      DestinationForward = true
      DnaSource = ""
      Pragmas = PragmaCollection.empty
      Breed = Breed.Linker
      MaterializedFrom = None
      Annotations = [] }

let mtRev = { mt with SourceForward = false }

let dumpSliceLayout (slices: DNASlice list) =
    String.Join(";", slices |> List.map (fun s -> SliceType.toString s.Type))

/// Insert FUSE directives and MT linkers to get a seamless design from later primergen
let procInsertFuse verbose (l: DNASlice list) =
    if verbose then
        printfn "Entering procInsertFuse"
        let names = l |> List.map (fun l -> l.SliceName)
        printfn "Slices presented: %s" (String.Join(";", names))

    let rec procInsertFuseInternal (l: DNASlice list) res =
        if verbose
        then printfn "placeFuseForSeamless: top l=%s" (dumpSliceLayout l)

        let finish parts = mt :: (List.rev parts) @ [ mtRev ]

        match l with
        | [] -> finish l
        | [ x ] -> finish (x :: res) // don't fuse after last piece
        | hd :: tl when hd.Type = SliceType.Fusion ->
            if verbose
            then printfn "placeFuseForSeamless: .. skipping existing fuse"
            // pass existing fuse elements straight through
            procInsertFuseInternal tl (hd :: res)
        | hd :: tl when hd.Type = SliceType.Inline ->
            // inline segments should get primer gen anyway I think
            if verbose
            then printfn "placeFuseForSeamless: .. ignoring inline"

            procInsertFuseInternal tl (hd :: res)

        | hd :: middle :: tl when hd.Type = SliceType.Fusion
                                  && middle.Type = SliceType.Inline ->
            // omit hd, not needed
            procInsertFuseInternal tl (middle :: res)

        | hd :: middle :: tl when hd.Type = SliceType.Regular
                                  && middle.Type = SliceType.Inline ->
            procInsertFuseInternal tl (middle :: hd :: res)

        | hd :: tl ->
            if verbose
            then printfn "placeFuseForSeamless: .. general case gets fuse"

            procInsertFuseInternal tl (DnaAssembly.fusionSliceConstant :: hd :: res)

        | hd :: tl -> procInsertFuseInternal tl (hd :: res)

    procInsertFuseInternal l []

/// strategic place fuse directives into linker free constructs to effectively
/// request a seamless design
let placeFuseForSeamless (at: ATContext) (a: DnaAssembly) =
    let printVerbose message =
        if at.Options.Verbose then printfn "%s" message


    let linkered =
        a.DnaParts
        |> List.exists (fun d -> d.Type = SliceType.Linker)

    if linkered then
        printVerbose "placeFuseForSeamless: skipping, since linkers present"
        ok a // we don't touch cases where linkers are already placed
    else
        printVerbose "placeFuseForSeamless: examining need for fuse slices"
        printVerbose (sprintf "placeFuseForSeamless: starting layout: %s" (dumpSliceLayout a.DnaParts))

        // flank final result with empty linkers to get end primer generation
        let dnaPartsProcessed =
            procInsertFuse at.Options.Verbose a.DnaParts

        printVerbose (sprintf "placeFuseForSeamless: final layout: %s" (dumpSliceLayout dnaPartsProcessed))

        ok { a with DnaParts = dnaPartsProcessed }

let seamlessArg =
    { name = "seamless"
      param = [ "(true or false)" ]
      alias = []
      desc = "Perform seamless assembly." }


type SeamlessAssembler =
    { run: bool
      /// Optionally attach a function to this plugin behavior to permit its operation to be
      /// configured by command line arguments injected by other plugins.  This is necessary because
      /// seamless assembly can alter a lot of expectations of downstream processing steps.
      processExtraArgs: ParsedCmdLineArg -> SeamlessAssembler -> SeamlessAssembler }
    interface IAssemblyTransform with
        member x.ProvidedArgs() = [ seamlessArg ]

        member x.Configure(arg) =
            if arg.spec = seamlessArg then
                let run =
                    match arg.values with
                    | [ "true" ] -> true
                    | [ "false" ] -> false
                    | [ x ] -> failwithf "Invalid argument for seamless: '%s'. Options are 'true' or 'false'." x
                    | _ -> failwithf "Seamless plugin received the wrong number of command line arguments."

                { x with run = run }
            else
                x
            |> x.processExtraArgs arg :> IAssemblyTransform

        member x.ConfigureFromOptions(opts) =
            if opts.NoPrimers then { x with run = false } :> IAssemblyTransform else x :> IAssemblyTransform

        member x.TransformAssembly context assembly =
            if x.run then
                placeFuseForSeamless context assembly
                // Run existing fuse processor to clean up fuses between certain part combinations.
                // Is this necessary or does the seamless logic already account for this?
                >>= preProcessFuse context
            else
                ok assembly

/// Produce an instance of the seamless assembly plugin with the provided extra argument processor.
let createSeamlessPlugin defaultRun extraArgProcessor =
    { Name = "seamless_assembly"
      Description = Some "Perform seamless assembly by liberally fusing slices."
      Behaviors =
          [ { Name = None
              Description = None
              Behavior =
                  AssemblyTransform
                      ({ run = defaultRun
                         processExtraArgs = extraArgProcessor }) } ]
      ProvidesPragmas = []
      ProvidesCapas = [] }

/// By default do not take any other command line args into account, and always run in seamless mode.
let seamlessPlugin = createSeamlessPlugin true (fun _ x -> x)
