﻿module GslCore.Gslc

open System.IO
open System
open System.Text
open System.Reflection
open GslCore.Pragma
open GslCore.Reference
open Microsoft.FSharp.Core.Printf

open GslCore.Core.Types
open GslCore.Core.CommandConfig
open GslCore.Ast
open GslCore.GslcProcess // Top-level compiler operations
open GslCore.Ast.Algorithms
open GslCore.Ast.ErrorHandling
// Helper libs for oligo design, sequence parsing all in Amyris.Bio.dll
// These imports are only needed for the temporary primer test function below.
open Amyris.Bio
open primercore
open GslCore.Constants
open GslCore.Core.PluginTypes
open GslCore.Ast.Types
open ProcessCmdLineArgs

/// Test bed for investigating primer misadventure
let testPrimer () =
    let template =
        "GCCAGCGATAGGAGTCCTTGGTTTAG".ToCharArray()

    for i in 15 .. 26 do
        printfn "%d %A" i (Amyris.Bio.primercore.temp defaultParams template i)

    let fwd = true

    let pen =
        { primercore.defaultParams with
              maxLength = 30
              tmPenalty = 3.0 (* template.Length *)  }

    let task: OligoTask =
        { tag = if fwd then "PF" else "PR"
          temp = template
          align = ANCHOR.LEFT
          strand = STRAND.TOP
          offset = 0
          targetTemp = Default.RyseLinkerTargetTemp
          sequencePenalties = None }

    let res = oligoDesign true pen task
    printf "pen=%A \n %A %s %d\n" pen (res.Value.temp) (utils.arr2seq res.Value.oligo) res.Value.oligo.Length

    ()

/// Helper type to allow functions to indicate whether compiler execution should continue or exit
/// with an exit code and optional message.
type FlowControl<'T> =
    | Continue of 'T
    | Exit of int * string option

/// Return the top-level exception message, as well as any messages from nested inner exceptions.
let fullExceptionMessage (e: Exception) =
    let sb = StringBuilder()
    let nl = Environment.NewLine

    let rec printException (e: Exception) count =
        if (e :? TargetException && e.InnerException <> null) then
            printException (e.InnerException) count
        else
            if (count = 1)
            then bprintf sb "%s%s" e.Message nl
            else bprintf sb "%d: %s%s" (count - 1) e.Message nl

            if (e.InnerException <> null)
            then printException e.InnerException (count + 1)

    printException e 1
    sb.ToString()

let basicExceptionHandler verbose f =
    try
        f ()
    with e ->
        // Some stages don't capture their own exceptions yet.
        // For now, do this simple printing to stay consistent with legacy expectations.
        let prependERROR (s: string) =
            if s.StartsWith("ERROR") then s else sprintf "ERROR: %s" s

        let msg =
            if verbose then prettyPrintException e else prependERROR (fullExceptionMessage e)

        Exit(1, Some(msg))

let maybeListRefGenomes (s: ConfigurationState) =
    // generate list of reference genomes and quit
    if s.Options.RefList then
        for f in enumerateLibs s.Options do
            printfn "refgenome\t%s" f

        Exit(0, None)
    else
        Continue(s)

let maybeDumpLoci (s: ConfigurationState) =
    // dump available loci for one reference genome if requested
    match s.Options.RefDump with
    | None -> Continue(s)
    | Some ref ->
        // dump available loci for this ref genome
        let p = utils.opj s.Options.LibDir ref

        if not (Directory.Exists(p)) then
            Exit(1, Some(sprintf "ERROR: unable to find genome reference dir %s\n" p))
        else
            let gd = GenomeDefinition.createEager s.Options.LibDir ref

            for f in gd |> GenomeDefinition.getAllFeatures do
                printfn "%s\t%s\t%s" f.sysName f.gene f.description

            Continue(s)

let checkInputFileList (s: ConfigurationState) =
    // For the moment we only support one file argument.
    match s.Files with
    | [] -> Exit(1, Some "no input files specified")
    | [ inputFile ] ->
        if not (File.Exists inputFile)
        then Exit(1, Some(sprintf "can't find file '%s'\n" inputFile))
        else Continue(s)
    | n -> Exit(1, Some(sprintf "ERROR: GSLC only supports one input file at a time. Got %A" n))


let maybeJustDoLexing (s: ConfigurationState) =
    // If selected, perform lexing and quit.
    if s.Options.LexOnly then
        let inputText = File.ReadAllText s.InputFile
        LexAndParse.lexTest s.Options.Verbose inputText
        Exit(0, None)
    else
        Continue(s)

/// Configure compiler and plugins from command line arguments.
let configureGslc unconfiguredPlugins argv =

    // collect up the command line arguments from all plugins
    let legalCmdLineArgs =
        collectCommandLineArgs unconfiguredPlugins

    // Get the input args and handle a few special cases
    match (argv |> List.ofArray (*|> List.tail*) ) with
    // print usage info
    | []
    | "--help" :: _ ->
        let msg = CommandLine.usageText legalCmdLineArgs
        Exit(1, Some(msg))
    // temporary entry point for primer test
    | "--test" :: _ ->
        testPrimer () |> ignore
        Exit(1, None)
    // Configure GSLc and plugins from command line arguments
    | args ->
        // need to trap an exception here or convert command line parsing to ROP,
        // otherwise exception bubbles all the way up.
        try
            let s =
                configure true legalCmdLineArgs unconfiguredPlugins args

            if not s.Options.Quiet
               && not s.Options.RefList
               && s.Options.RefDump.IsNone then
                printf "// GSL core compiler version %s (%s)\n" version informalVersion

            if s.Options.ListPlugins then
                let pluginDescs =
                    s.Plugins
                    |> List.map (fun p -> p.Info)
                    |> String.concat "\n\n"

                printfn "Installed plugins:\n%s" pluginDescs


            // fulfill this request only after we've processed all the plugins and determined full list of pragmas
            if s.Options.DoHelpPragmas then PragmaBuilder.printPragmaUsage s.GlobalAssets.PragmaBuilder

            Continue(s)
        with e -> Exit(1, Some(sprintf "An error occurred during configuration:\n%s" e.Message))

let runCompiler (s: ConfigurationState) =

    // Start with input from the users supplied file
    let input =
        File.ReadAllText s.InputFile |> GslSourceCode

    // Run GSLC on the input file.
    // No exception handler necessary here as processGSL never raises an exception.
    let compileResult = processGSL s input

    Continue(compileResult, input, s)


let handleCompileResult (result, input: GslSourceCode, s) =
    match result with
    | Ok ((assemblies, tree: AstTreeHead), warnings) ->
        // print any warnings from compilation
        for w in GslParseErrorContext.deduplicateMessages warnings do
            printfn "%s\n" w.Summary
        // if we just want one expansion step, reprint expanded source and done
        if not s.Options.Iter || s.Options.OnlyPhase1
        then Exit(0, Some(AstNode.decompile tree.wrappedNode))
        // do output generation
        else Continue(assemblies, input, s)
    | Bad (errors) ->
        // convert messages into strings for printing
        let msgs =
            [ for msg in GslParseErrorContext.deduplicateMessages errors -> msg |> AstMessage.getLongForm (s.Options.Verbose, input) ]

        Exit(1, Some(msgs |> String.concat "\n\n"))

let doDnaMaterialization (assemblies, input, s) =
    Continue(materializeDna s assemblies, input, s)

let doAssemblyTransform (assemblies, input, s) =
    Continue(transformAssemblies s assemblies, input, s)

let handleTransformResult phase (r, input, s) =
    match r with
    | Ok (transformedAssemblies, warnings: AssemblyTransformationMessage<_> list) ->
        for w in warnings do
            printfn "%s\n" (w.Format(phase, input, s.Options.Verbose))

        Continue(transformedAssemblies, input, s)
    | Bad (errors) ->
        let msgs =
            [ for msg in errors -> msg.Format(phase, input, s.Options.Verbose) ]

        Exit(1, Some(msgs |> String.concat "\n\n"))

/// Design primers for assemblies, if requested, and write all output formats.
let doOutput (assemblies, input, s): FlowControl<unit> =
    let doOutput () =
        let primers, modifiedAssemblies = doPrimerDesign s.Options assemblies
        doOutputGeneration s primers modifiedAssemblies
        Exit(0, None)

    basicExceptionHandler s.Options.Verbose doOutput

/// Infix version of bind for flow control.  If controlResult is Continue, execute the
/// operation.  Otherwise, fall through.
let (>?>) controlResult (f: 'a -> FlowControl<'b>) =
    match controlResult with
    | Continue (x) -> f x
    | Exit (returnCode, msg) -> Exit(returnCode, msg)

/// Main function call to run GSLc from the command line.
/// Clients of GSLCore may wish to bypass this function and write their own version if they want
/// to intercept the output from the various compiler stages.
let gslc unconfiguredPlugins argv: FlowControl<_> =

    // Configure GSLc and plugins from command line arguments
    configureGslc unconfiguredPlugins argv
    >?> maybeListRefGenomes
    >?> maybeDumpLoci
    >?> checkInputFileList
    >?> maybeJustDoLexing

    >?> runCompiler
    >?> handleCompileResult
    >?> doDnaMaterialization
    >?> handleTransformResult "DNA materialization"
    >?> doAssemblyTransform
    >?> handleTransformResult "DnaAssembly transformation"
    >?> doOutput
