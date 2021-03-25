module GslCore.Tests.RunCompilerTest

open GslCore
open GslCore.Plugin
open NUnit.Framework
open Gslc

[<Test>]
[<Category("Manual")>]
let main() =
    let bareMinimumPlugins =
        [ BasicCodonProvider.basicCodonProviderPlugin ]
    
    let argv: string[] =
        [|
            "--lib"; "/home/legezam/dev/gitlab/demetrixbio/demgslc/src/Dmx.GslcService/gslc_lib"
            "//home/legezam/tmp/demetrix/testing/gsl-string-expression/input.gsl"
        |]
    
    try
        let flowResult = gslc bareMinimumPlugins argv
        match flowResult with
        | Exit(code, msg) ->
            msg |> Option.map (printf "%s") |> ignore
            printfn "%A" msg
            printfn "%i" code
        | Continue _ ->
            printfn "InternalError: GSL relinquished flow control in the continue state."
            exit 1

    with e ->
        printfn "InternalError:\n%A" (e)
