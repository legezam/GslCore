namespace GslCore.Tests

open GslCore
open GslCore.Ast
open GslCore.Ast.MessageTranslation
open GslCore.Core.Expansion
open GslCore.GslResult
open GslCore.Pragma
open NUnit.Framework
open GslCore.Constants
open GslCore.Ast.Types
open GslCore.Ast.ErrorHandling
open GslCore.Ast.Algorithms
open GslCore.AstAssertions
open GslCore.Ast.Legacy

[<TestFixture>]
type TestBootstrapping() =

    /// Very dumb expansion rule that just reprints the asseembly.
    let reprintAssembly a =
        let s = LegacyPrettyPrint.assembly a
        printfn "Reprinted assembly: %s" s.String
        s

    /// Expansion rule that always raises an exception.
    let expansionFail _ =
        failwith "Expansion failed with an exception."

    let bootstrapToTree node =
        match node with
        | Splice (nodes) -> AstTreeHead(Block(Node.wrapNode (List.ofArray nodes)))
        | x -> failwithf "Illegal: %A" x

    let bootstrapPhase1NoCapas =
        Bootstrapping.bootstrapPhase1 AssemblyTestSupport.defaultPhase1Parameters

    let compilePhase1NoCapas =
        GslSourceCode
        >> (compile
                (Phase1.phase1 AssemblyTestSupport.defaultPhase1Parameters
                 >> GslResult.mapError Phase1Message.toAstMessage))

    /// Test that a bootstrap operation round-trips successfully.
    let testAssembly source =
        let source = GslSourceCode source

        source
        |> bootstrapPhase1NoCapas []
        |> GslResult.map bootstrapToTree
        |> failIfBad (Some(source))
        |> GslResult.valueOr (failwithf "%A")
        |> assertDecompilesTo source.String


    let testPhase1 node =
        let bootstrapOperation =
            Bootstrapping.bootstrapExpandLegacyAssembly GeneralError reprintAssembly bootstrapPhase1NoCapas

        Bootstrapping.executeBootstrap bootstrapOperation Serial node

    let testCaptureException node =
        let bootstrapOperation =
            Bootstrapping.bootstrapExpandLegacyAssembly GeneralError expansionFail bootstrapPhase1NoCapas

        Bootstrapping.executeBootstrap bootstrapOperation Serial node

    [<Test>]
    member x.TestBootstrapAssembly() =
        testAssembly "gFOO ; ### ; ~ ; gFOO[1S:30E] {#name foo}"

    [<Test>]
    member x.SmokeTestFullPhase1Bootstrap() =
        let source =
            "gFOO ; ### ; ~ ; gFOO[1S:30E] {#name foo}"

        let compiledTree =
            compilePhase1NoCapas source
            |> failIfBad (Some(GslSourceCode source))
            |> GslResult.valueOr (failwithf "%A")

        compiledTree
        |> testPhase1
        |> failIfBad None
        |> GslResult.valueOr (failwithf "%A")
        |> assertDecompilesTo source

        printfn "Source in: %s" source

    [<Test>]
    member x.TestCaptureExpansionFailure() =
        let source = "gFOO" // doesn't matter what's in here for this test

        compilePhase1NoCapas source
        |> failIfBad (Some(GslSourceCode source))
        |> GslResult.valueOr (failwithf "%A")
        |> testCaptureException
        |> assertFail (GeneralError) (Some "Expansion failed with an exception.")
        |> ignore
