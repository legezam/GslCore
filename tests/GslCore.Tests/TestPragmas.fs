namespace GslCore

open GslCore.Ast.Process
open GslCore.DesignParams
open GslCore.GslResult
open GslCore.Pragma
open NUnit.Framework
open Amyris.Bio.primercore
open GslCore.Ast.Types
open GslCore.AstFixtures
open GslCore.Ast.Algorithms
open GslCore.AstAssertions
open GslCore.Ast.ErrorHandling
open GslCore.Constants

[<TestFixture>]
type TestPragmas() =
    let pragmaBuilder = PragmaBuilder.builtin

    [<Test>]
    member x.TestBadPragmasLocal() =
        let badName = "constantinople"
        let badOption = "timbuktu"

        let goodName = "warn"
        let goodOption = "test"

        Assert.Throws(fun () ->
            (PragmaBuilder.createPragmaFromNameValue badName [ badOption ] pragmaBuilder)
            |> GslResult.valueOr (failwithf "%A")
            |> ignore)
        |> ignore

        Assert.DoesNotThrow(fun () ->
            PragmaBuilder.createPragmaFromNameValue goodName [ goodOption ]
            |> ignore)


    [<Test>]
    /// Smoke test parsing PCR parameters.
    member x.TestPcrParamsParsing() =
        let doParse ifGood ifBad args =
            match DesignParams.revise defaultParams args
                  |> GslResult.GetValue with
            | Ok success -> ifGood success.Result
            | Error msgs -> ifBad msgs

        let shouldPass =
            doParse ignore (fun errs -> Assert.Fail(String.concat ", " errs))

        let shouldFail =
            doParse (fun _ -> Assert.Fail("Parsing didn't fail.")) ignore

        shouldPass [ "mon=50mM"; "primer=5.mM" ]
        shouldFail [ "incrediblybadargument" ]

        shouldFail [ "mon=2.0uM"
                     "anotherbadargument" ]

[<TestFixture>]
type TestPragmasAST() =

    let pragmaBuilder = PragmaBuilder.builtin

    let phase1Params =
        { Phase1Parameters.PragmaBuilder = pragmaBuilder
          LegalCapabilities = [ "capa1"; "capa2" ] |> Set.ofList }

    let pragmaBuildPipeline =
        VariableResolution.resolveVariables
        >=> Inlining.inlineFunctionCalls
        >=> Cleanup.stripFunctions
        >=> VariableResolution.resolveVariablesStrict
        >=> Cleanup.stripVariables
        >=> ExpressionReduction.reduceMathExpressions
        >=> (PragmaBuilding.buildPragmas phase1Params)

    let compilePragmas = compile pragmaBuildPipeline

    let checkPragmaIsBuilt node =
        match node with
        | Pragma _ -> Validation.good
        | ParsePragma (p) -> AstResult.errStringF GeneralError "Pragma '%s' was not built." p.Value.Name node
        | _ -> Validation.good

    let pragmaBuildTest source =
        let source = GslSourceCode source

        source
        |> compilePragmas
        >>= Validation.validate checkPragmaIsBuilt
        |> failIfBad (Some(source))
        |> ignore

    let stuffPragmasPipeline =
        pragmaBuildPipeline
        >=> AssemblyFlattening.flattenAssemblies phase1Params
        >=> AssemblyStuffing.stuffPragmasIntoAssemblies

    [<Test>]
    member x.TestBasicPragmaBuild() =
        let source = """
#name test
let myName = "foo"
#name &myName
let bar(baz) =
    #name &myName
end
"""
        pragmaBuildTest source

    [<Test>]
    member x.TestFuncArgsInPragmas() =
        let source = """
let myName = "foo"
let myNumber = 123
let bar(baz) =
    #name &myName
    #name &baz
    #primermax &myNumber
end
bar("qux")
"""
        pragmaBuildTest source

    [<Test>]
    member x.TestUnknownPragma() =
        let source = "#verybadpragma"

        source
        |> GslSourceCode
        |> compile pragmaBuildPipeline
        |> assertFail PragmaError (Some("Unknown or invalid pragma: '#verybadpragma'"))
        |> ignore

    [<Test>]
    member x.TestBadPragmaScopes() =
        let source = "#fuse\ngFOO {#capa capa1} ; gBAR"

        source
        |> GslSourceCode
        |> compile pragmaBuildPipeline
        |> assertFailMany [ PragmaError; PragmaError ] [
            Some("#fuse is used at block-level")
            Some("#capa is used at part-level")
           ]
        |> ignore


    [<Test>]
    member x.TestDeprecatedPragma() =
        let source = "#stitch\n#stitch"

        source
        |> GslSourceCode
        |> compile pragmaBuildPipeline
        |> assertWarnMany [ DeprecationWarning
                            DeprecationWarning ] [
            Some("#stitch is deprecated")
            Some("#stitch is deprecated")
           ]
        // test the deprecation warning deduplication mechanism
        |> fun success -> GslParseErrorContext.deduplicateMessages success.Warnings
        |> (fun msgs -> AstResult.warns msgs ())
        |> assertWarn DeprecationWarning (Some("appear only once per file"))
        |> ignore


    [<Test>]
    member x.TestStuffPragmas() =
        // first assembly should have name, and no others
        // assembly inside the do block should have different seed
        let source = """
#name foobar
#seed 123
gFOO
#name shouldOnlyBeOnInner
do
    #seed 456
    gBAR
end
gBAZ"""

        // outer assemblies shouldn't reprint their pragma context as that isn't idiomatic GSL
        sourceCompareTest stuffPragmasPipeline source source

        let reducedAst =
            (compile stuffPragmasPipeline (source |> GslSourceCode))
            |> GslResult.valueOr (failwithf "%A")

        // we need to dive into the AST to check this
        let namePrag =
            Pragma
                (Node.wrapNode
                    { Definition = BuiltIn.namePragmaDef
                      Arguments = [ "foobar" ] })

        let namePrag2 =
            Pragma
                (Node.wrapNode
                    { Definition = BuiltIn.namePragmaDef
                      Arguments = [ "shouldOnlyBeOnInner" ] })

        let seedPrag =
            Pragma
                (Node.wrapNode
                    { Definition =
                          PragmaBuilder
                              .builtin
                              .Pragmas
                              .TryFind("seed")
                              .Value
                      Arguments = [ "123" ] })

        let seedPrag2 =
            Pragma
                (Node.wrapNode
                    { Definition =
                          PragmaBuilder
                              .builtin
                              .Pragmas
                              .TryFind("seed")
                              .Value
                      Arguments = [ "456" ] })

        let barGenePart = basePartWrap (createGenePart "gBAR")
        let bazGenePart = basePartWrap (createGenePart "gBAZ")
        let fooAssembly = assemble [ fooGenePart ]
        let barAssembly = assemble [ barGenePart ]
        let bazAssembly = assemble [ bazGenePart ]

        let fooAssemblyWithPragmas =
            addPragsToPart [ namePrag; seedPrag ] fooAssembly

        let barAssemblyWithPragmas =
            addPragsToPart [ namePrag2; seedPrag2 ] barAssembly

        let bazAssemblyWithPragmas = addPragsToPart [ seedPrag ] bazAssembly

        let innerBlock =
            blockify [ seedPrag2
                       barAssemblyWithPragmas ]

        assertTreesEqual
            (treeify [ namePrag
                       seedPrag
                       fooAssemblyWithPragmas
                       namePrag2
                       innerBlock
                       bazAssemblyWithPragmas ])
            reducedAst

    [<Test>]
    member x.TestNoCollisions() =
        // A pragma collision is almost impossible to create in source code, which is a good sign I guess.
        // Create an artificial AST to make sure the error is triggered.
        let source = "#name foobar\ngFOO; gBAR"

        let tree =
            compile stuffPragmasPipeline (GslSourceCode(source))
            |> GslResult.valueOr (failwithf "%A")
        // now replace the outer name pragma and make sure the second pass triggers the collision error
        match tree.wrappedNode with
        | Block ({ Value = [ Pragma (npw); assem ]
                   Positions = p }) ->
            let newNamePrag =
                Pragma
                    ({ npw with
                           Value =
                               { npw.Value with
                                     Arguments = [ "differentName" ] } })

            Block
                ({ Value = [ newNamePrag; assem ]
                   Positions = p })
        | _ -> failwith "Didn't unwrap correctly."
        |> AstTreeHead
        |> stuffPragmasPipeline
        |> assertFail
            PragmaError
               (Some
                   ("The pragma #name is set in this assembly as well as in the enclosing environment with conflicting values."))
        |> ignore
