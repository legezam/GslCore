namespace GslCore


open FsToolkit.ErrorHandling
open GslCore.Ast
open GslCore.Ast.Process
open GslCore.Ast.Process.AssemblyStuffing
open GslCore.Ast.Process.PragmaBuilding
open GslCore.DesignParams
open GslCore.GslResult
open GslCore.PcrParamParse
open GslCore.Pragma
open NUnit.Framework
open Amyris.Bio.primercore
open GslCore.Ast.Types
open GslCore.AstFixtures
open GslCore.Ast.Algorithms
open GslCore.AstAssertions
open GslCore.Constants
open GslCore.Ast.Phase1

[<TestFixture>]
[<Category("Integration")>]
type TestPragmas() =
    let pragmaBuilder = PragmaFactory.builtin

    [<Test>]
    member x.TestBadPragmasLocal() =
        let badName = "constantinople"
        let badOption = "timbuktu"

        let goodName = "warn"
        let goodOption = "test"

        Assert.Throws(fun () ->
            (PragmaFactory.createPragmaFromNameValue badName [ badOption ] pragmaBuilder)
            |> GslResult.valueOr (failwithf "%A")
            |> ignore)
        |> ignore

        Assert.DoesNotThrow(fun () ->
            PragmaFactory.createPragmaFromNameValue goodName [ goodOption ]
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
            doParse ignore (fun errs ->
                Assert.Fail
                    (errs
                     |> List.map PcrParameterParseError.toString
                     |> String.concat ", "))

        let shouldFail =
            doParse (fun _ -> Assert.Fail("Parsing didn't fail.")) ignore

        shouldPass [ "mon=50mM"; "primer=5.mM" ]
        shouldFail [ "incrediblybadargument" ]

        shouldFail [ "mon=2.0uM"
                     "anotherbadargument" ]

[<TestFixture>]
[<Category("Integration")>]
type TestPragmasAST() =

    let pragmaBuilder = PragmaFactory.builtin

    let phase1Params =
        { Phase1Parameters.PragmaBuilder = pragmaBuilder
          LegalCapabilities = [ "capa1"; "capa2" ] |> Set.ofList }

    let pragmaBuildPipeline =
        Phase1.variableResolution
        >=> Phase1.functionInlining
        >=> Phase1.stripFunctions
        >=> Phase1.variableResolutionStrict
        >=> Phase1.stripVariables
        >=> Phase1.expressionReduction
        >=> Phase1.pragmaBuilding phase1Params

    let compilePragmas = compile pragmaBuildPipeline

    let checkPragmaIsBuilt node =
        match node with
        | AstNode.Pragma _ -> GslResult.ok ()
        | AstNode.ParsePragma parsePragmaWrapper -> GslResult.err parsePragmaWrapper.Value.Name
        | _ -> GslResult.ok ()

    let pragmaBuildTest source =
        let source = GslSourceCode source

        source
        |> (compilePragmas >> GslResult.mapErrorToAnything)
        >>= Validation.validate checkPragmaIsBuilt
        |> GslResult.assertOk
        |> ignore

    let stuffPragmasPipeline =
        pragmaBuildPipeline
        >=> Phase1.assemblyFlattening phase1Params
        >=> Phase1.assemblyStuffing

    let compilePragmasAndStuffAssemblies =
        compile (pragmaBuildPipeline >=> stuffPragmasPipeline)

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

        let error =
            source
            |> GslSourceCode
            |> compilePragmas
            |> GslResult.assertError

        match error with
        | Choice2Of2 (Phase1Error.PragmaBuildingError (PragmaBuildingError.PragmaBuilder (PragmaFactoryError.MissingDefinition "verybadpragma",
                                                                                          _node))) -> Assert.Pass()
        | x -> Assert.Fail(sprintf "Expecting PragmaBuildingError. Got something else instead: %A" x)

    [<Test>]
    member x.TestBadPragmaScopes() =
        let source = "#fuse\ngFOO {#capa capa1} ; gBAR"

        let errors =
            source
            |> GslSourceCode
            |> compilePragmas
            |> GslResult.assertErrors

        match errors.[0] with
        | Choice2Of2 (Phase1Error.PragmaBuildingError (PragmaBuildingError.PragmaIsUsedInWrongScope (name,
                                                                                                     _allowedScope,
                                                                                                     usedScope,
                                                                                                     _node))) ->
            Assert.AreEqual("fuse", name)
            Assert.AreEqual("block-level", usedScope)
        | x -> Assert.Fail(sprintf "Expecting PragmaIsUsedInWrongScope. Got something else instead: %A" x)

        match errors.[1] with
        | Choice2Of2 (Phase1Error.PragmaBuildingError (PragmaBuildingError.PragmaIsUsedInWrongScope (name,
                                                                                                     _allowedScope,
                                                                                                     usedScope,
                                                                                                     _node))) ->
            Assert.AreEqual("capa", name)
            Assert.AreEqual("part-level", usedScope)
        | x -> Assert.Fail(sprintf "Expecting PragmaIsUsedInWrongScope. Got something else instead: %A" x)


    [<Test>]
    member x.TestDeprecatedPragma() =
        let source = "#stitch\n#stitch"

        let warnings =
            source
            |> GslSourceCode
            |> compilePragmas
            |> GslResult.assertWarnings

        match warnings.[0] with
        | Choice2Of2 (Phase1Error.PragmaBuildingError (PragmaBuildingError.PragmaDeprecated (deprecation, _node))) ->
            Assert.AreEqual("stitch", deprecation.Name)
        | x -> Assert.Fail(sprintf "Expecting PragmaDeprecated. Got something else instead: %A" x)

        match warnings.[1] with
        | Choice2Of2 (Phase1Error.PragmaBuildingError (PragmaBuildingError.PragmaDeprecated (deprecation, _node))) ->
            Assert.AreEqual("stitch", deprecation.Name)
        | x -> Assert.Fail(sprintf "Expecting PragmaDeprecated. Got something else instead: %A" x)


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
        sourceCompareTest
            (stuffPragmasPipeline
             >> GslResult.mapError Phase1Error.toAstMessage)
            source
            source

        let reducedAst =
            (compilePragmasAndStuffAssemblies (source |> GslSourceCode))
            |> GslResult.valueOr (failwithf "%A")

        // we need to dive into the AST to check this
        let namePrag =
            AstNode.Pragma
                (Node.wrapNode
                    { Definition = BuiltIn.namePragmaDef
                      Arguments = [ "foobar" ] })

        let namePrag2 =
            AstNode.Pragma
                (Node.wrapNode
                    { Definition = BuiltIn.namePragmaDef
                      Arguments = [ "shouldOnlyBeOnInner" ] })

        let seedPrag =
            AstNode.Pragma
                (Node.wrapNode
                    { Definition =
                          PragmaFactory
                              .builtin
                              .Pragmas
                              .TryFind("seed")
                              .Value
                      Arguments = [ "123" ] })

        let seedPrag2 =
            AstNode.Pragma
                (Node.wrapNode
                    { Definition =
                          PragmaFactory
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
            compilePragmasAndStuffAssemblies (GslSourceCode(source))
            |> GslResult.assertOk
        // now replace the outer name pragma and make sure the second pass triggers the collision error
        match tree.wrappedNode with
        | AstNode.Block ({ Value = [ AstNode.Pragma (npw); assem ]
                           Positions = p }) ->
            let newNamePrag =
                AstNode.Pragma
                    ({ npw with
                           Value =
                               { npw.Value with
                                     Arguments = [ "differentName" ] } })

            AstNode.Block
                ({ Value = [ newNamePrag; assem ]
                   Positions = p })
        | _ -> failwith "Didn't unwrap correctly."
        |> AstTreeHead
        |> stuffPragmasPipeline
        |> GslResult.assertError
        |> function
        | Phase1Error.AssemblyStuffingError (AssemblyStuffingError.PragmaMerge (_node,
                                                                                PragmaMergeError.PragmaConflict (incoming,
                                                                                                                 existing))) ->

            Assert.AreEqual("name", incoming.Name)

            Assert.AreEqual("name", existing.Name)
        | x -> Assert.Fail(sprintf "Expecting CollidingPragmas. Got something else instead: %A" x)
