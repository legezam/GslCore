namespace GslCore.Tests

open GslCore
open GslCore.Ast
open GslCore.Ast.Linting
open GslCore.Ast.Phase1
open GslCore.Ast.Process
open GslCore.Ast.Process.Inlining
open GslCore.Ast.Process.Validation
open GslCore.Ast.Process.VariableResolution
open GslCore.Pragma
open NUnit.Framework
open GslCore.GslResult
open GslCore.Ast.Types
open GslCore.Ast.ErrorHandling
open GslCore.AstFixtures
open GslCore.AstAssertions
open GslCore.Ast.Algorithms
open GslCore.Constants

[<TestFixture>]
[<Category("Integration")>]
type TestLinting() =

    [<Test>]
    member x.TestDetectOldVariableSyntax() =
        "@foo"
        |> GslSourceCode
        |> compile Phase1.linting
        |> GslResult.assertWarning
        |> function
        | Choice2Of2 (Phase1Error.LinterHint (LinterHint.VariableReferenceDeprecated (name, _node))) ->
            Assert.AreEqual("foo", name)
        | x -> Assert.Fail(sprintf "Expected AstMessage, got %O instead" x)

    [<Test>]
    member x.TestDetectPush() =
        "#push"
        |> GslSourceCode
        |> compile Phase1.linting
        |> GslResult.assertErrors
        |> fun errors ->
            match errors.[0] with
            | Choice2Of2 (Phase1Error.LinterHint (LinterHint.PushPopDeprecated _node)) -> Assert.Pass()
            | x -> Assert.Fail(sprintf "Expected AstMessage, got %O instead" x)


    [<Test>]
    member x.TestDetectPop() =
        "#pop"
        |> GslSourceCode
        |> compile Phase1.linting
        |> GslResult.assertErrors
        |> fun errors ->
            match errors.[0] with
            | Choice2Of2 (Phase1Error.LinterHint (LinterHint.PushPopDeprecated _node)) -> Assert.Pass()
            | x -> Assert.Fail(sprintf "Expected AstMessage, got %O instead" x)

    [<Test>]
    member x.TestDetectPushPop2() =
        let result =
            "#push\n#pop"
            |> GslSourceCode
            |> compile Phase1.linting

        printfn "%A" result




[<TestFixture>]
[<Category("Integration")>]
type TestValidation() =

    let assertValidationFail (msgType: AstMessageType)
                             (msgSnippet: string option)
                             (op: AstNode -> GslResult<unit, AstMessage>)
                             (tree: AstTreeHead)
                             =
        (Validation.validate op tree)
        |> GslResult.assertError
        |> fun error ->
            Assert.AreEqual(msgType, error.Type)

            msgSnippet
            |> Option.iter (fun snippet -> Assert.IsTrue(error.Message.Contains snippet))

            error

    [<Test>]
    member x.TestDetectParseError() =
        let errorText = "test failure"
        let err = Utils.createParseError errorText []

        let tree = treeify [ err ]

        let failure =
            assertValidationFail
                AstMessageType.ParserError
                (Some errorText)
                (Phase1.checkParseError
                 >> GslResult.mapError Phase1Error.toAstMessage)
                tree

        Assert.AreEqual(err, failure.Node)

    [<Test>]
    member x.NoModsAllowed() =
        let source = GslSourceCode("###[2:20]")

        let tree =
            lexparse source
            |> GslResult.valueOr (failwithf "%A")

        assertValidationFail
            AstMessageType.PartError
            (Some "Can only apply part mods to Gene or PartId, not Marker")
            (PartValidation.validateModifiers
             >> GslResult.mapError
                 (Phase1Error.PartModifierValidationError
                  >> Phase1Error.toAstMessage))
            tree
        |> ignore

    [<Test>]
    member x.NoModsOnAssemblies() =
        let source = GslSourceCode("(pFOO; gFOO)[2:20]")

        let tree =
            lexparse source
            |> GslResult.valueOr (failwithf "%A")

        assertValidationFail
            AstMessageType.PartError
            (Some "Can only apply part mods to Gene or PartId, not Assembly")
            (PartValidation.validateModifiers
             >> GslResult.mapError
                 (Phase1Error.PartModifierValidationError
                  >> Phase1Error.toAstMessage))
            tree
        |> ignore

[<TestFixture>]
[<Category("Integration")>]
type TestTransformation() =


    let variableTest =
        sourceCompareTest
            (Phase1.variableResolution
             >> GslResult.mapError Phase1Error.toAstMessage)

    let mathReductionTest =
        sourceCompareTest
            (Phase1.variableResolution
             >=> Phase1.expressionReduction
             >> GslResult.mapError Phase1Error.toAstMessage)

    let functionInliningTest =
        sourceCompareTest
            (Phase1.variableResolution
             >=> Phase1.functionInlining
             >=> Phase1.stripFunctions
             >> GslResult.mapError Phase1Error.toAstMessage)

    let flattenAssemblyTest =
        sourceCompareTest
            (Phase1.pragmaBuilding AssemblyTestSupport.defaultPhase1Parameters
             >=> Phase1.assemblyFlattening AssemblyTestSupport.defaultPhase1Parameters
             >> GslResult.mapError Phase1Error.toAstMessage)

    let flattenPartTest =
        sourceCompareTest
            (Phase1.variableResolution
             >=> Phase1.assemblyFlattening AssemblyTestSupport.defaultPhase1Parameters
             >> GslResult.mapError Phase1Error.toAstMessage)

    let variableResolutionPipeline =
        Phase1.recursiveCallCheck
        >=> Phase1.variableResolution
        >=> Phase1.functionInlining
        >=> Phase1.stripFunctions
        >=> Phase1.variableResolutionStrict

    let fullVariableResolutionTest =
        sourceCompareTest variableResolutionPipeline

    [<Test>]
    member x.TestVariableResolutionBasic() =
        variableTest "let foo = gFOO\n&foo" "let foo = gFOO\ngFOO"

    [<Test>]
    member x.TestVariableResolutionOneLevel() =
        let source = """
let foo = gFOO
let bar(p) =
    &p
    &foo
end"""

        let expectedResolution = """
let foo = gFOO
let bar(p) =
    &p
    gFOO
end"""
        variableTest source expectedResolution

    [<Test>]
    member x.TestBlockScopedVariables() =
        let source = """
let foo(bar) =
    let baz = 1
    &bar
end
&bar
"""

        GslSourceCode(source)
        |> compile Phase1.variableResolution
        |> GslResult.assertError
        |> fun error ->
            match error with
            | Choice2Of2 (Phase1Error.VariableResolutionError (VariableResolutionError.UnresolvedVariable (name, _node))) ->
                Assert.AreEqual("bar", name)
            | x -> Assert.Fail(sprintf "Expected AstMessage, got %O instead" x)

    [<Test>]
    member x.TestBlockScopedVariables2() =
        let source = """
let foo(bar) =
    let baz = 1
    &bar
end
&baz
"""

        GslSourceCode(source)
        |> compile Phase1.variableResolution
        |> GslResult.assertError
        |> fun error ->
            match error with
            | Choice2Of2 (Phase1Error.VariableResolutionError (VariableResolutionError.UnresolvedVariable (name, node))) ->
                Assert.AreEqual("baz", name)
            | x -> Assert.Fail(sprintf "Expected AstMessage, got %O instead" x)

    [<Test>]
    member x.TestIntExprResolution() =
        let source = "let foo = 1\nlet bar = &foo + 2\n"
        let expected = "let foo = 1\nlet bar = (1 + 2)\n"
        variableTest source expected

        // test deep nesting
        let source = """
let foo = 1
let bar = &foo + 2
let baz = &foo + &bar
let qux(a, b) =
    let local0 = &b
    let local1 = &a + &foo
    let local2 = &local1 + &bar + &baz + &local1 + &local0
end
"""

        let expected = """
let foo = 1
let bar = (1 + 2)
let baz = (1 + (1 + 2))
let qux(a, b) =
    let local0 = &b
    let local1 = (&a + 1)
    let local2 = (((((&a + 1) + (1 + 2)) + (1 + (1 + 2))) + (&a + 1)) + &local0)
end
"""
        // note that &local0 cannot resolve to &b at this phase, because the use has a a type via
        // context but eventually resolves to the untyped &b function local variable, so it should
        // not expand at this phase.
        variableTest source expected

    [<Test>]
    member x.TestMathReduction() =
        let source = "let foo = 1 + 1\n"
        let expected = "let foo = 2"
        mathReductionTest source expected

    [<Test>]
    member x.TestAlwaysFailingRegressionTest() =
        let source = """
let x = -12
let y = 1+1
let z = (4*10)+7-5
gHO[&x:&z]
gHO[&y:~&z]"""

        let expected = """
let x = -12
let y = 2
let z = 42
gHO[-12:42]
gHO[2:~42]"""
        mathReductionTest source expected

    [<Test>]
    member x.TestFunctionInlining() =
        let source = """
let foo(bar) =
    let baz = 1 + &bar
    let innerFunc(x) =
        let innerVar = "wow!"
        let innerVar2 = &x
        pFOO ; gBAR[&innerVar2:20] {#name &innerVar}
    end
    innerFunc(&baz)
end
foo(2)
"""

        let expected = """
do
    let bar = 2
    let baz = (1 + &bar)
    do
        let x = (1 + 2)
        let innerVar = "wow!"
        let innerVar2 = &x
        pFOO ; gBAR[&innerVar2:20] {#name wow!}
    end
end
"""
        functionInliningTest source expected

    [<Test>]
    member x.TestFunctionCallNested() =
        let source = """
let foo(bar, baz) =
    &bar ; &baz
end
let fooWithFixedArg(qux) =
    foo(&qux, gFOO)
end
fooWithFixedArg(pFOO)
"""

        let expected = """
do
    let qux = pFOO
    do
        let bar = pFOO
        let baz = gFOO
        pFOO ; gFOO
    end
end
"""
        fullVariableResolutionTest source expected

    [<Test>]
    member x.TestFunctionCallAliasing() =
        let source = """
let f1(x, y, z) =
    &x ; &y ; &z
end
let f2(x, y) =
    f1(gFOO, &x, &y)
end
f2(gBAR, gBAZ)
"""

        let expected = """
do
    let x = gBAR
    let y = gBAZ
    do
        let x = gFOO
        let y = gBAR
        let z = gBAZ
        gFOO ; gBAR ; gBAZ
    end
end
"""
        fullVariableResolutionTest source expected

    [<Test>]
    /// Test that variable aliases (let foo = &bar) resolve correctly.
    member x.TestVariableAliasResolution() =
        let source = """
let foo = gFOO
let bar = 1
let fooAlias = &foo
let barAlias = &bar
let testFunc(a, b) =
    let baz = &a
    pFOO ; &b
end
testFunc(&barAlias, &fooAlias)
"""

        let expected = """
let foo = gFOO
let bar = 1
let fooAlias = gFOO
let barAlias = 1
do
    let a = 1
    let b = gFOO
    let baz = 1
    pFOO ; gFOO
end
"""
        fullVariableResolutionTest source expected

    [<Test>]
    member x.TestRecursiveFunctionCall() =
        let source = """
let foo(x) =
    let bar(y) =
        foo(&y)
    end
    bar(&x)
end
foo(1)"""

        GslSourceCode(source)
        |> compile variableResolutionPipeline
        |> GslResult.assertError
        |> function
        | Choice2Of2 (Phase1Error.RecursiveCallCheckError (RecursiveCallFoundError _)) -> Assert.Pass()
        | x -> Assert.Fail(sprintf "Expected AstMessage, got %O instead" x)

    [<Test>]
    /// Test that we correctly catch type errors.
    member x.TestTypeChecking() =
        let source = """
let fooPart = gFOO
let fooInt = 1
let testFunc(int, part) =
    let baz = 1 + &int
    pFOO ; &part
end
testFunc(&fooPart, &fooInt)
"""

        GslSourceCode(source)
        |> compile variableResolutionPipeline
        |> GslResult.assertErrors
        |> fun errors ->
            match errors.[0] with
            | Choice2Of2 (Phase1Error.VariableResolutionError (VariableResolutionError.TypeCheck (TypeCheckError.ElisionConflict (elidedType,
                                                                                                                                  targetType),
                                                                                                  _node,
                                                                                                  varName))) ->
                Assert.AreEqual("part", varName)
                Assert.AreEqual(PartType, targetType)
                Assert.AreEqual(IntType, elidedType)
            | x -> Assert.Fail(sprintf "Expected AstMessage, got %O instead" x)


    [<Test>]
    member x.TestFunctionCallArgCount() =
        let source = """
let foo(a, b) =
    &a ; &b
end
foo(gFOO)
foo(gFOO, pFOO, dFOO)
"""

        GslSourceCode(source)
        |> compile variableResolutionPipeline
        |> GslResult.assertErrors
        |> fun errors ->
            match errors.[0] with
            | Choice2Of2 (Phase1Error.FunctionInliningError (InliningError.Validation (_,
                                                                                       FunctionValidationError.ParameterNumberMismatch (_,
                                                                                                                                        needed,
                                                                                                                                        actual)))) ->
                Assert.AreEqual(2, needed)
                Assert.AreEqual(1, actual)
            | x -> Assert.Fail(sprintf "Expected AstMessage, got %O instead" x)


    [<Test>]
    member x.TestFunctionCallArgCount2() =
        let source = """
let foo(a, b) =
    &a ; &b
end
foo(gFOO, pFOO, dFOO)
"""

        GslSourceCode(source)
        |> compile variableResolutionPipeline
        |> GslResult.assertErrors
        |> fun errors ->
            match errors.[0] with
            | Choice2Of2 (Phase1Error.FunctionInliningError (InliningError.Validation (_node,
                                                                                       FunctionValidationError.ParameterNumberMismatch (_functionCall,
                                                                                                                                        needed,
                                                                                                                                        actual)))) ->
                Assert.AreEqual(2, needed)
                Assert.AreEqual(3, actual)
            | x -> Assert.Fail(sprintf "Expected AstMessage, got %O instead" x)


    [<Test>]
    member x.TestFlattenAssemblies() =
        let source =
            "gFOO ; (pBAR ; (gBAR ; dBAR) {#name inner}) {#seed 123} ; gBAZ"

        let expected =
            "gFOO ; pBAR {#seed 123} ; gBAR {#name inner #seed 123} ; dBAR {#name inner #seed 123} ; gBAZ"

        flattenAssemblyTest source expected

    [<Test>]
    member x.TestFlattenReverseAssemblies() =
        let source =
            "gFOO ; !(pBAR {#fuse} ; !gBAR ; gBAZ) ; gQUX"

        let expected =
            "gFOO ; !gBAZ ; gBAR {#fuse} ; !pBAR ; gQUX"

        flattenAssemblyTest source expected

    [<Test>]
    member x.TestFlattenReverseWithInvertPragmas() =
        let source =
            "gFOO ; !(pBAR {#rabitstart} ; !gBAR {#rabitend} ; gBAZ) ; gQUX"

        let expected =
            "gFOO ; !gBAZ ; gBAR {#rabitstart} ; !pBAR {#rabitend} ; gQUX"

        flattenAssemblyTest source expected

    [<Test>]
    member x.TestFlattenSinglePartVariable() =
        let source = "let foo = @R123\n!&foo"
        let expected = "let foo = @R123\n!@R123"

        flattenPartTest source expected
