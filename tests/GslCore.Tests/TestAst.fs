namespace GslCore.Tests

open GslCore
open GslCore.Ast.Linting
open GslCore.Ast.MessageTranslation
open GslCore.Ast.Process
open GslCore.Ast.Process.Validation
open GslCore.Pragma
open NUnit.Framework
open GslCore.GslResult
open GslCore.Ast.Types
open GslCore.Ast.Process.VariableResolution
open GslCore.Ast.Process.ExpressionReduction
open GslCore.Ast.Process.Inlining
open GslCore.Ast.Process.PragmaBuilding
open GslCore.Ast.Process.AssemblyFlattening
open GslCore.Ast.ErrorHandling
open GslCore.Ast
open GslCore.AstFixtures
open GslCore.AstAssertions
open GslCore.Ast.Algorithms
open GslCore.Constants

[<TestFixture>]
type TestLinting() =

    [<Test>]
    member x.TestDetectOldVariableSyntax() =
        "@foo"
        |> GslSourceCode
        |> compile
            ((Linter.linters
              >> GslResult.mapError LinterHintMessage.toAstMessage))
        |> assertWarn Warning (Some("The syntax for using a variable has changed"))
        |> ignore

    [<Test>]
    member x.TestDetectPushPop() =
        "#push\n#pop"
        |> GslSourceCode
        |> compile
            (Linter.linters
             >> GslResult.mapError LinterHintMessage.toAstMessage)
        |> assertFailMany [ PragmaError; PragmaError ] [
            Some("#push and #pop have been removed")
            Some("#push and #pop have been removed")
           ]
        |> ignore



[<TestFixture>]
type TestValidation() =

    let assertValidationFail msgType msgSnippet op tree =
        (Validation.validate op tree)
        |> assertFail msgType msgSnippet

    [<Test>]
    member x.TestDetectParseError() =
        let errorText = "test failure"
        let err = Utils.createParseError errorText []

        let tree = treeify [ err ]

        let failure =
            assertValidationFail
                ParserError
                (Some errorText)
                (ParseErrorValidation.checkParseError
                 >> GslResult.mapError ParseErrorMessage.toAstMessage)
                tree

        Assert.AreEqual(err, failure.Node)

    [<Test>]
    member x.NoModsAllowed() =
        let source = GslSourceCode("###[2:20]")

        let tree =
            lexparse source
            |> GslResult.valueOr (failwithf "%A")

        assertValidationFail
            PartError
            (Some "Can only apply part mods to Gene or PartId, not Marker")
            (PartValidation.validateModifiers
             >> GslResult.mapError PartModifierValidationMessage.toAstMessage)
            tree
        |> ignore

    [<Test>]
    member x.NoModsOnAssemblies() =
        let source = GslSourceCode("(pFOO; gFOO)[2:20]")

        let tree =
            lexparse source
            |> GslResult.valueOr (failwithf "%A")

        assertValidationFail
            PartError
            (Some "Can only apply part mods to Gene or PartId, not Assembly")
            (PartValidation.validateModifiers
             >> GslResult.mapError PartModifierValidationMessage.toAstMessage)
            tree
        |> ignore

[<TestFixture>]
type TestTransformation() =


    let variableTest =
        sourceCompareTest
            (VariableResolution.resolveVariables
             >> GslResult.mapError VariableResolutionMessage.toAstMessage)

    let mathReductionTest =
        sourceCompareTest
            ((VariableResolution.resolveVariables
              >> GslResult.mapError VariableResolutionMessage.toAstMessage)
             >=> (ExpressionReduction.reduceMathExpressions
                  >> GslResult.mapError ExpressionReductionMessage.toAstMessage))

    let functionInliningTest =
        sourceCompareTest
            ((VariableResolution.resolveVariables
              >> GslResult.mapError VariableResolutionMessage.toAstMessage)
             >=> (Inlining.inlineFunctionCalls
                  >> GslResult.mapError FunctionInliningMessage.toAstMessage)
             >=> (Cleanup.stripFunctions
                  >> GslResult.mapError NoMessage.toAstMessage))

    let flattenAssemblyTest =
        sourceCompareTest
            ((PragmaBuilding.buildPragmas AssemblyTestSupport.defaultPhase1Parameters
              >> GslResult.mapError PragmaBuildingMessage.toAstMessage)
             >=> (AssemblyFlattening.flattenAssemblies AssemblyTestSupport.defaultPhase1Parameters
                  >> GslResult.mapError AssemblyFlatteningMessage.toAstMessage))

    let flattenPartTest =
        sourceCompareTest
            ((VariableResolution.resolveVariables
              >> GslResult.mapError VariableResolutionMessage.toAstMessage)
             >=> (AssemblyFlattening.flattenAssemblies AssemblyTestSupport.defaultPhase1Parameters
                  >> GslResult.mapError AssemblyFlatteningMessage.toAstMessage))

    let variableResolutionPipeline =
        (RecursiveCalls.check
         >> GslResult.mapError RecursiveCallCheckMessage.toAstMessage)
        >=> (VariableResolution.resolveVariables
             >> GslResult.mapError VariableResolutionMessage.toAstMessage)
        >=> (Inlining.inlineFunctionCalls
             >> GslResult.mapError FunctionInliningMessage.toAstMessage)
        >=> (Cleanup.stripFunctions
             >> GslResult.mapError NoMessage.toAstMessage)
        >=> (VariableResolution.resolveVariablesStrict
             >> GslResult.mapError VariableResolutionMessage.toAstMessage)

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
&baz
"""

        GslSourceCode(source)
        |> compile
            (VariableResolution.resolveVariables
             >> GslResult.mapError VariableResolutionMessage.toAstMessage)
        |> assertFailMany [ UnresolvedVariable
                            UnresolvedVariable ] [
            Some("bar")
            Some("baz")
           ]
        |> ignore

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
        |> assertFail RecursiveFunctionCall None
        |> ignore

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
        |> assertFailMany [ TypeError; TypeError ] [
            Some("The variable int has been inferred to have the type Part")
            Some("The variable part has been inferred to have the type Int")
           ]
        |> ignore

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
        |> assertFailMany [ TypeError; TypeError ] [
            Some("Function 'foo' expects 2 arguments but received 1.")
            Some("Function 'foo' expects 2 arguments but received 3.")
           ]
        |> ignore

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
