module GslCore.Tests.Ast.Process.FunctionInliningTest

open System.Collections
open GslCore.Ast.Process.Inlining
open GslCore.Ast.Process.VariableResolution
open GslCore.Ast.Types
open GslCore.AstAssertions
open GslCore.GslResult
open NUnit.Framework

let createTestNode (value: 'a): Node<'a> = { Node.Value = value; Positions = [] }

[<Test>]
let ``checkArguments results in ok if same number of parameters are provided`` () =
    let parseFunction =
        { ParseFunction.Name = "foo"
          ArgumentNames = [ "param" ]
          Body = AstNode.String(createTestNode "") }

    let functionCall =
        { FunctionCall.Name = "foo"
          Arguments = [ AstNode.String(createTestNode "doesn't matter") ] }

    let testNode = AstNode.String(createTestNode "foo")

    let resultParseFunction, resultFunctionCall =
        Inlining.checkArguments parseFunction functionCall testNode
        |> GslResult.assertOk

    Assert.AreSame(functionCall, resultFunctionCall)
    Assert.AreSame(parseFunction, resultParseFunction)

[<Test>]
let ``checkArguments results in error if different number of parameters are provided`` () =
    let parseFunction =
        { ParseFunction.Name = "foo"
          ArgumentNames = [ "param1"; "param2" ]
          Body = AstNode.String { Node.Value = ""; Positions = [] } }

    let functionCall =
        { FunctionCall.Name = "foo"
          Arguments = [ AstNode.String(createTestNode "doesn't matter") ] }

    let testNode = AstNode.String(createTestNode "foo")

    let result =
        Inlining.checkArguments parseFunction functionCall testNode
        |> GslResult.assertError


    Assert.AreEqual(ParameterNumberMismatchError(testNode, functionCall, 2, 1), result)


type TestCasesForVariableBindingFromTypedValueAndName() =
    static member TestCasesForVariableBindingFromTypedValueAndName: IEnumerable =
        seq {
            let testVariableBinding =
                { VariableBinding.Name = "fooVariable"
                  Type = GslVariableType.NotYetTyped
                  Value = createTestNode "myValue" |> AstNode.String }

            TestCaseData({| Bindings =
                                [ "fooVariable",
                                  VariableResolutionWrapper.VariableBinding(createTestNode testVariableBinding) ]
                                |> Map.ofList
                            Name = "fooParameter"
                            Value =
                                AstNode.TypedValue
                                    (createTestNode
                                        (GslVariableType.NotYetTyped,
                                         AstNode.TypedVariable
                                             (createTestNode ("fooVariable", GslVariableType.NotYetTyped)))) |})

                .Returns({ testVariableBinding with
                               Name = "fooParameter" }
                         |> createTestNode
                         |> AstNode.VariableBinding
                         |> GslResult.ok: GslResult<AstNode, FunctionInliningError>)
                .SetName("Default case, function parameter gets inlined as a local variable with the name of the TypedVariable")

            let missingVariableName = "fooVariable2"

            let typedVariable =
                AstNode.TypedVariable(createTestNode (missingVariableName, GslVariableType.NotYetTyped))

            TestCaseData({| Bindings =
                                [ "fooVariable",
                                  VariableResolutionWrapper.VariableBinding(createTestNode testVariableBinding) ]
                                |> Map.ofList
                            Name = "fooParameter"
                            Value = AstNode.TypedValue(createTestNode (GslVariableType.NotYetTyped, typedVariable)) |})

                .Returns(GslResult.err
                             (FunctionInliningError.VariableResolution
                                 (VariableResolutionError.UnresolvedVariable(missingVariableName, typedVariable))): GslResult<AstNode, FunctionInliningError>)
                .SetName("Missing captured binding, results in error")



            let illegalFunctionCallContent = AstNode.String(createTestNode "illegal")

            TestCaseData({| Bindings =
                                Map.empty : CapturedVariableBindings
                            Name = "fooParameter"
                            Value = illegalFunctionCallContent |})

                .Returns(GslResult.err
                             (FunctionInliningError.InternalFunctionCallTypeMismatch illegalFunctionCallContent): GslResult<AstNode, FunctionInliningError>)
                .SetName("FunctionCall must contain TypedValues")

            let variableTypes =
                [ GslVariableType.FloatType
                  GslVariableType.IntType
                  GslVariableType.PartType
                  GslVariableType.StringType
                  GslVariableType.NotYetTyped ]

            for testCapturedVariableBindingType in variableTypes do
                for typedValueWrapperType in variableTypes do
                    for typedVariableBindingType in variableTypes do
                        if testCapturedVariableBindingType = typedVariableBindingType then
                            let testCapturedVariableBinding =
                                { VariableBinding.Name = "fooVariable"
                                  Type = testCapturedVariableBindingType
                                  Value = createTestNode "myValue" |> AstNode.String }

                            TestCaseData({| Bindings =
                                                [ "fooVariable",
                                                  VariableResolutionWrapper.VariableBinding
                                                      (createTestNode testCapturedVariableBinding) ]
                                                |> Map.ofList
                                            Name = "fooParameter"
                                            Value =
                                                AstNode.TypedValue
                                                    (createTestNode
                                                        (typedValueWrapperType,
                                                         AstNode.TypedVariable
                                                             (createTestNode ("fooVariable", typedVariableBindingType)))) |})

                                .Returns({ testCapturedVariableBinding with
                                               Name = "fooParameter"
                                               Type = typedValueWrapperType }
                                         |> createTestNode
                                         |> AstNode.VariableBinding
                                         |> GslResult.ok: GslResult<AstNode, FunctionInliningError>)
                                .SetName(sprintf
                                             "Function inlining type check. Captured=%A TypedValue=%A VariableBinding=%A"
                                             testCapturedVariableBindingType
                                             typedValueWrapperType
                                             typedVariableBindingType)

        } :> IEnumerable

[<TestCaseSource(typeof<TestCasesForVariableBindingFromTypedValueAndName>,
                 "TestCasesForVariableBindingFromTypedValueAndName")>]
let testVariableBindingFromTypedValueAndName (input: {| Bindings: CapturedVariableBindings
                                                        Name: string
                                                        Value: AstNode |})
                                             : GslResult<AstNode, FunctionInliningError> =
    Inlining.variableBindingFromTypedValueAndName input.Bindings (input.Name, input.Value)
