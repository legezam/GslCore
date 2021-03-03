module GslCore.Tests.Ast.Process.VariableCapturingTest

open GslCore.Ast.Process.VariableResolution
open GslCore.Ast.Types
open NUnit.Framework
open GslCore.Ast.Algorithms

let testVariableBindingNode =
    { Node.Value =
          { VariableBinding.Name = "foo"
            Type = GslVariableType.StringType
            Value = AstNode.Int({ Node.Value = 10; Positions = [] }) }
      Positions = [] }

[<Test>]
let ``captureVariableBindings captures VariableBinding in PreTransform mode`` () =
    let binding = testVariableBindingNode

    let inputWrapper = AstNode.VariableBinding binding
    let bindings = Map.empty
    let mode = PreTransform

    let result =
        VariableCapturing.captureVariableBindings mode bindings inputWrapper

    let expectedResult =
        [ testVariableBindingNode.Value.Name, VariableResolutionWrapper.VariableBinding binding ]
        |> Map.ofList

    Assert.AreEqual(expectedResult, result)

[<Test>]
let ``captureVariableBindings does not capture VariableBinding in PostTransform mode`` () =
    let binding = testVariableBindingNode

    let inputWrapper = AstNode.VariableBinding binding
    let bindings = Map.empty
    let mode = PostTransform

    let result =
        VariableCapturing.captureVariableBindings mode bindings inputWrapper

    let expectedResult = Map.empty

    Assert.AreEqual(expectedResult, result)


let testIrrelevantNode = AstNode.Int({ Node.Value = 10; Positions = [] })

[<Test>]
let ``captureVariableBindings does not capture irrelevant node in PostTransform mode`` () =

    let inputWrapper = testIrrelevantNode
    let bindings = Map.empty
    let mode = PostTransform

    let result =
        VariableCapturing.captureVariableBindings mode bindings inputWrapper

    let expectedResult = Map.empty

    Assert.AreEqual(expectedResult, result)

let testSelfReferencingVariableBindingNode =
    { Node.Value =
          { VariableBinding.Name = "foo"
            Type = GslVariableType.StringType
            Value =
                AstNode.TypedVariable
                    { Value = "foo", GslVariableType.IntType
                      Positions = [] } }
      Positions = [] }

[<Test>]
let ``captureVariableBindings does not capture self-referencing VariableBinding in PreTransform mode`` () =
    let binding = testSelfReferencingVariableBindingNode

    let inputWrapper = AstNode.VariableBinding binding
    let bindings = Map.empty
    let mode = PreTransform

    let result =
        VariableCapturing.captureVariableBindings mode bindings inputWrapper

    let expectedResult = Map.empty

    Assert.AreEqual(expectedResult, result)

let testFunctionLocals =
    { Node.Value = { FunctionLocals.Names = [ "bar"; "baz" ] }
      Positions = [] }

[<Test>]
let ``captureVariableBindings captures FunctionLocals in PreTransform mode`` () =
    let binding = testFunctionLocals

    let inputWrapper = AstNode.FunctionLocals binding
    let bindings = Map.empty
    let mode = PreTransform

    let result =
        VariableCapturing.captureVariableBindings mode bindings inputWrapper

    let expectedResult =
        binding.Value.Names
        |> List.map (fun name -> name, VariableResolutionWrapper.FunctionLocal)
        |> Map.ofList

    Assert.AreEqual(expectedResult, result)

[<Test>]
let ``captureVariableBindings does not capture FunctionLocals in PostTransform mode`` () =
    let binding = testFunctionLocals

    let inputWrapper = AstNode.FunctionLocals binding
    let bindings = Map.empty
    let mode = PostTransform

    let result =
        VariableCapturing.captureVariableBindings mode bindings inputWrapper

    let expectedResult = Map.empty

    Assert.AreEqual(expectedResult, result)
