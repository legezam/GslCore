module GslCore.Tests.Ast.Process.FunctionCapturingTest

open System.Collections
open GslCore.Ast.Process.Inlining

open GslCore.Ast.Types
open NUnit.Framework
open GslCore.Ast.Algorithms

let testParseFunction =
    { ParseFunction.Name = "foo"
      ArgumentNames = []
      Body = AstNode.String { Value = "foo"; Positions = [] } }

let testParseFunctionNode =
    { Node.Value = testParseFunction
      Positions = [] }

let testParseFunctionAstNode =
    AstNode.FunctionDef testParseFunctionNode

type TestCasesForFunctionCapturing() =
    static member TestCasesForFunctionCapturing: IEnumerable =
        seq {

            TestCaseData({| Node = testParseFunctionAstNode
                            Mode = PreTransform
                            State =
                                { FunctionInliningState.Definitions = Map.empty
                                  Variables = Map.empty
                                  Depth = 0 } |})
                .Returns({ FunctionInliningState.Variables = Map.empty
                           Definitions =
                               FunctionInliningState.empty.Definitions
                               |> Map.add testParseFunction.Name testParseFunction
                           Depth = 1 })
                .SetName("Depth increasing and definition is added")

            TestCaseData({| Node = testParseFunctionAstNode
                            Mode = PostTransform
                            State =
                                { FunctionInliningState.Definitions = Map.empty
                                  Variables = Map.empty
                                  Depth = 1 } |})
                .Returns({ FunctionInliningState.Variables = Map.empty
                           Definitions = Map.empty
                           Depth = 0 })
                .SetName("Depth is decreasing in post transform")

            TestCaseData({| Node = AstNode.Int { Node.Value = 3; Positions = [] }
                            Mode = PreTransform
                            State =
                                { FunctionInliningState.Definitions = Map.empty
                                  Variables = Map.empty
                                  Depth = 0 } |})
                .Returns({ FunctionInliningState.Variables = Map.empty
                           Definitions = Map.empty
                           Depth = 0 })
                .SetName("Ignores non parsefunction nodes")

            let updatedParseFunction =
                { testParseFunction with
                      ArgumentNames = [ "baz" ] }

            TestCaseData({| Node =
                                AstNode.FunctionDef
                                    { Node.Value = updatedParseFunction
                                      Positions = [] }
                            Mode = PreTransform
                            State =
                                { FunctionInliningState.Definitions =
                                      Map.empty
                                      |> Map.add testParseFunction.Name testParseFunction
                                  Variables = Map.empty
                                  Depth = 0 } |})
                .Returns({ FunctionInliningState.Variables = Map.empty
                           Definitions =
                               FunctionInliningState.empty.Definitions
                               |> Map.add testParseFunction.Name updatedParseFunction
                           Depth = 1 })
                .SetName("Collected definition get overwritten if another definition with the same name exists")
        } :> IEnumerable

[<TestCaseSource(typeof<TestCasesForFunctionCapturing>, "TestCasesForFunctionCapturing")>]
[<Category("Phase1")>]
let testFunctionCapturing (parameters: {| Node: AstNode
                                          Mode: StateUpdateMode
                                          State: FunctionInliningState |})
                          : FunctionInliningState =
    Inlining.collectFunctionDefinition parameters.Mode parameters.State parameters.Node
