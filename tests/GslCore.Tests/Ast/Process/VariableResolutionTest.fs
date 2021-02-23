module GslCore.Tests.Ast.Process.VariableResolutionTest

open System.Collections
open GslCore.Ast.Process
open GslCore.Ast.Types
open NUnit.Framework
open GslCore.Ast.Process.VariableResolution

type TestCasesForTypeElision() =
    static member TestCasesForTypeElision: IEnumerable =
        seq {
            TestCaseData(Part
                             { Node.Value =
                                   { ParsePart.BasePart = Int({ Value = 12; Positions = [] })
                                     Modifiers = []
                                     Pragmas = []
                                     IsForward = false }
                               Positions = [] })
                .Returns(Some PartType)

            TestCaseData(FunctionLocals
                             { Node.Value = { FunctionLocals.Names = [ "bar"; "baz" ] }
                               Positions = [] })
                .Returns(None)

            TestCaseData(Int({ Value = 12; Positions = [] }))
                .Returns(Some IntType)

            TestCaseData(Float({ Value = 12.0; Positions = [] }))
                .Returns(Some FloatType)

            TestCaseData(String({ Value = "foo"; Positions = [] }))
                .Returns(Some StringType)
        } :> IEnumerable

[<TestCaseSource(typeof<TestCasesForTypeElision>, "TestCasesForTypeElision")>]
let testTypeElision (node: AstNode): GslVariableType option =
    VariableResolution.elideType node
