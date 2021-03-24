module GslCore.Tests.Ast.Process.ExpressionReductionTest

open System.Collections
open GslCore.Ast.Process.ExpressionReduction
open GslCore.Ast.Process.Inlining
open GslCore.Ast.Types
open GslCore.GslResult
open GslCore.Tests.Ast.Process
open NUnit.Framework

let createTestNode (value: 'a): Node<'a> = { Node.Value = value; Positions = [] }

type TestCasesForReduceExpressions() =
    static member TestCasesForReduceExpressions: IEnumerable =
        seq {
            // INT + INT
            TestCaseData(AstNode.BinaryOperation
                             (createTestNode
                                 { BinaryOperation.Operator = Add
                                   Left = AstNode.Int(createTestNode 4)
                                   Right = AstNode.Int(createTestNode 2) }))
                .Returns(GslResult.ok (AstNode.Int(createTestNode 6)): GslResult<AstNode, ExpressionReductionError>)
                .SetName("4 + 2 = 6")

            TestCaseData(AstNode.BinaryOperation
                             (createTestNode
                                 { BinaryOperation.Operator = Subtract
                                   Left = AstNode.Int(createTestNode 4)
                                   Right = AstNode.Int(createTestNode 2) }))
                .Returns(GslResult.ok (AstNode.Int(createTestNode 2)): GslResult<AstNode, ExpressionReductionError>)
                .SetName("4 - 2 = 2")

            TestCaseData(AstNode.BinaryOperation
                             (createTestNode
                                 { BinaryOperation.Operator = Multiply
                                   Left = AstNode.Int(createTestNode 4)
                                   Right = AstNode.Int(createTestNode 2) }))
                .Returns(GslResult.ok (AstNode.Int(createTestNode 8)): GslResult<AstNode, ExpressionReductionError>)
                .SetName("4 * 2 = 8")

            TestCaseData(AstNode.BinaryOperation
                             (createTestNode
                                 { BinaryOperation.Operator = Add
                                   Left = AstNode.Int(createTestNode 4)
                                   Right = AstNode.Int(createTestNode 2) }))
                .Returns(GslResult.ok (AstNode.Int(createTestNode 6)): GslResult<AstNode, ExpressionReductionError>)
                .SetName("4 + 2 = 6")

            TestCaseData(AstNode.BinaryOperation
                             (createTestNode
                                 { BinaryOperation.Operator = Divide
                                   Left = AstNode.Int(createTestNode 4)
                                   Right = AstNode.Int(createTestNode 2) }))
                .Returns(GslResult.ok (AstNode.Int(createTestNode 2)): GslResult<AstNode, ExpressionReductionError>)
                .SetName("4 / 2 = 2")

            // STRING + STRING
            TestCaseData(AstNode.BinaryOperation
                             (createTestNode
                                 { BinaryOperation.Operator = Add
                                   Left = AstNode.String(createTestNode "foo")
                                   Right = AstNode.String(createTestNode "bar") }))
                .Returns(GslResult.ok (AstNode.String(createTestNode "foobar")): GslResult<AstNode, ExpressionReductionError>)
                .SetName("foo + bar = foobar")

            let operation =
                AstNode.BinaryOperation
                    (createTestNode
                        { BinaryOperation.Operator = Subtract
                          Left = AstNode.String(createTestNode "foo")
                          Right = AstNode.String(createTestNode "bar") })

            TestCaseData(operation)
                .Returns(GslResult.err (ExpressionReductionError.UnsupportedStringOperation(Subtract, operation)): GslResult<AstNode, ExpressionReductionError>)
                .SetName("foo - bar = error")

            let operation =
                AstNode.BinaryOperation
                    (createTestNode
                        { BinaryOperation.Operator = Multiply
                          Left = AstNode.String(createTestNode "foo")
                          Right = AstNode.String(createTestNode "bar") })

            TestCaseData(operation)
                .Returns(GslResult.err (ExpressionReductionError.UnsupportedStringOperation(Multiply, operation)): GslResult<AstNode, ExpressionReductionError>)
                .SetName("foo * bar = error")

            let operation =
                AstNode.BinaryOperation
                    (createTestNode
                        { BinaryOperation.Operator = Divide
                          Left = AstNode.String(createTestNode "foo")
                          Right = AstNode.String(createTestNode "bar") })

            TestCaseData(operation)
                .Returns(GslResult.err (ExpressionReductionError.UnsupportedStringOperation(Divide, operation)): GslResult<AstNode, ExpressionReductionError>)
                .SetName("foo / bar = error")

            // FLOAT + FLOAT
            TestCaseData(AstNode.BinaryOperation
                             (createTestNode
                                 { BinaryOperation.Operator = Add
                                   Left = AstNode.Float(createTestNode 4.0)
                                   Right = AstNode.Float(createTestNode 2.0) }))
                .Returns(GslResult.ok (AstNode.Float(createTestNode 6.0)): GslResult<AstNode, ExpressionReductionError>)
                .SetName("4.0 + 2.0 = 6.0")

            TestCaseData(AstNode.BinaryOperation
                             (createTestNode
                                 { BinaryOperation.Operator = Subtract
                                   Left = AstNode.Float(createTestNode 4.0)
                                   Right = AstNode.Float(createTestNode 2.0) }))
                .Returns(GslResult.ok (AstNode.Float(createTestNode 2.0)): GslResult<AstNode, ExpressionReductionError>)
                .SetName("4.0 - 2.0 = 2.0")

            TestCaseData(AstNode.BinaryOperation
                             (createTestNode
                                 { BinaryOperation.Operator = Multiply
                                   Left = AstNode.Float(createTestNode 4.0)
                                   Right = AstNode.Float(createTestNode 2.0) }))
                .Returns(GslResult.ok (AstNode.Float(createTestNode 8.0)): GslResult<AstNode, ExpressionReductionError>)
                .SetName("4.0 * 2.0 = 8.0")

            TestCaseData(AstNode.BinaryOperation
                             (createTestNode
                                 { BinaryOperation.Operator = Divide
                                   Left = AstNode.Float(createTestNode 4.0)
                                   Right = AstNode.Float(createTestNode 2.0) }))
                .Returns(GslResult.ok (AstNode.Float(createTestNode 2.0)): GslResult<AstNode, ExpressionReductionError>)
                .SetName("4.0 / 2.0 = 2.0")

            // INT + FLOAT
            TestCaseData(AstNode.BinaryOperation
                             (createTestNode
                                 { BinaryOperation.Operator = Add
                                   Left = AstNode.Int(createTestNode 4)
                                   Right = AstNode.Float(createTestNode 2.0) }))
                .Returns(GslResult.ok (AstNode.Float(createTestNode 6.0)): GslResult<AstNode, ExpressionReductionError>)
                .SetName("4 + 2.0 = 6.0")

            TestCaseData(AstNode.BinaryOperation
                             (createTestNode
                                 { BinaryOperation.Operator = Subtract
                                   Left = AstNode.Int(createTestNode 4)
                                   Right = AstNode.Float(createTestNode 2.0) }))
                .Returns(GslResult.ok (AstNode.Float(createTestNode 2.0)): GslResult<AstNode, ExpressionReductionError>)
                .SetName("4 - 2.0 = 2.0")

            TestCaseData(AstNode.BinaryOperation
                             (createTestNode
                                 { BinaryOperation.Operator = Multiply
                                   Left = AstNode.Int(createTestNode 4)
                                   Right = AstNode.Float(createTestNode 2.0) }))
                .Returns(GslResult.ok (AstNode.Float(createTestNode 8.0)): GslResult<AstNode, ExpressionReductionError>)
                .SetName("4 * 2.0 = 8.0")

            TestCaseData(AstNode.BinaryOperation
                             (createTestNode
                                 { BinaryOperation.Operator = Divide
                                   Left = AstNode.Int(createTestNode 4)
                                   Right = AstNode.Float(createTestNode 2.0) }))
                .Returns(GslResult.ok (AstNode.Float(createTestNode 2.0)): GslResult<AstNode, ExpressionReductionError>)
                .SetName("4 / 2.0 = 2.0")

            // FLOAT + INT
            TestCaseData(AstNode.BinaryOperation
                             (createTestNode
                                 { BinaryOperation.Operator = Add
                                   Left = AstNode.Float(createTestNode 4.0)
                                   Right = AstNode.Int(createTestNode 2) }))
                .Returns(GslResult.ok (AstNode.Float(createTestNode 6.0)): GslResult<AstNode, ExpressionReductionError>)
                .SetName("4.0 + 2 = 6.0")

            TestCaseData(AstNode.BinaryOperation
                             (createTestNode
                                 { BinaryOperation.Operator = Subtract
                                   Left = AstNode.Float(createTestNode 4.0)
                                   Right = AstNode.Int(createTestNode 2) }))
                .Returns(GslResult.ok (AstNode.Float(createTestNode 2.0)): GslResult<AstNode, ExpressionReductionError>)
                .SetName("4.0 - 2 = 4.0")

            TestCaseData(AstNode.BinaryOperation
                             (createTestNode
                                 { BinaryOperation.Operator = Multiply
                                   Left = AstNode.Float(createTestNode 4.0)
                                   Right = AstNode.Int(createTestNode 2) }))
                .Returns(GslResult.ok (AstNode.Float(createTestNode 8.0)): GslResult<AstNode, ExpressionReductionError>)
                .SetName("4.0 * 2 = 8.0")

            TestCaseData(AstNode.BinaryOperation
                             (createTestNode
                                 { BinaryOperation.Operator = Divide
                                   Left = AstNode.Float(createTestNode 4.0)
                                   Right = AstNode.Int(createTestNode 2) }))
                .Returns(GslResult.ok (AstNode.Float(createTestNode 2.0)): GslResult<AstNode, ExpressionReductionError>)
                .SetName("4.0 / 2 = 2.0")

            // NEGATION
            TestCaseData(AstNode.Negation(createTestNode (AstNode.Int(createTestNode 2))))
                .Returns(GslResult.ok (AstNode.Int(createTestNode -2)): GslResult<AstNode, ExpressionReductionError>)
                .SetName("neg 2 = -2")

            TestCaseData(AstNode.Negation(createTestNode (AstNode.Float(createTestNode 2.0))))
                .Returns(GslResult.ok (AstNode.Float(createTestNode -2.0)): GslResult<AstNode, ExpressionReductionError>)
                .SetName("neg 2.0 = -2.0")        

        } :> IEnumerable


[<TestCaseSource(typeof<TestCasesForReduceExpressions>, "TestCasesForReduceExpressions")>]
let reduceExpressions (input: AstNode): GslResult<AstNode, ExpressionReductionError> =
    ExpressionReduction.reduceMathExpression input
