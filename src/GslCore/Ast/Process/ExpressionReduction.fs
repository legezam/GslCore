namespace GslCore.Ast.Process.ExpressionReduction

open GslCore.Ast.Types

open GslCore.Ast.Algorithms
open GslCore.GslResult

// =====================
// simplification of binary expressions
// =====================

type ExpressionReductionError =
    | ExpectedNumericVariable of node: AstNode * foundType: GslVariableType
    | TypeIsNotAllowedInBinaryExpression of node: AstNode
    | TypeIsNotAllowedInNegationExpression of node: AstNode

module ExpressionReduction =

    /// Reduce a fully specified binary expression into a single node.
    /// Also collapse negations while we're at it.  If we find something we can't negate, return an error.
    let private reduceMathExpression (node: AstNode): GslResult<AstNode, ExpressionReductionError> =
        match node with
        | AstNode.BinaryOperation ({ Node.Value = binaryOperation
                                     Positions = positions }) ->
            match binaryOperation.Left, binaryOperation.Right with
            | AstNode.Int left, AstNode.Int right ->
                // two concrete integers, we can operate on them
                let result =
                    match binaryOperation.Operator with
                    | Add -> left.Value + right.Value
                    | Subtract -> left.Value - right.Value
                    | Multiply -> left.Value * right.Value
                    | Divide -> left.Value / right.Value

                GslResult.ok
                    (AstNode.Int
                        ({ Value = result
                           Positions = positions }))
            // If we don't have two ints (because one or both are still variables), we can't reduce but
            // this is an OK state of affairs.
            | AllowedInMathExpression _, AllowedInMathExpression _ -> GslResult.ok node
            // One node is disallowed in a math expression, oh my.
            | AllowedInMathExpression _, illegal
            | illegal, AllowedInMathExpression _ -> GslResult.err (TypeIsNotAllowedInBinaryExpression illegal)
            // Neither node is allowed here.  Wow, we sure screwed up somewhere.
            | illegalLeft, illegalRight ->
                GslResult.err (TypeIsNotAllowedInBinaryExpression illegalLeft)
                |> GslResult.addMessages [ TypeIsNotAllowedInBinaryExpression illegalRight ]
        | AstNode.Negation ({ Value = inner; Positions = pos }) ->
            match inner with
            | AstNode.Int ({ Value = i; Positions = _ }) ->
                let v = -1 * i
                GslResult.ok (AstNode.Int({ Value = v; Positions = pos }))
            | AstNode.Float ({ Value = i; Positions = _ }) ->
                let v = -1.0 * i
                GslResult.ok (AstNode.Float({ Value = v; Positions = pos }))
            // If we have a variable, it should be numeric.  If so, we're ok
            | IntVariable _
            | FloatVariable _ -> GslResult.ok node
            // Non-numeric variable.  We're in trouble.
            | OtherVariable foundType -> GslResult.err (ExpectedNumericVariable(inner, foundType))
            | NotAVariable -> GslResult.err (TypeIsNotAllowedInNegationExpression inner)
        | _ -> GslResult.ok node

    let reduceMathExpressions =
        FoldMap.map Serial BottomUp reduceMathExpression
