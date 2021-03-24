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
    | UnsupportedStringOperation of providedOperator: BinaryOperator * node: AstNode

module ExpressionReduction =

    /// Reduce a fully specified binary expression into a single node.
    /// Also collapse negations while we're at it.  If we find something we can't negate, return an error.
    let internal reduceMathExpression (node: AstNode): GslResult<AstNode, ExpressionReductionError> =
        match node with
        | AstNode.BinaryOperation { Node.Value = binaryOperation
                                    Positions = positions } ->
            match binaryOperation.Left, binaryOperation.Right with
            | AstNode.Int left, AstNode.Int right ->
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
            | AstNode.Float left, AstNode.Float right ->
                let result =
                    match binaryOperation.Operator with
                    | Add -> left.Value + right.Value
                    | Subtract -> left.Value - right.Value
                    | Multiply -> left.Value * right.Value
                    | Divide -> left.Value / right.Value

                GslResult.ok
                    (AstNode.Float
                        ({ Value = result
                           Positions = positions }))
            | AstNode.Float leftFloat, AstNode.Int rightInt ->
                let result =
                    let right = float rightInt.Value
                    let left = leftFloat.Value

                    match binaryOperation.Operator with
                    | Add -> left + right
                    | Subtract -> left - right
                    | Multiply -> left * right
                    | Divide -> left / right

                GslResult.ok
                    (AstNode.Float
                        ({ Value = result
                           Positions = positions }))
            | AstNode.Int leftInt, AstNode.Float rightFloat ->
                let result =
                    let right = rightFloat.Value
                    let left = float leftInt.Value

                    match binaryOperation.Operator with
                    | Add -> left + right
                    | Subtract -> left - right
                    | Multiply -> left * right
                    | Divide -> left / right

                GslResult.ok
                    (AstNode.Float
                        ({ Value = result
                           Positions = positions }))

            | AstNode.String left, AstNode.String right ->
                match binaryOperation.Operator with
                | Add ->
                    let result = left.Value + right.Value

                    GslResult.ok
                        (AstNode.String
                            { Node.Value = result
                              Positions = positions })
                | unsupportedOperator -> GslResult.err (UnsupportedStringOperation(unsupportedOperator, node))

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
