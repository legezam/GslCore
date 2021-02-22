module GslCore.Ast.Process.ExpressionReduction

open GslCore.Ast.Types
open GslCore.Ast.ErrorHandling
open GslCore.Ast.Algorithms
open GslCore.GslResult

// =====================
// simplification of binary expressions
// =====================

/// Create an error message for a variable that isn't numeric.
let private numericVariableTypeError t node =
    AstResult.errString TypeError (sprintf "Expecting a numeric variable type, but found %O." t) node


/// Reduce a fully specified binary expression into a single node.
/// Also collapse negations while we're at it.  If we find something we can't negate, return an error.
let private reduceMathExpression (node: AstNode): AstResult<AstNode> =
    // convenience function for type errors we may come across
    let wrongTypeErrorMsg (whichKind: string) (node: AstNode) =
        sprintf "'%s' is not allowed to appear in a %s." node.TypeName whichKind

    let binOpErrMsg =
        wrongTypeErrorMsg "numeric binary operation"

    let negationErrMsg = wrongTypeErrorMsg "negation"

    match node with
    | BinaryOperation ({ Node.Value = binaryOperation
                         Positions = positions }) ->
        match binaryOperation.Left, binaryOperation.Right with
        | Int left, Int right ->
            // two concrete integers, we can operate on them
            let result =
                match binaryOperation.Operator with
                | Add -> left.Value + right.Value
                | Subtract -> left.Value - right.Value
                | Multiply -> left.Value * right.Value
                | Divide -> left.Value / right.Value

            GslResult.ok
                (Int
                    ({ Value = result
                       Positions = positions }))
        // If we don't have two ints (because one or both are still variables), we can't reduce but
        // this is an OK state of affairs.
        | AllowedInMathExpression _, AllowedInMathExpression _ -> GslResult.ok node
        // One node is disallowed in a math expression, oh my.
        | AllowedInMathExpression _, illegal
        | illegal, AllowedInMathExpression _ -> AstResult.errString TypeError (binOpErrMsg illegal) illegal
        // Neither node is allowed here.  Wow, we sure screwed up somewhere.
        | illegalLeft, illegalRight ->
            let leftMessages =
                AstResult.errString TypeError (binOpErrMsg illegalLeft) illegalLeft

            let rightMessages =
                [ AstMessage.createErrorWithStackTrace TypeError (binOpErrMsg illegalRight) illegalRight ]

            leftMessages
            |> GslResult.addMessages rightMessages
    | Negation ({ Value = inner; Positions = pos }) ->
        match inner with
        | Int ({ Value = i; Positions = _ }) ->
            let v = -1 * i
            GslResult.ok (Int({ Value = v; Positions = pos }))
        | Float ({ Value = i; Positions = _ }) ->
            let v = -1.0 * i
            GslResult.ok (Float({ Value = v; Positions = pos }))
        // If we have a variable, it should be numeric.  If so, we're ok
        | IntVariable _
        | FloatVariable _ -> GslResult.ok node
        // Non-numeric variable.  We're in trouble.
        | OtherVariable t -> numericVariableTypeError t inner
        | NotAVariable -> AstResult.errString TypeError (negationErrMsg inner) inner
    | _ -> GslResult.ok node

let reduceMathExpressions =
    FoldMap.map Serial BottomUp reduceMathExpression
