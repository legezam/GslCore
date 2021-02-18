module GslCore.Ast.Process.ExpressionReduction

open Amyris.ErrorHandling
open GslCore.AstTypes
open GslCore.AstErrorHandling
open GslCore.AstAlgorithms

// =====================
// simplification of binary expressions
// =====================

/// Create an error message for a variable that isn't numeric.
let private numericVariableTypeError t node =
    AstMessage.createError TypeError (sprintf "Expecting a numeric variable type, but found %O." t) node


/// Reducde a fully specified binary expression into a single node.
/// Also collapse negations while we're at it.  If we find something we can't negate, return an error.
let private reduceMathExpression node =
    // convenience function for type errors we may come across
    let wrongTypeErrorMsg whichKind (n: AstNode) =
        sprintf "'%s' is not allowed to appear in a %s." n.TypeName whichKind

    let binOpErrMsg =
        wrongTypeErrorMsg "numeric binary operation"

    let negationErrMsg = wrongTypeErrorMsg "negation"

    match node with
    | BinaryOperation ({ Value = bo; Positions = pos }) ->
        match bo.Left, bo.Right with
        | Int (l), Int (r) ->
            // two concrete integers, we can operate on them
            let result =
                match bo.Operator with
                | Add -> l.Value + r.Value
                | Subtract -> l.Value - r.Value
                | Multiply -> l.Value * r.Value
                | Divide -> l.Value / r.Value

            ok (Int({ Value = result; Positions = pos }))
        // If we don't have two ints (because one or both are still variables), we can't reduce but
        // this is an OK state of affairs.
        | AllowedInMathExpression _, AllowedInMathExpression _ -> ok node
        // One node is disallowed in a math expression, oh my.
        | AllowedInMathExpression _, x
        | x, AllowedInMathExpression _ -> AstMessage.createError TypeError (binOpErrMsg x) x
        // Neither node is allowed here.  Wow, we sure screwed up somewhere.
        | x, y ->
            AstMessage.createError TypeError (binOpErrMsg x) x
            |> mergeMessages [ AstMessage.createErrorWithStackTrace TypeError (binOpErrMsg y) y ]
    | Negation ({ Value = inner; Positions = pos }) ->
        match inner with
        | Int ({ Value = i; Positions = _ }) ->
            let v = -1 * i
            ok (Int({ Value = v; Positions = pos }))
        | Float ({ Value = i; Positions = _ }) ->
            let v = -1.0 * i
            ok (Float({ Value = v; Positions = pos }))
        // If we have a variable, it should be numeric.  If so, we're ok
        | IntVariable _
        | FloatVariable _ -> ok node
        // Non-numeric variable.  We're in trouble.
        | OtherVariable t -> numericVariableTypeError t inner
        | NotAVariable -> AstMessage.createError TypeError (negationErrMsg inner) inner
    | _ -> ok node

let reduceMathExpressions = FoldMap.map Serial BottomUp reduceMathExpression