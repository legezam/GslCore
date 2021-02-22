module GslCore.Ast.Process.PragmaWarning

open GslCore.Ast.Types
open GslCore.Ast.ErrorHandling
open GslCore.Ast.Algorithms
open GslCore.GslResult
open GslCore.Pragma

// =======================
// collecting warning messages from pragmas
// =======================

let private collectWarningPragma (node: AstNode): AstResult<AstNode> =
    match node with
    | Pragma pragma when pragma.Value |> Pragma.isWarning ->
        let msg =
            pragma.Value.Arguments |> String.concat " "

        let warnMsg = AstMessage.createWarning msg node
        GslResult.warn warnMsg node // add a warning into the message stream
    | _ -> GslResult.ok node

/// Add warnings into the message stream for every #warn pragma in the tree.
let collect =
    FoldMap.map Serial TopDown collectWarningPragma
