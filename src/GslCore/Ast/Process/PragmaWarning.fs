namespace GslCore.Ast.Process.PragmaWarning

open GslCore.Ast.Types
open GslCore.Ast.Algorithms
open GslCore.GslResult
open GslCore.Pragma

// =======================
// collecting warning messages from pragmas
// =======================

type PragmaWarningError = PragmaWarningError of pragma: Pragma * node: AstNode

module PragmaWarning =
    let private collectWarningPragma (node: AstNode): GslResult<AstNode, PragmaWarningError> =
        match node with
        | Pragma pragma when pragma.Value |> Pragma.isWarning ->
            GslResult.warn (PragmaWarningError(pragma.Value, node)) node // add a warning into the message stream
        | _ -> GslResult.ok node

    /// Add warnings into the message stream for every #warn pragma in the tree.
    let collect =
        FoldMap.map Serial TopDown collectWarningPragma
