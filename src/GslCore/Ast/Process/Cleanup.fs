module GslCore.Ast.Process.Cleanup

open GslCore.Ast.ErrorHandling
open GslCore.Ast.Types
open GslCore.Ast.Algorithms
open GslCore.GslResult

// =========================
// stripping all non-literals from a tree
// =========================

// there are some phases where we want to clean a tree by removing certain kinds of nodes
// these functions are defined here

/// Match only function declarations.
let private cleanFunction (node: AstNode): AstNode option =
    match node with
    | AstNode.FunctionDef _ -> None
    | _ -> Some node

/// Match only variable declarations
let private cleanVariable (node: AstNode): AstNode option =
    match node with
    | AstNode.VariableBinding _ -> None
    | _ -> Some node

/// Clean function definitions and variable bindings from blocks.
let private cleanBlock (cleaner: AstNode -> AstNode option) (node: AstNode): AstNode =
    match node with
    | AstNode.Block blockWrapper ->
        let newBlockContents =
            blockWrapper.Value |> List.choose cleaner

        AstNode.Block
            ({ blockWrapper with
                   Value = newBlockContents })
    | _ -> node

/// Strip function definitions from tree.
let stripFunctions: AstTreeHead -> GslResult<AstTreeHead, unit> =
    FoldMap.map Serial TopDown (GslResult.promote (cleanBlock cleanFunction))

/// Strip variable bindings from tree.
let stripVariables: AstTreeHead -> GslResult<AstTreeHead, unit> =
    FoldMap.map Serial TopDown (GslResult.promote (cleanBlock cleanVariable))
