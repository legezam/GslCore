module GslCore.Ast.Process.Cleanup

open Amyris.ErrorHandling
open GslCore.Ast.Types
open GslCore.Ast.Algorithms

// =========================
// stripping all non-literals from a tree
// =========================

// there are some phases where we want to clean a tree by removing certain kinds of nodes
// these functions are defined here

/// Match only function declarations.
let private cleanFunction node =
    match node with
    | FunctionDef _ -> None
    | _ -> Some node

/// Match only variable declarations
let private cleanVariable node =
    match node with
    | VariableBinding _ -> None
    | _ -> Some node

/// Clean function defintions and variable bindings from blocks.
let private cleanBlock cleaner node =
    match node with
    | Block blockWrapper ->
        let newBlockContents =
            blockWrapper.Value |> List.choose cleaner

        Block
            ({ blockWrapper with
                   Value = newBlockContents })
    | _ -> node

/// Strip function defintions from tree.
let stripFunctions =
    FoldMap.map Serial TopDown (promote (cleanBlock cleanFunction))

/// Strip variable bindings from tree.
let stripVariables =
    FoldMap.map Serial TopDown (promote (cleanBlock cleanVariable))
