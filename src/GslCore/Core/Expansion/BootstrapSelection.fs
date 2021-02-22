namespace GslCore.Core.Expansion

open GslCore.Ast.ErrorHandling
open GslCore.Ast.Types
open GslCore.Ast.Algorithms
open GslCore.GslResult




// ==================================
// determining which bootstrapped expansion to run
// ==================================

type BootstrapExpansionMode =
    | ExpandMutation
    | ExpandProtein
    | ExpandHetBlock

module BootstrapExpansionMode =
    let internal expansionPriority mode =
        match mode with
        | ExpandHetBlock -> 5
        | ExpandProtein -> 10
        | ExpandMutation -> 20

    let internal prioritize mode1 mode2 =
        if expansionPriority mode1 > expansionPriority mode2
        then mode1
        else mode2

    /// Given a node, determine what expansion step it requires to continue.
    let internal expansionMode node =
        match node with
        | Mutation _ -> Some(ExpandMutation)
        | InlineProtein _ -> Some(ExpandProtein)
        | HetBlock _ -> Some(ExpandHetBlock)
        | _ -> None

module BoostrapSelection =
    /// Given an AST, determine the highest priority of expansion needed to continue.
    let tryGetExpansionMode tree =
        let expansionsNeeded =
            tree
            |> AstNode.traverse
            |> Seq.map BootstrapExpansionMode.expansionMode
            |> Seq.choose id
            |> Set.ofSeq

        if expansionsNeeded.IsEmpty
        then None
        else Some(expansionsNeeded |> Seq.reduce BootstrapExpansionMode.prioritize)

    ///<summary>
    /// We only want to run the assemblies that require expansion through the bootstrapper.
    /// Wrap a bootstrap operation in a check that passes the existing node through if it doesn't need
    /// the current expansion step.
    ///</summary>
    let maybeBypassBootstrap mode bootstrapOperation state (node: AstNode) =
        let modesRequiredByThisNode =
            AstTreeHead(node)
            |> AstNode.traverse
            |> Seq.choose BootstrapExpansionMode.expansionMode
            |> Set.ofSeq

        if modesRequiredByThisNode.Contains(mode) then
            // this node needs to be bootstrapped, run it
            bootstrapOperation state node
        else
            GslResult.ok node