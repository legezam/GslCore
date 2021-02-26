namespace GslCore.Core.Expansion

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
    let internal getExpansionPriority mode =
        match mode with
        | ExpandHetBlock -> 5
        | ExpandProtein -> 10
        | ExpandMutation -> 20

    let internal prioritize mode1 mode2 =
        if getExpansionPriority mode1 > getExpansionPriority mode2 then
            mode1
        else
            mode2

    /// Given a node, determine what expansion step it requires to continue.
    let internal tryGetRequiredExpansionMode node =
        match node with
        | AstNode.Mutation _ -> Some(ExpandMutation)
        | AstNode.InlineProtein _ -> Some(ExpandProtein)
        | AstNode.HetBlock _ -> Some(ExpandHetBlock)
        | _ -> None

module BoostrapSelection =
    /// Given an AST, determine the highest priority of expansion needed to continue.
    let tryGetExpansionMode tree =
        let expansionsNeeded =
            tree
            |> AstNode.traverse
            |> Seq.map BootstrapExpansionMode.tryGetRequiredExpansionMode
            |> Seq.choose id
            |> Set.ofSeq

        if expansionsNeeded.IsEmpty then
            None
        else
            Some
                (expansionsNeeded
                 |> Seq.reduce BootstrapExpansionMode.prioritize)

    ///<summary>
    /// We only want to run the assemblies that require expansion through the bootstrapper.
    /// Wrap a bootstrap operation in a check that passes the existing node through if it doesn't need
    /// the current expansion step.
    ///</summary>
    let maybeBypassBootstrap (mode: BootstrapExpansionMode)
                             (bootstrapOperation: 'a -> AstNode -> GslResult<AstNode, 'b>)
                             (state: 'a)
                             (node: AstNode)
                             : GslResult<AstNode, 'b> =
        let modesRequiredByThisNode =
            AstTreeHead(node)
            |> AstNode.traverse
            |> Seq.choose BootstrapExpansionMode.tryGetRequiredExpansionMode
            |> Set.ofSeq

        if modesRequiredByThisNode.Contains(mode) then
            // this node needs to be bootstrapped, run it
            bootstrapOperation state node
        else
            GslResult.ok node
