/// AST versions of biological expansions
namespace GslCore.Core

open GslCore.Ast.Types
open GslCore.Ast.ErrorHandling
open GslCore.Core.Expansion
open GslCore.Core.PluginTypes
open GslCore.GslResult
open GslCore.Pragma
open GslCore.Reference

// ===========================
// putting it all together: recursively expanding a post-phase-1 tree until we're done
// ===========================

/// Perform phase 2 compilation on a post-phase-1 tree.
/// This step is recursive and will execute until all expansions are
/// complete, or we hit the limit on recursion optionally set by maxPasses.
/// This ensures we catch misbehaving expansion steps in a clear fashion.
/// Well-behaved expansion should never create an expansion phase higher than itself,
/// so we should probably have a hard limit at N^2, where N is the number of nontrivial expansion
/// passes.
// FIXME: way too many arguments
module Phase2 =
    let phase2 (parameters: Phase2Parameters) (treeIn: AstTreeHead): AstResult<AstTreeHead> =

        let runPhase2 mode tree =
            match mode with
            | ExpandMutation -> MutationExpansion.expandMutations parameters tree
            | ExpandProtein -> ProteinExpansion.expandInlineProteins parameters tree
            | ExpandHetBlock -> HeterologyExpansion.expandHetBlocks parameters tree

        let rec doPhase2 passNumber (tree: AstTreeHead) =
            match parameters.MaxPasses with
            | Some (limit) when passNumber > limit -> // if we're past a limit passed in, fail.
                AstResult.errStringF
                    (InternalError(GeneralError))
                    "Compiler phase 2 hit recursion limit of %d."
                    limit
                    tree.wrappedNode
            | _ -> // otherwise, run the expansion step
                match BoostrapSelection.tryGetExpansionMode tree with
                | Some mode -> runPhase2 mode tree >>= doPhase2 (passNumber + 1)
                | None -> GslResult.ok tree

        // if we just want to expand one step and re-emit literal source code
        if parameters.OneShot then
            match BoostrapSelection.tryGetExpansionMode treeIn with
            | Some mode -> runPhase2 mode treeIn
            | None -> GslResult.ok treeIn
        else
            doPhase2 0 treeIn
