/// AST versions of biological expansions
namespace GslCore.Core

open GslCore.Ast.Phase1Message
open GslCore.Ast.Types
open GslCore.Ast.ErrorHandling
open GslCore.Core.Expansion
open GslCore.Core.Expansion.Bootstrapping
open GslCore.GslResult

type Phase2Error =
    | MutationExpansionError of BootstrapError<BootstrapError<Phase1Message>> * pass: int
    | ProteinExpansionError of BootstrapError<BootstrapError<Phase1Message>> * pass: int
    | HeterologyExpansionError of BootstrapError<BootstrapError<Phase1Message>> * pass: int
    | LimitExceeded of limit: int * node: AstNode

module Phase2Error =
    let makeMutationError pass error = MutationExpansionError(error, pass)
    let makeProteinError pass error = ProteinExpansionError(error, pass)
    let makeHeterologyError pass error = HeterologyExpansionError(error, pass)

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
module Phase2 =
    let phase2 (parameters: Phase2Parameters) (treeIn: AstTreeHead): GslResult<AstTreeHead, Phase2Error> =

        let runPhase2 (pass: int) (mode: BootstrapExpansionMode) (tree: AstTreeHead): GslResult<AstTreeHead, Phase2Error> =
            match mode with
            | ExpandMutation ->
                MutationExpansion.expandMutations parameters tree
                |> GslResult.mapError (Phase2Error.makeMutationError pass)
            | ExpandProtein ->
                ProteinExpansion.expandInlineProteins parameters tree
                |> GslResult.mapError (Phase2Error.makeProteinError pass)
            | ExpandHetBlock ->
                HeterologyExpansion.expandHetBlocks parameters tree
                |> GslResult.mapError (Phase2Error.makeHeterologyError pass)

        let rec doPhase2 (passNumber: int) (tree: AstTreeHead) =
            match parameters.MaxPasses with
            | Some limit when passNumber > limit -> GslResult.err (LimitExceeded(limit, tree.wrappedNode))
            | _ -> // otherwise, run the expansion step
                match BoostrapSelection.tryGetExpansionMode tree with
                | Some mode ->
                    runPhase2 passNumber mode tree
                    >>= doPhase2 (passNumber + 1)
                | None -> GslResult.ok tree

        // if we just want to expand one step and re-emit literal source code
        if parameters.OneShot then
            match BoostrapSelection.tryGetExpansionMode treeIn with
            | Some mode -> runPhase2 1 mode treeIn
            | None -> GslResult.ok treeIn
        else
            doPhase2 0 treeIn
