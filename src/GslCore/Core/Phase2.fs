/// AST versions of biological expansions
namespace GslCore.Core

open GslCore.Ast.ErrorHandling
open GslCore.Ast.Phase1Message
open GslCore.Ast.Types
open GslCore.Core.Expansion
open GslCore.Core.Expansion.Bootstrapping
open GslCore.GslResult

type Phase2Error =
    | MutationExpansionError of
        BootstrapExecutionError<BootstrapExpandAssemblyError<BootstrapError<Phase1Error>>> *
        pass: int
    | ProteinExpansionError of
        BootstrapExecutionError<BootstrapExpandAssemblyError<BootstrapError<Phase1Error>>> *
        pass: int
    | HeterologyExpansionError of
        BootstrapExecutionError<BootstrapExpandAssemblyError<BootstrapError<Phase1Error>>> *
        pass: int
    | LimitExceeded of limit: int * node: AstNode

module Phase2Error =
    let makeMutationError pass error = MutationExpansionError(error, pass)
    let makeProteinError pass error = ProteinExpansionError(error, pass)
    let makeHeterologyError pass error = HeterologyExpansionError(error, pass)

    let toAstMessage: Phase2Error -> AstMessage =
        function
        // TODO previously we passed the error type into the inner bootstrapper function so the error returned became specific to the type of
        // the step being executed. This is now the opposite, we know from the error that where it came from so this would
        // be a good place to make the AstMessage be specific to the step being executed. Now skipping that as it doesn't
        // fit correctly here (i removed the exception catching completely as it is not supposed to be the way of handling errors here more on that soon.
        | MutationExpansionError (err, _pass)
        | HeterologyExpansionError (err, _pass)
        | ProteinExpansionError (err, _pass) ->
            err
            |> BootstrapExecutionError.toAstMessage
                (BootstrapExpandAssemblyError.toAstMessage (BootstrapError.toAstMessage Phase1Error.toAstMessage))


        | LimitExceeded (limit, node) ->
            AstResult.errStringFMsg
                (InternalError(GeneralError))
                "Compiler phase 2 hit recursion limit of %d."
                limit
                node

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
