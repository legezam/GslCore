namespace GslCore.Legacy

open GslCore.Ast.ErrorHandling
open GslCore.GslResult
open GslCore.Pragma
open GslCore.Ast.Types
open GslCore.Ast.Process.AssemblyStuffing
open GslCore.Legacy.Types

type LegacyL2LineCreationError =
    | IllegalIncomingL2ElementContent of left: AstNode * right: AstNode * node: AstNode
    | IllegalIncomingL2Element of node: AstNode
    | FailedLocusUnpacking of node: AstNode

// ======================
// conversion from L2 AST node to legacy L2 line type
// ======================
module LegacyL2Conversion =
    /// Build a concrete L2 element from an AST node.
    let private buildL2Element (node: AstNode): GslResult<BuiltL2Element, LegacyL2LineCreationError> =
        match node with
        | AstNode.L2Element (nw) ->
            match nw.Value.Promoter, nw.Value.Target with
            | AstNode.L2Id _, AstNode.L2Id (tw)
            | AstNode.Part _, AstNode.L2Id (tw) ->
                GslResult.ok
                    { Promoter = nw.Value.Promoter
                      Target = tw.Value }

            | x, y -> GslResult.err (IllegalIncomingL2ElementContent(x, y, node))
        | x -> GslResult.err (IllegalIncomingL2Element x)

    let private unpackLocus (maybeNode: AstNode option): GslResult<L2Id option, LegacyL2LineCreationError> =
        match maybeNode with
        | Some (AstNode.L2Id lw) -> GslResult.ok (Some(lw.Value))
        | Some x -> GslResult.err (FailedLocusUnpacking x)
        | None -> GslResult.ok None

    /// Build a concrete L2 expression from an AST node.
    let private buildL2Expression (expressionWrapper: Node<L2Expression>)
                                  : GslResult<BuiltL2Expression, LegacyL2LineCreationError> =
        let parts =
            expressionWrapper.Value.Parts
            |> List.map buildL2Element
            |> GslResult.collectA

        let locus =
            unpackLocus expressionWrapper.Value.Locus

        (locus, parts)
        ||> GslResult.map2 (fun locus parts -> { L2Locus = locus; Parts = parts })

    /// Build a L2Line from an AST node and pragma environment.
    let convertL2Line (pragmaEnv: PragmaEnvironment)
                      (l2Expression: Node<L2Expression>)
                      : GslResult<L2Line, LegacyL2LineCreationError> =
        let pragmas =
            pragmaEnv.Persistent
            |> PragmaCollection.mergeInCollection pragmaEnv.AssignedTransients

        let name =
            pragmas
            |> PragmaCollection.tryGetValue BuiltIn.namePragmaDef

        let uri =
            pragmas
            |> PragmaCollection.tryGetValue BuiltIn.uriPragmaDef

        buildL2Expression l2Expression
        |> GslResult.map (fun l2Design ->
            { L2Line.L2Design = l2Design
              Name = name
              Uri = uri
              Pragmas = pragmas
              Capabilities = pragmaEnv.Capabilities })
