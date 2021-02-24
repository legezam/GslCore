namespace GslCore.Ast.Process.RoughageExpansion

open GslCore.Ast.Types
open GslCore.Ast.Algorithms
open GslCore.GslResult
open GslCore.Pragma


// ====================
// expanding inline roughage
// ====================

type RoughageExpansionError = ConstructHasIndeterminateLocus of node: AstNode

// the parser outputs inline roughage sections as blocks for convenience.
// we expand each individual line into block, possibly containing pragmas, and one L2 line.
// we need the pragma context to do this
module RoughageExpansion =
    let private validateRoughageLine (roughageWrapper: Node<Roughage>): GslResult<Node<Roughage>, RoughageExpansionError> =
        let roughage = roughageWrapper.Value
        // Rule 1:  must be able to work out the locus.  Locus can be either explicit (ho^) or
        //          implicit pSLN1>YNG1  but can't have just bidirectional promoters with no explicit locus  e.g.   ADH1<pGAL1-pGAL10>ADH2
        let hasLocus =
            roughage.Locus.IsSome
            || (roughage.Parts.Length > 0
                && not roughage.Parts.Head.Value.PromoterAndTarget2.IsSome)

        let node = AstNode.Roughage roughageWrapper

        if not hasLocus then
            GslResult.err (ConstructHasIndeterminateLocus node)
        else
            GslResult.ok roughageWrapper

    /// Roughage expands to Level 2 GSL.  We actually do this using the AST rather than bootstrapping.
    let private expandRoughage (roughageWrapper: Node<Roughage>): AstNode =
        let roughage = roughageWrapper.Value
        // TODO Hard coded mapping of markers for now
        let markerMapping (s: string) =
            match s with
            | "mURA" -> "ura3"
            | "mKANA" -> "kan"
            | "mLEU2" -> "leu2"
            | "mTRP1" -> "trp1"
            | "mURA3" -> "ura3"
            | "mURA3LO" -> "ura3lo"
            | x -> x // TODO: more generalized support not hard coded

        let l2ElementFromRoughagePair (ptw: Node<RoughagePTPair>) =
            let pt = ptw.Value
            let promoter = AstNode.L2Id(pt.Promoter)
            let target = AstNode.L2Id(pt.Target)
            L2.createL2Element promoter target

        // For roughage, if no marker is specified, it defaults to ura3
        let marker =
            match roughage.HasMarker with
            | None -> "ura3"
            | Some (x) -> markerMapping x

        let markerPragma =
            AstNode.Pragma
                ({ Value =
                       { Pragma.Definition = BuiltIn.markersetPragmaDef
                         Arguments = [ marker ] }
                   Positions = roughageWrapper.Positions })

        let l2Elements =
            [ for p in roughage.Parts do
                yield l2ElementFromRoughagePair p.Value.PromoterAndTarget1

                match p.Value.PromoterAndTarget2 with
                | Some (pt) -> yield l2ElementFromRoughagePair pt
                | None -> () ]

        let l2Locus =
            match roughage.Locus with
            | Some (l) -> Some(AstNode.L2Id(l))
            | None -> None

        let l2Expression = L2.createL2Expression l2Locus l2Elements

        // wrap the marker pragma and the L2 line up in a block
        AstNode.Block
            ({ Value = [ markerPragma; l2Expression ]
               Positions = [] })

    let private expandRoughageLine (node: AstNode): GslResult<AstNode, RoughageExpansionError> =
        match node with
        | AstNode.Roughage (rw) ->
            validateRoughageLine rw
            |> GslResult.map expandRoughage

        | _ -> GslResult.ok node

    /// Expand all inline roughage definitions into subblocks.
    let expandRoughageLines =
        FoldMap.map Serial TopDown expandRoughageLine
