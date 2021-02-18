/// Processing and validation of AST nodes and trees.
/// Non-bioinformatic tree transformation algorithms also live here.
module GslCore.AstProcess

open Amyris.ErrorHandling
open Amyris.Dna
open GslCore.Ast.Process
open GslCore.AstTypes
open GslCore.AstErrorHandling
open GslCore.AstAlgorithms
open GslCore.Constants
open GslCore.Pragma
open GslCore.Reference


// ==================
// checking gene naming
// ==================

/// If a node is a part with a gene, validate the name of that gene.
/// Uses the pragmas of the enclosing part and the outer assembly context.
let private checkGeneName (rgs: GenomeDefinitions) (library: Map<string, Dna>) assemblyPragmas node =
    match node with
    | GenePart (pp, gp) ->
        let geneName = gp.Value.Gene.[1..].ToUpper()
        let partPragmas = ParsePart.getPragmas pp

        GenomeDefinitions.getReferenceGenome rgs [ partPragmas; assemblyPragmas ]
        |> mapMessages (fun s -> AstMessage.createErrorWithStackTrace RefGenomeError s node)
        >>= (fun reference ->
            if reference |> GenomeDefinition.isValidFeature geneName
               || library.ContainsKey(geneName) then
                Validation.good
            else
                AstMessage.createErrorf PartError "Unknown gene: '%s'." geneName (pp.Value.BasePart))
    | _ -> Validation.good

/// Check all the gene names in the context of a single assembly.
let private checkGeneNamesInAssembly (rgs: GenomeDefinitions) library node =
    match node with
    | AssemblyPart (pw, aw) ->
        let assemblyPrags = ParsePart.getPragmas pw

        aw.Value
        |> List.map (checkGeneName rgs library assemblyPrags)
        |> collectValidations
    | _ -> Validation.good

/// Validate all gene names.
let checkGeneNames rgs library =
    Validation.validate (checkGeneNamesInAssembly rgs library)

// =========================
// stripping all non-literals from a tree
// =========================

// there are some phases where we want to clean a tree by removing certain kinds of nodes
// these functions are defined here

/// Match only function declarations.
let cleanFunction node =
    match node with
    | FunctionDef _ -> None
    | _ -> Some node

/// Match only variable declarations
let cleanVariable node =
    match node with
    | VariableBinding _ -> None
    | _ -> Some node

/// Clean function defintions and variable bindings from blocks.
let private cleanBlock cleaner node =
    match node with
    | Block (bw) ->
        let newBlockContents = bw.Value |> List.choose cleaner
        Block({ bw with Value = newBlockContents })
    | _ -> node

/// Strip function defintions from tree.
let stripFunctions =
    FoldMap.map Serial TopDown (promote (cleanBlock cleanFunction))

/// Strip variable bindings from tree.
let stripVariables =
    FoldMap.map Serial TopDown (promote (cleanBlock cleanVariable))

// =======================
// collecting warning messages from pragmas
// =======================

let private collectWarning (node: AstNode): Result<AstNode, AstMessage> =
    match node with
    | Pragma pragma when pragma.Value |> Pragma.isWarning ->
        let msg = pragma.Value.Arguments |> String.concat " "
        let warnMsg = AstMessage.createWarning msg node
        warn warnMsg node // add a warning into the message stream
    | _ -> ok node

/// Add warnings into the message stream for every #warn pragma in the tree.
let collectWarnings = FoldMap.map Serial TopDown collectWarning

// =====================
// naming every assembly if it isn't named
// =====================

let private nameLegal =
    "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789![]@$%^&*()'\":_-=+,.?/`~"
    |> Set.ofSeq

let private whitespace c =
    match c with
    | ' ' -> true
    | '\t' -> true
    | _ -> false

let cleanHashName (s: string) =
    s
    |> Seq.choose (fun c ->
        if nameLegal.Contains(c) then Some(c)
        else if whitespace c then None
        else Some('_'))
    |> Array.ofSeq
    |> Amyris.Bio.utils.arr2seq


/// Name an assembly if it is not already named.
/// We accomplish this by replacing the assembly with a subblock, into which we're placed a name pragma.
/// Since naming happens after pragma stuffing, we also put the name pragma into the assebly itself.
let private nameAssembly (node: AstNode): AstNode =
    match node with
    | AssemblyPart (assemblyWrapper, _) ->
        let pragmas = ParsePart.getPragmas assemblyWrapper

        if pragmas
           |> PragmaCollection.contains BuiltIn.namePragmaDef then node // already named
        else let literal = AstNode.decompile node |> cleanHashName

             let name =
                 literal
                     .Substring(0, min literal.Length Default.NameMaxLength)
                     .Replace("@", "(@)")

             let namePragma =
                 { Pragma.Definition = BuiltIn.namePragmaDef
                   Arguments = [ name ] }

             let mergedPragmas =
                 pragmas |> PragmaCollection.add namePragma

             let namedAssembly =
                 Part(ParsePart.replacePragmas assemblyWrapper mergedPragmas)

             let pragmaNode = Pragma(Node.wrapNode namePragma)
             Block(Utils.nodeWrapWithNodePosition node [ pragmaNode; namedAssembly ])
    | _ -> node


///<summary>
/// If an assembly does not have a name, generate one and stuff it in.
/// Also prepend a name pragma, accomplished by replacing the assembly with a subblock that includes
/// the new name pragma.
///</summary>
let nameAssemblies =
    FoldMap.map Serial TopDown (promote nameAssembly)


// ====================
// expanding inline roughage
// ====================

// the parser outputs inline roughage sections as blocks for convenience.
// we expand each individual line into block, possibly containing pragmas, and one L2 line.
// we need the pragma context to do this

let private validateRoughageLine (rw: Node<Roughage>) =
    let r = rw.Value
    // Rule 1:  must be able to work out the locus.  Locus can be either explicit (ho^) or
    //          implicit pSLN1>YNG1  but can't have just bidirectional promoters with no explicit locus  e.g.   ADH1<pGAL1-pGAL10>ADH2
    let hasLocus =
        r.Locus.IsSome
        || (r.Parts.Length > 0
            && not r.Parts.Head.Value.PromoterAndTarget2.IsSome)

    let node = Roughage(rw)

    if not hasLocus
    then AstMessage.createErrorf ValueError "Roughage construct has indeterminate locus: %s" (AstNode.decompile node) node
    else ok rw

/// Roughage expands to Level 2 GSL.  We actually do this using the AST rather than bootstrapping.
let private expandRoughage (roughageWrapper: Node<Roughage>): AstNode =
    let roughage = roughageWrapper.Value
    // FIXME Hard coded mapping of markers for now
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
        let promoter = L2Id(pt.Promoter)
        let target = L2Id(pt.Target)
        L2.createL2Element promoter target

    // For roughage, if no marker is specified, it defaults to ura3
    let marker =
        match roughage.HasMarker with
        | None -> "ura3"
        | Some (x) -> markerMapping x

    let markerPragma =
        Pragma
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
        | Some (l) -> Some(L2Id(l))
        | None -> None

    let l2Expression = L2.createL2Expression l2Locus l2Elements

    // wrap the marker pragma and the L2 line up in a block
    Block
        ({ Value = [ markerPragma; l2Expression ]
           Positions = [] })

let private expandRoughageLine node =
    match node with
    | Roughage (rw) ->
        validateRoughageLine rw
        >>= (promote expandRoughage)

    | _ -> ok node

/// Expand all inline roughage definitions into subblocks.
let expandRoughageLines = FoldMap.map Serial TopDown expandRoughageLine
