module GslCore.Core.Expansion.Bootstrapping

open GslCore.Ast.Phase1Message
open GslCore.Constants
open GslCore.Ast.Types
open GslCore.Ast
open GslCore.Ast.ErrorHandling
open GslCore.Ast.Algorithms
open GslCore.Ast.Process
open GslCore.Ast.Legacy.Types
open GslCore.Ast.Legacy
open GslCore.GslResult
open GslCore.Ast.Process.AssemblyStuffing

// ==================
// bootstrapping literal source into an AST node
// ==================

let bootstrapError expectedType note tree =
    let extraText =
        match note with
        | Some (n) -> sprintf " %s" n
        | None -> ""

    let msg =
        sprintf "Unable to unpack as a '%s'.%s" expectedType extraText

    AstResult.errString (BootstrapError(Some(tree))) msg tree

/// Bootstrapped expansion phases don't have meaningful source positions as a result of expansion.
/// Instead, replace all of the positions with one provided from the external context to at least
/// locate the source of the error to the line in the original input source.
let private replaceSourcePosition pos node =
    match node with
    | AstNode.Int (nw) -> AstNode.Int({ nw with Positions = pos })
    | AstNode.Float (nw) -> AstNode.Float({ nw with Positions = pos })
    | AstNode.String (nw) -> AstNode.String({ nw with Positions = pos })
    | AstNode.Docstring (nw) -> AstNode.Docstring({ nw with Positions = pos })
    | AstNode.TypedVariable (nw) -> AstNode.TypedVariable({ nw with Positions = pos })
    | AstNode.TypedValue (nw) -> AstNode.TypedValue({ nw with Positions = pos })
    | AstNode.VariableBinding (nw) -> AstNode.VariableBinding({ nw with Positions = pos })
    | AstNode.BinaryOperation (nw) -> AstNode.BinaryOperation({ nw with Positions = pos })
    | AstNode.Negation (nw) -> AstNode.Negation({ nw with Positions = pos })
    | AstNode.ParseRelPos (nw) -> AstNode.ParseRelPos({ nw with Positions = pos })
    | AstNode.RelPos (nw) -> AstNode.RelPos({ nw with Positions = pos })
    | AstNode.Slice (nw) -> AstNode.Slice({ nw with Positions = pos })
    | AstNode.Mutation (nw) -> AstNode.Mutation({ nw with Positions = pos })
    | AstNode.DotMod (nw) -> AstNode.DotMod({ nw with Positions = pos })
    | AstNode.Part (nw) -> AstNode.Part({ nw with Positions = pos })
    | AstNode.Marker (nw) -> AstNode.Marker({ nw with Positions = pos })
    | AstNode.PartId (nw) -> AstNode.PartId({ nw with Positions = pos })
    | AstNode.InlineDna (nw) -> AstNode.InlineDna({ nw with Positions = pos })
    | AstNode.InlineProtein (nw) -> AstNode.InlineProtein({ nw with Positions = pos })
    | AstNode.HetBlock (nw) -> AstNode.HetBlock({ nw with Positions = pos })
    | AstNode.Gene (nw) -> AstNode.Gene({ nw with Positions = pos })
    | AstNode.L2Id (nw) -> AstNode.L2Id({ nw with Positions = pos })
    | AstNode.L2Element (nw) -> AstNode.L2Element({ nw with Positions = pos })
    | AstNode.L2Expression (nw) -> AstNode.L2Expression({ nw with Positions = pos })
    | AstNode.Roughage (nw) -> AstNode.Roughage({ nw with Positions = pos })
    | AstNode.ParsePragma (nw) -> AstNode.ParsePragma({ nw with Positions = pos })
    | AstNode.Pragma (nw) -> AstNode.Pragma({ nw with Positions = pos })
    | AstNode.Block (nw) -> AstNode.Block({ nw with Positions = pos })
    | AstNode.FunctionDef (nw) -> AstNode.FunctionDef({ nw with Positions = pos })
    | AstNode.FunctionLocals (nw) -> AstNode.FunctionLocals({ nw with Positions = pos })
    | AstNode.FunctionCall (nw) -> AstNode.FunctionCall({ nw with Positions = pos })
    | AstNode.Assembly (nw) -> AstNode.Assembly({ nw with Positions = pos })
    | AstNode.ParseError (nw) -> AstNode.ParseError({ nw with Positions = pos })
    | AstNode.Splice (x) -> AstNode.Splice(x)

/// Replace all source positions in a bootstrapped expanded tree with the position of the node
/// that was expanded into source.
let private replaceSourcePositions pos =
    FoldMap.map Serial TopDown (GslResult.promote (replaceSourcePosition pos))

/// If any messages emanated from a bootstrapped parsing, replace their positions with the input position.
let private replaceMessagePositions pos =
    GslResult.mapMessages (fun (msg: AstMessage) -> { msg with SourcePosition = pos })

///<summary>
/// Later phases of the compiler currently output literal source code which is parsed again.
/// This function accepts literal source code which is parsed and the resuling AST is run
/// through the provided operation.  This function unpacks the contents of the top-level
/// block that results from compilation and re-packs it as a Splice, to indicate
/// to a subsequent expansion pass that this node needs to be unpacked into its outer context.
///</summary>
let bootstrap originalPosition (op: AstTreeHead -> TreeTransformResult<AstMessage>) (source: GslSourceCode) =
    /// Unpack a bootstrapped AST to a block or fail.
    let asBlock tree =
        match tree with
        | AstTreeHead (AstNode.Block (nw)) -> GslResult.ok (AstNode.Splice(Array.ofList nw.Value))
        | AstTreeHead (node) -> bootstrapError "Block" None node

    let contextMsg =
        sprintf "An error occurred while parsing this internally-generated GSL source code:\n%s" source.String

    LexAndParse.lexAndParse false source
    |> GslResult.mapError (LexParseError.toAstMessage)
    |> GslResult.addMessageToError
        (AstMessage.createErrorWithStackTrace
            (InternalError(ParserError))
             contextMsg
             (AstNode.String
                 ({ Value = source.String
                    Positions = originalPosition })))
    >>= (replaceSourcePositions originalPosition)
    >>= op
    >>= asBlock

/// Parse string source code, run compiler phase 1, and return the resulting contents of the
/// top-level block.
let bootstrapPhase1 (parameters: Phase1Parameters) originalPosition =
    bootstrap
        originalPosition
        (Phase1.phase1 parameters
         >> GslResult.mapError Phase1Message.toAstMessage)


// =================
// splicing bootstraps back into the tree
// =================

/// Determine if a list of nodes contains any splices.
let private containsSplice nodes =
    nodes
    |> List.tryPick (fun node ->
        match node with
        | AstNode.Splice _ -> Some(node)
        | _ -> None)
    |> Option.isSome

/// Explode Splice nodes into their enclosing context.
/// They can appear in Blocks or Assemblies.
let private healSplice node =
    match node with
    | AstNode.Block (bw) ->
        let nodeList = bw.Value
        // if no splices, do nothing
        if not (containsSplice nodeList) then
            AstNode.Block(bw)
        else
            let newNodeList =
                nodeList // make an array out of each node
                |> List.map (fun node ->
                    match node with
                    | AstNode.Splice (newNodes) -> newNodes
                    | x -> [| x |])
                |> Array.concat // concat the arrays
                |> List.ofArray

            AstNode.Block({ bw with Value = newNodeList })
    | _ -> node

/// Explode all Splices into their enclosing context.
let healSplices =
    FoldMap.map Serial TopDown (GslResult.promote healSplice)


// ==================================
// structure of a bootstrapped expansion step
// ==================================

/// Convert an assembly into a Splice using an expansion function and a bootstrap operation.
/// Since the expansion function may raise an exception, we capture that exception
/// and inject it into the result stream.
let bootstrapExpandLegacyAssembly errorMsgType
                                  (expansionFunction: Assembly -> GslSourceCode)
                                  bootstrapOperation
                                  assemblyConversionContext
                                  node
                                  : NodeTransformResult<AstMessage> =
    /// Perform the expansion operation, capturing any exception as an error.
    let expandCaptureException assembly =
        try
            expansionFunction assembly |> GslResult.ok
        with e ->
            AstResult.exceptionToError errorMsgType node e
            |> GslResult.err

    match node with
    | AssemblyPart (apUnpack) ->
        LegacyConversion.convertAssembly assemblyConversionContext apUnpack
        >>= expandCaptureException
        >>= (bootstrapOperation ((fst apUnpack).Positions))
    | _ -> GslResult.ok node

/// Execute a complete bootstrapped expansion on an AST.
/// Runs foldmap on the provided expansion function, followed by
/// an operation that heals all of the scars in the AST left by the expansion.
/// This is necessary because some bootstrapped expansion phases convert a single
/// node into a miniature block, which we want to expand into the outer context.
let executeBootstrap bootstrappedExpansionFunction mode (tree: AstTreeHead) =
    let foldmapParameters =
        { FoldMapParameters.Direction = TopDown
          Mode = mode
          StateUpdate = LegacyConversion.updateConversionContext
          Map = bootstrappedExpansionFunction }

    FoldMap.foldMap  // run the bootstrapped expand operation
        AssemblyConversionContext.empty
        foldmapParameters
        tree
    >>= healSplices // heal the splices
    >>= (AssemblyStuffing.stuffPragmasIntoAssemblies
         >> GslResult.mapError AssemblyStuffingMessage.toAstMessage) // Bootstrapped assemblies need their pragma environment reinjected
