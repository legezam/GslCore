module GslCore.Core.Expansion.Bootstrapping

open GslCore.Ast.MessageTranslation
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
    | Int (nw) -> Int({ nw with Positions = pos })
    | Float (nw) -> Float({ nw with Positions = pos })
    | String (nw) -> String({ nw with Positions = pos })
    | Docstring (nw) -> Docstring({ nw with Positions = pos })
    | TypedVariable (nw) -> TypedVariable({ nw with Positions = pos })
    | TypedValue (nw) -> TypedValue({ nw with Positions = pos })
    | VariableBinding (nw) -> VariableBinding({ nw with Positions = pos })
    | BinaryOperation (nw) -> BinaryOperation({ nw with Positions = pos })
    | Negation (nw) -> Negation({ nw with Positions = pos })
    | ParseRelPos (nw) -> ParseRelPos({ nw with Positions = pos })
    | RelPos (nw) -> RelPos({ nw with Positions = pos })
    | Slice (nw) -> Slice({ nw with Positions = pos })
    | Mutation (nw) -> Mutation({ nw with Positions = pos })
    | DotMod (nw) -> DotMod({ nw with Positions = pos })
    | Part (nw) -> Part({ nw with Positions = pos })
    | Marker (nw) -> Marker({ nw with Positions = pos })
    | PartId (nw) -> PartId({ nw with Positions = pos })
    | InlineDna (nw) -> InlineDna({ nw with Positions = pos })
    | InlineProtein (nw) -> InlineProtein({ nw with Positions = pos })
    | HetBlock (nw) -> HetBlock({ nw with Positions = pos })
    | Gene (nw) -> Gene({ nw with Positions = pos })
    | L2Id (nw) -> L2Id({ nw with Positions = pos })
    | L2Element (nw) -> L2Element({ nw with Positions = pos })
    | L2Expression (nw) -> L2Expression({ nw with Positions = pos })
    | Roughage (nw) -> Roughage({ nw with Positions = pos })
    | ParsePragma (nw) -> ParsePragma({ nw with Positions = pos })
    | Pragma (nw) -> Pragma({ nw with Positions = pos })
    | Block (nw) -> Block({ nw with Positions = pos })
    | FunctionDef (nw) -> FunctionDef({ nw with Positions = pos })
    | FunctionLocals (nw) -> FunctionLocals({ nw with Positions = pos })
    | FunctionCall (nw) -> FunctionCall({ nw with Positions = pos })
    | Assembly (nw) -> Assembly({ nw with Positions = pos })
    | ParseError (nw) -> ParseError({ nw with Positions = pos })
    | Splice (x) -> Splice(x)

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
        | AstTreeHead (Block (nw)) -> GslResult.ok (Splice(Array.ofList nw.Value))
        | AstTreeHead (node) -> bootstrapError "Block" None node

    let contextMsg =
        sprintf "An error occurred while parsing this internally-generated GSL source code:\n%s" source.String

    LexAndParse.lexAndParse false source
    |> GslResult.addMessageToError
        (AstMessage.createErrorWithStackTrace
            (InternalError(ParserError))
             contextMsg
             (String
                 ({ Value = source.String
                    Positions = originalPosition })))
    >>= (replaceSourcePositions originalPosition)
    >>= op
    >>= asBlock

/// Parse string source code, run compiler phase 1, and return the resulting contents of the
/// top-level block.
let bootstrapPhase1 (parameters: Phase1Parameters) originalPosition =
    bootstrap originalPosition (Phase1.phase1 parameters)


// =================
// splicing bootstraps back into the tree
// =================

/// Determine if a list of nodes contains any splices.
let private containsSplice nodes =
    nodes
    |> List.tryPick (fun node ->
        match node with
        | Splice _ -> Some(node)
        | _ -> None)
    |> Option.isSome

/// Explode Splice nodes into their enclosing context.
/// They can appear in Blocks or Assemblies.
let private healSplice node =
    match node with
    | Block (bw) ->
        let nodeList = bw.Value
        // if no splices, do nothing
        if not (containsSplice nodeList) then
            Block(bw)
        else
            let newNodeList =
                nodeList // make an array out of each node
                |> List.map (fun node ->
                    match node with
                    | Splice (newNodes) -> newNodes
                    | x -> [| x |])
                |> Array.concat // concat the arrays
                |> List.ofArray

            Block({ bw with Value = newNodeList })
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
