namespace GslCore.Core.Expansion.Bootstrapping

open GslCore.Ast.Phase1Message
open GslCore.Constants
open GslCore.Ast.Types
open GslCore.Ast
open GslCore.Ast.ErrorHandling
open GslCore.Ast.Algorithms
open GslCore.Ast.Process
open GslCore.Legacy.Types
open GslCore.Legacy
open GslCore.GslResult
open GslCore.Ast.Process.AssemblyStuffing

type BootstrapError =
    // ==================
    // bootstrapping literal source into an AST node
    // ==================
//    let bootstrapError expectedType note tree =
//        let extraText =
//            match note with
//            | Some n -> sprintf " %s" n
//            | None -> ""
//
//        let msg =
//            sprintf "Unable to unpack as a '%s'.%s" expectedType extraText
//
//        AstResult.errString (BootstrapError(Some(tree))) msg tree    
    | FailedToUnpack of expectedType: string * maybeNote: string option * node: AstNode

module Bootstrapping =

    /// Bootstrapped expansion phases don't have meaningful source positions as a result of expansion.
    /// Instead, replace all of the positions with one provided from the external context to at least
    /// locate the source of the error to the line in the original input source.
    let private replaceSourcePosition (pos: SourcePosition list): AstNode -> AstNode =
        function
        | AstNode.Int wrapper -> AstNode.Int { wrapper with Positions = pos }
        | AstNode.Float wrapper -> AstNode.Float { wrapper with Positions = pos }
        | AstNode.String wrapper -> AstNode.String { wrapper with Positions = pos }
        | AstNode.Docstring wrapper -> AstNode.Docstring { wrapper with Positions = pos }
        | AstNode.TypedVariable wrapper -> AstNode.TypedVariable { wrapper with Positions = pos }
        | AstNode.TypedValue wrapper -> AstNode.TypedValue { wrapper with Positions = pos }
        | AstNode.VariableBinding wrapper -> AstNode.VariableBinding { wrapper with Positions = pos }
        | AstNode.BinaryOperation wrapper -> AstNode.BinaryOperation { wrapper with Positions = pos }
        | AstNode.Negation wrapper -> AstNode.Negation { wrapper with Positions = pos }
        | AstNode.ParseRelPos wrapper -> AstNode.ParseRelPos { wrapper with Positions = pos }
        | AstNode.RelPos wrapper -> AstNode.RelPos { wrapper with Positions = pos }
        | AstNode.Slice wrapper -> AstNode.Slice { wrapper with Positions = pos }
        | AstNode.Mutation wrapper -> AstNode.Mutation { wrapper with Positions = pos }
        | AstNode.DotMod wrapper -> AstNode.DotMod { wrapper with Positions = pos }
        | AstNode.Part wrapper -> AstNode.Part { wrapper with Positions = pos }
        | AstNode.Marker wrapper -> AstNode.Marker { wrapper with Positions = pos }
        | AstNode.PartId wrapper -> AstNode.PartId { wrapper with Positions = pos }
        | AstNode.InlineDna wrapper -> AstNode.InlineDna { wrapper with Positions = pos }
        | AstNode.InlineProtein wrapper -> AstNode.InlineProtein { wrapper with Positions = pos }
        | AstNode.HetBlock wrapper -> AstNode.HetBlock { wrapper with Positions = pos }
        | AstNode.Gene wrapper -> AstNode.Gene { wrapper with Positions = pos }
        | AstNode.L2Id wrapper -> AstNode.L2Id { wrapper with Positions = pos }
        | AstNode.L2Element wrapper -> AstNode.L2Element { wrapper with Positions = pos }
        | AstNode.L2Expression wrapper -> AstNode.L2Expression { wrapper with Positions = pos }
        | AstNode.Roughage wrapper -> AstNode.Roughage { wrapper with Positions = pos }
        | AstNode.ParsePragma wrapper -> AstNode.ParsePragma { wrapper with Positions = pos }
        | AstNode.Pragma wrapper -> AstNode.Pragma { wrapper with Positions = pos }
        | AstNode.Block wrapper -> AstNode.Block { wrapper with Positions = pos }
        | AstNode.FunctionDef wrapper -> AstNode.FunctionDef { wrapper with Positions = pos }
        | AstNode.FunctionLocals wrapper -> AstNode.FunctionLocals { wrapper with Positions = pos }
        | AstNode.FunctionCall wrapper -> AstNode.FunctionCall { wrapper with Positions = pos }
        | AstNode.Assembly wrapper -> AstNode.Assembly { wrapper with Positions = pos }
        | AstNode.ParseError wrapper -> AstNode.ParseError { wrapper with Positions = pos }
        | AstNode.Splice x -> AstNode.Splice x

    /// Replace all source positions in a bootstrapped expanded tree with the position of the node
    /// that was expanded into source.
    let private replaceSourcePositions (position: SourcePosition list): AstTreeHead -> TreeTransformResult<'a> =
        FoldMap.map Serial TopDown (GslResult.promote (replaceSourcePosition position))


    ///<summary>
    /// Later phases of the compiler currently output literal source code which is parsed again.
    /// This function accepts literal source code which is parsed and the resuling AST is run
    /// through the provided operation.  This function unpacks the contents of the top-level
    /// block that results from compilation and re-packs it as a Splice, to indicate
    /// to a subsequent expansion pass that this node needs to be unpacked into its outer context.
    ///</summary>
    let bootstrap originalPosition
                  (op: AstTreeHead -> TreeTransformResult<AstMessage>)
                  (source: GslSourceCode)
                  : GslResult<AstNode, AstMessage> =
        /// Unpack a bootstrapped AST to a block or fail.
        let asBlock tree =
            match tree with
            | AstTreeHead (AstNode.Block blockWrapper) -> GslResult.ok (AstNode.Splice(Array.ofList blockWrapper.Value))
            | AstTreeHead node -> bootstrapError "Block" None node

        let contextMsg =
            sprintf "An error occurred while parsing this internally-generated GSL source code:\n%s" source.String

        LexAndParse.lexAndParse false source
        |> GslResult.mapError (LexParseError.toAstMessage)
        |> GslResult.addMessageToError
            (AstMessage.createErrorWithStackTrace
                (InternalError(ParserError))
                 contextMsg
                 (AstNode.String
                     ({ Node.Value = source.String
                        Positions = originalPosition })))
        >>= (replaceSourcePositions originalPosition)
        >>= op
        >>= asBlock

    /// Parse string source code, run compiler phase 1, and return the resulting contents of the
    /// top-level block.
    let bootstrapPhase1 (parameters: Phase1Parameters)
                        (originalPosition: SourcePosition list)
                        : GslSourceCode -> GslResult<AstNode, AstMessage> =
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
        | AssemblyPart apUnpack ->
            LegacyConversion.convertAssembly assemblyConversionContext apUnpack
            |> GslResult.mapError LegacyAssemblyCreationError.toAstMessage
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
