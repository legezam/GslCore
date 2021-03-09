﻿/// Generic algorithms for operating on ASTs.
namespace GslCore.Ast.Algorithms

open System.Text
open GslCore
open GslCore.GslResult
open GslCore.Constants
open GslCore.Pragma
open GslCore.Ast.Types
open GslCore.Ast.ErrorHandling
open FSharp.Collections.ParallelSeq


// ===================================
// Decompiling AST into literal source code
// ===================================

/// Keep track of state while printing.
type PrintState =
    { IndentLevel: int
      IsInsideAssembly: bool
      QuoteStrings: bool }

module AstNode =
    /// Decompile an AST back into GSL source code.
    let decompile (tree: AstNode): string =
        let indentSize = 4
        let INDENT = String.replicate indentSize " "

        // Since we are really printing an entire tree, we will gather pieces and string them together.
        let sb = StringBuilder()

        // helper functions
        let append (s: string) = sb.Append(s) |> ignore
        let appendf fmt s = append (sprintf fmt s)
        let appendff fmt s0 s1 = append (sprintf fmt s0 s1)
        let newline () = append "\n"

        // default state for decompiling a top-level block
        let initialPrintState =
            { IndentLevel = 0
              IsInsideAssembly = false
              QuoteStrings = true }

        /// Recursively print an entire AST.
        /// TargetIndent is the level of block indentation we are currently sitting at.
        let rec _print node state =
            /// Indent to one level above the current indentation level.
            /// Useful for rewriting docstrings as they live inside parts rather than blocks.
            let minorIndent () =
                append (String.replicate (max 0 (state.IndentLevel - 1)) INDENT)
            /// Indent to the full current level.
            let fullIndent () =
                append (String.replicate state.IndentLevel INDENT)

            let printSemicolonSeparatedList (elements: AstNode list) =
                let nElem = elements.Length

                elements
                |> List.iteri (fun i p ->
                    _print p state
                    if i < nElem - 1 then append " ; ")

            match node with
            // print nothing for internal-only nodes
            | BookkeepingNode _ -> ()
            | AstNode.Splice _ ->
                failwithf "Cannot decompile a bootstrap splice marker.  The tree must be spliced first."
            // leaf nodes are easy
            | AstNode.Int ({ Value = i; Positions = _ }) -> appendf "%d" i
            | AstNode.Float ({ Value = i; Positions = _ }) -> appendf "%f" i
            | AstNode.String ({ Value = i; Positions = _ }) ->
                if state.QuoteStrings then appendf "\"%s\"" i else append i
            | AstNode.Docstring (dw) -> appendf "///%s" dw.Value
            | AstNode.TypedVariable ({ Value = (name, _); Positions = _ }) -> appendf "&%s" name
            | AstNode.TypedValue ({ Value = (_, inner); Positions = _ }) -> _print inner state
            | AstNode.VariableBinding ({ Value = vb; Positions = _ }) ->
                appendf "let %s = " vb.Name
                _print vb.Value state
            | AstNode.BinaryOperation ({ Value = binop; Positions = _ }) ->
                // Explicitly group every binary in parens to keep things unambiguous.
                append "("
                _print binop.Left state

                append
                    (match binop.Operator with
                     | Add -> " + "
                     | Subtract -> " - "
                     | Multiply -> " * "
                     | Divide -> " / ")

                _print binop.Right state
                append ")"
            | AstNode.Negation ({ Value = inner; Positions = _ }) ->
                append "-"
                _print inner state
            // basic parts
            | AstNode.Marker _ -> append "###"
            | AstNode.PartId ({ Value = name; Positions = _ }) -> appendf "@%s" name
            | AstNode.InlineDna ({ Value = dna; Positions = _ }) -> appendf "/%s/" dna
            | AstNode.InlineProtein ({ Value = pseq; Positions = _ }) -> appendf "/$%s/" pseq
            | AstNode.HetBlock _ -> append "~"
            | AstNode.Gene ({ Value = pg; Positions = _ }) ->
                match pg.Linker with
                | Some ({ Linker1 = l1
                          Linker2 = l2
                          Orient = o }) -> append (sprintf "%s-%s-%s-%s" l1 l2 o pg.Gene)
                | None -> append pg.Gene
            // part mods
            | AstNode.ParseRelPos ({ Value = rp; Positions = _ }) ->
                _print rp.Item state

                match rp.Qualifier with
                | Some (rpq) -> append (Parts.relPosQualifierToString rpq)
                | None -> ()
            | AstNode.RelPos ({ Value = rp; Positions = _ }) ->
                appendff
                    "%d%s"
                    rp.Position
                    (match rp.RelativeTo with
                     | FivePrime -> "S"
                     | ThreePrime -> "E")
            | AstNode.Slice ({ Value = s; Positions = _ }) ->
                append "["
                if s.LeftApprox then append "~"
                _print s.Left state
                append ":"
                if s.RightApprox then append "~"
                _print s.Right state
                append "]"
            | AstNode.Mutation ({ Value = mut; Positions = _ }) ->
                match mut.Type with
                | AA -> append "$"
                | NT -> append "*"

                append (sprintf "%c%d%c" mut.From mut.Location mut.To)
            | AstNode.DotMod ({ Value = s; Positions = _ }) -> appendf ".%s" s
            | AstNode.ParsePragma ({ Value = pp; Positions = _ }) ->
                appendf "#%s" pp.Name

                for v in pp.Values do
                    append " "
                    _print v { state with QuoteStrings = false } // don't quote string pragma values
            | AstNode.Pragma ({ Value = p; Positions = _ }) ->
                appendf "#%s" p.Definition.Name

                for arg in p.Arguments do
                    appendf " %s" arg
            | AstNode.FunctionDef ({ Value = fd; Positions = _ }) ->
                // declaration line
                appendff "let %s(%s) =" fd.Name (String.concat ", " fd.ArgumentNames)
                newline ()
                // print the enclosed block
                _print fd.Body state
                minorIndent ()
                append "end"
            | AstNode.FunctionCall ({ Value = fc; Positions = _ }) ->
                appendf "%s(" fc.Name
                // the args are nodes, so recurse and add separators
                let rec printArgs args =
                    let stateInner = { state with IsInsideAssembly = true }

                    match args with
                    | [] -> ()
                    | [ a ] -> _print a stateInner
                    | hd :: tl ->
                        _print hd stateInner
                        append ", "
                        printArgs tl

                printArgs fc.Arguments
                append ")"
            | AstNode.Part ({ Value = p; Positions = _ }) ->
                // Print the base part.
                // Need to make sure subassemblies are grouped in parens.
                // TODO: decide what we want to do about assemblies with only one part.
                let enclose =
                    state.IsInsideAssembly
                    && match p.BasePart with
                       | AstNode.Assembly _ -> true
                       | _ -> false

                // Any deeper recursion is inside a part and ought to enclose its assemblies in parens.
                // Inner parts should also emit pragmas.
                let revisedState = { state with IsInsideAssembly = true }

                if not p.IsForward then append "!"

                if enclose then append "("
                _print p.BasePart revisedState
                if enclose then append ")"

                // print the mods
                for m in p.Modifiers do
                    _print m revisedState
                // print the pragmas
                if state.IsInsideAssembly && not (p.Pragmas.IsEmpty) then
                    append " {"
                    let nPragmas = p.Pragmas.Length

                    p.Pragmas
                    |> List.iteri (fun i prag ->
                        _print prag revisedState
                        if i < nPragmas - 1 then append " ")

                    append "}"
            | AstNode.Assembly ({ Value = parts; Positions = _ }) ->
                // print all the parts in the assembly separated by semicolons
                printSemicolonSeparatedList parts
            | AstNode.L2Id (lw) -> append lw.Value.String
            | AstNode.L2Element (lw) ->
                _print lw.Value.Promoter state
                append ">"
                _print lw.Value.Target state
            | AstNode.L2Expression (lw) ->
                let nParts = lw.Value.Parts.Length

                match lw.Value.Locus with
                | Some (l) ->
                    _print l state
                    append "^"
                    if nParts > 0 then append " ; "
                | None -> ()

                printSemicolonSeparatedList lw.Value.Parts
            | AstNode.Roughage ({ Value = rLine; Positions = _ }) ->
                // decompile lines of rougage as individual blocks
                append "<@ "

                let header =
                    match rLine.Locus, rLine.Marker with
                    | Some (lw), Some (mw) -> [ sprintf "%s^[%s]" lw.Value.String mw.Value ]
                    | Some (lw), None -> [ sprintf "%s^" lw.Value.String ]
                    | None, _ -> []

                // Now individual elements
                let tail =
                    rLine.Parts
                    |> List.map (fun rew ->
                        let re = rew.Value

                        let elementHead =
                            let pt1 = re.PromoterAndTarget1

                            match re.PromoterAndTarget2 with
                            | Some (pt2) ->
                                sprintf "%s-%s" (pt1.Value.ToString(RoughageRev)) (pt2.Value.ToString(RoughageFwd))
                            | None -> pt1.Value.ToString(RoughageFwd)

                        let elementTail =
                            match re.Marker with
                            | Some (mw) -> sprintf "[%s]" mw.Value
                            | None -> ""

                        elementHead + elementTail)

                append (String.concat "::" (header @ tail))
                append " @>"
            | AstNode.Block ({ Value = lines; Positions = _ }) ->
                for l in lines do
                    let printInnerLine () =
                        _print
                            l
                            { state with
                                  IndentLevel = state.IndentLevel + 1 }

                    match l with
                    | BookkeepingNode _ -> () // ignore bookkeeping nodes in blocks
                    | AstNode.Block _ ->
                        // we need to wrap an inner block in do/end
                        // we also delegate newlines and indentation to the inner block
                        fullIndent ()
                        append "do"
                        newline ()

                        printInnerLine ()

                        fullIndent ()
                        append "end"
                        newline ()
                    | _ ->
                        // regular lines need to be indented and have newlines added after them
                        fullIndent ()
                        printInnerLine ()
                        newline ()

            | AstNode.ParseError (x) -> failwithf "Parse error found during AST code generation: %A" x
            | x -> Utils.nonExhaustiveError x

        _print tree initialPrintState

        sb.ToString()

    // =====================
    // Basic AST traversal
    // =====================

    /// Return a sequence of all of the immediate children of this node.
    let getChildren (node: AstNode): AstNode seq =
        match node with
        | Leaf _ -> Seq.empty
        // variable binding
        | AstNode.VariableBinding (vb) -> Seq.singleton vb.Value.Value
        // typed value
        | AstNode.TypedValue ({ Value = (_, n); Positions = _ }) -> Seq.singleton n
        // Simple operations on values
        | AstNode.BinaryOperation ({ Value = binOp; Positions = _ }) ->
            seq {
                yield binOp.Left
                yield binOp.Right
            }
        | AstNode.Negation ({ Value = n; Positions = _ }) -> Seq.singleton n
        // Slicing
        | AstNode.ParseRelPos ({ Value = { Item = n; Qualifier = _ }
                                 Positions = _ }) -> Seq.singleton n
        | AstNode.Slice ({ Value = slice; Positions = _ }) ->
            seq {
                yield slice.Left
                yield slice.Right
            }
        // generic part with mods, pragmas, direction
        | AstNode.Part ({ Value = part; Positions = _ }) ->
            seq {
                yield part.BasePart
                yield! Seq.ofList part.Modifiers
                yield! Seq.ofList part.Pragmas
            }
        // L2 elements
        | AstNode.L2Element (lw) ->
            seq {
                yield lw.Value.Promoter
                yield lw.Value.Target
            }
        | AstNode.L2Expression (lw) ->
            seq {
                match lw.Value.Locus with
                | Some (l) -> yield l
                | None -> ()

                yield! Seq.ofList lw.Value.Parts
            }
        // pragmas
        | AstNode.ParsePragma ({ Value = pp; Positions = _ }) -> Seq.ofList pp.Values
        // Block of code
        | AstNode.Block ({ Value = nodes; Positions = _ }) -> Seq.ofList nodes
        // Function definition and call
        | AstNode.FunctionDef ({ Value = f; Positions = _ }) -> Seq.singleton f.Body
        | AstNode.FunctionCall ({ Value = fc; Positions = _ }) -> Seq.ofList fc.Arguments
        | AstNode.Assembly ({ Value = parts; Positions = _ }) -> Seq.ofList parts
        | x -> Utils.nonExhaustiveError x

    ///Visit every AST node in the tree starting at the top and returning children in depth-first
    ///left-to-right order.
    let traverse (tree: AstTreeHead): AstNode seq =
        let rec loop node =
            seq {
                // yield this node
                yield node
                // recurse into its children
                for child in getChildren node do
                    yield! loop child
            }

        loop tree.wrappedNode

// =========================
// foldmap, the workhorse of AST manipulation
// =========================

type NodeTransformResult<'a> = GslResult<AstNode, 'a>

type TreeTransformResult<'a> = GslResult<AstTreeHead, 'a>

type FoldMapDirection =
    | TopDown
    | BottomUp

///<summary>
/// State update functions can update the state before the node transformation, using the original
/// node and the incoming state, and they can operate again on pre-transformation node and the
/// state resulting from the pre-transformation update.  Virtually all transformations should be
/// exclusively PreTransform with no PostTransform mode, as the behavior of foldmap in this mode
/// ensures that natual block scoping rules apply.  However, transient pragmas have challenging
/// semantics and require special treatment, so the pragma collection function requires both of
/// these modes to ensure that blocks capture the transient pragma state, leaving a clean slate
/// in the outer scope.  Note that state updates ALWAYS operate on the pre-transformation node
/// and NEVER the post-transformation node.
///</summary>
type StateUpdateMode =
    | PreTransform
    | PostTransform

module StateUpdateMode =
    /// Create a state update function that only ever operates in PreTransform mode.
    let pretransformOnly (operator: 'a -> 'b -> 'a) (mode: StateUpdateMode) (state: 'a) (node: 'b): 'a =
        match mode with
        | PreTransform -> operator state node
        | PostTransform -> state

type FoldmapMode =
    | Serial
    | Parallel

type FoldMapParameters<'State, 'Msg> =
    { Mode: FoldmapMode
      Direction: FoldMapDirection
      StateUpdate: StateUpdateMode -> 'State -> AstNode -> 'State
      Map: 'State -> AstNode -> NodeTransformResult<'Msg> }

module FoldMap =

    /// Inner recursive call.
    let rec internal loop (state: 'State)
                          (node: AstNode)
                          (parameters: FoldMapParameters<'State, 'Msg>)
                          : 'State * GslResult<AstNode, 'Msg> =

        /// Fold over this node and produce an updated state.
        let preTransformedState =
            parameters.StateUpdate PreTransform state node

        // depending on the direction switch, either first transform the node and then its children,
        // or transform the children first and then the current node.  This is the difference between
        // rebuilding the tree top-down and bottom-up.
        let transformedTree =
            match parameters.Direction with
            | TopDown ->
                parameters.Map preTransformedState node
                >>= transformChildren preTransformedState parameters
            | BottomUp ->
                // transform the children of the node
                transformChildren preTransformedState parameters node
                // then transform the node with transformed children
                >>= parameters.Map preTransformedState

        // run the state update function in PostTransform mode, using the original node
        // this allows block-scoped cleanup to occur.
        let postTransformedState =
            parameters.StateUpdate PostTransform preTransformedState node
        // return the transformed node and the state from folding only it, not its children
        postTransformedState, transformedTree

    /// Recursive into the children of a node.
    and internal transformChildren (state: 'State)
                                   (parameters: FoldMapParameters<'State, 'Msg>)
                                   (node: AstNode)
                                   : GslResult<AstNode, 'Msg> =
        // recurse into children of the revised node
        match node with
        | Leaf leaf -> GslResult.ok leaf // leaf nodes need no recursion
        | AstNode.VariableBinding variableBindingWrapper ->
            processVariableBinding state parameters variableBindingWrapper
        | AstNode.TypedValue typedValueWrapper -> processTypedValue state parameters typedValueWrapper
        | AstNode.BinaryOperation binaryOperationWrapper ->
            processBinaryOperation state parameters binaryOperationWrapper
        | AstNode.Negation negationWrapper -> processNegation state parameters negationWrapper
        | AstNode.ParseRelPos relativePositionWrapper ->
            processRelativePosition state parameters relativePositionWrapper
        | AstNode.Slice sliceWrapper -> processSlice state parameters sliceWrapper
        | AstNode.Part partWrapper -> processPart state parameters partWrapper
        | AstNode.L2Element level2Wrapper -> processL2Element state parameters level2Wrapper
        | AstNode.L2Expression expressionWrapper -> processL2Expression state parameters expressionWrapper
        | AstNode.ParsePragma pragmaWrapper -> processPragma state parameters pragmaWrapper
        | AstNode.Block blockWrapper ->
            match parameters.Mode with
            | Parallel -> processBlockParallel state parameters blockWrapper
            | Serial -> processBlock state parameters blockWrapper
        // Function definition and call
        | AstNode.FunctionDef functionDefinitionWrapper ->
            processFunctionDefinition state parameters functionDefinitionWrapper
        | AstNode.FunctionCall functionCallWrapper -> processFunctionCall state parameters functionCallWrapper
        | AstNode.Assembly assemblyWrapper -> processAssembly state parameters assemblyWrapper
        | x -> Utils.nonExhaustiveError x

    /// Make the recursive call to fold, with updated state from this node.
    /// Discard the state returned from the call.
    /// The only recursive call which should not use this variant is that made
    /// in processBlock, to ensure that state only accumulates over top-level nodes inside
    /// blocks.
    and internal foldAndDropState (state: 'State)
                                  (parameters: FoldMapParameters<'State, 'Msg>)
                                  (node: AstNode)
                                  : GslResult<AstNode, 'Msg> =
        snd (loop state node parameters)

    and internal processRelativePosition (state: 'State)
                                         (parameters: FoldMapParameters<'State, 'Msg>)
                                         (node: Node<ParseRelativePosition>)
                                         : GslResult<AstNode, 'Msg> =
        foldAndDropState state parameters node.Value.Item
        |> GslResult.map (fun updatedValue ->
            let updatedWrapper = { node.Value with Item = updatedValue }

            AstNode.ParseRelPos { node with Value = updatedWrapper })

    and internal processSlice (state: 'State)
                              (parameters: FoldMapParameters<'State, 'Msg>)
                              (node: Node<ParseSlice>)
                              : GslResult<AstNode, 'Msg> =
        let newLeft =
            foldAndDropState state parameters node.Value.Left

        let newRight =
            foldAndDropState state parameters node.Value.Right

        (newLeft, newRight)
        ||> GslResult.map2 (fun newLeft newRight ->
                let updatedWrapper =
                    { node.Value with
                          Left = newLeft
                          Right = newRight }

                AstNode.Slice { node with Value = updatedWrapper })

    and internal processL2Element (state: 'State)
                                  (parameters: FoldMapParameters<'State, 'Msg>)
                                  (node: Node<L2Element>)
                                  : GslResult<AstNode, 'Msg> =
        let newPromoter =
            foldAndDropState state parameters node.Value.Promoter

        let newTarget =
            foldAndDropState state parameters node.Value.Target

        (newPromoter, newTarget)
        ||> GslResult.map2 (fun newPromoter newTarget ->
                let updatedWrapperValue =
                    { node.Value with
                          Promoter = newPromoter
                          Target = newTarget }

                AstNode.L2Element
                    ({ node with
                           Value = updatedWrapperValue }))

    and internal processPragma (state: 'State)
                               (parameters: FoldMapParameters<'State, 'Msg>)
                               (node: Node<ParsePragma>)
                               : GslResult<AstNode, 'Msg> =
        node.Value.Values
        |> List.map (foldAndDropState state parameters)
        |> GslResult.collectA
        |> GslResult.map (fun newVals ->
            let updatedValue = { node.Value with Values = newVals }

            AstNode.ParsePragma { node with Value = updatedValue })

    and internal processFunctionDefinition (state: 'State)
                                           (parameters: FoldMapParameters<'State, 'Msg>)
                                           (node: Node<ParseFunction>)
                                           : GslResult<AstNode, 'Msg> =
        foldAndDropState state parameters node.Value.Body
        |> GslResult.map (fun newBody ->
            let updatedValue = { node.Value with Body = newBody }

            AstNode.FunctionDef { node with Value = updatedValue })

    and internal processFunctionCall (state: 'State)
                                     (parameters: FoldMapParameters<'State, 'Msg>)
                                     (node: Node<FunctionCall>)
                                     : GslResult<AstNode, 'Msg> =
        GslResult.collectA (List.map (foldAndDropState state parameters) node.Value.Arguments)
        |> GslResult.map (fun newArgs ->
            let updatedValue = { node.Value with Arguments = newArgs }

            AstNode.FunctionCall { node with Value = updatedValue })

    and internal processAssembly (state: 'State)
                                 (parameters: FoldMapParameters<'State, 'Msg>)
                                 (node: Node<AstNode list>)
                                 : GslResult<AstNode, 'Msg> =
        GslResult.collectA (List.map (foldAndDropState state parameters) node.Value)
        |> GslResult.map (fun newParts -> AstNode.Assembly { node with Value = newParts })

    // Parts have quite a few children, so break this out as a function for cleanliness.
    and internal processPart (state: 'State)
                             (parameters: FoldMapParameters<'State, 'Msg>)
                             (partWrapper: Node<ParsePart>)
                             : GslResult<AstNode, 'Msg> =
        let parsePart = partWrapper.Value

        let updatedBasePart =
            foldAndDropState state parameters parsePart.BasePart

        let updatedModifiers =
            parsePart.Modifiers
            |> List.map (foldAndDropState state parameters)
            |> GslResult.collectA

        let updatedPragmas =
            parsePart.Pragmas
            |> List.map (foldAndDropState state parameters)
            |> GslResult.collectA

        (updatedBasePart, updatedModifiers, updatedPragmas)
        |||> GslResult.map3 (fun basePart modifiers pragmas ->
                 let updatedParsePart =
                     { parsePart with
                           BasePart = basePart
                           Modifiers = modifiers
                           Pragmas = pragmas }

                 AstNode.Part
                     { partWrapper with
                           Value = updatedParsePart })

    // L2 expressions are a bit complicated, so break this out for cleanliness.
    and internal processL2Expression (state: 'State)
                                     (parameters: FoldMapParameters<'State, 'Msg>)
                                     (level2Wrapper: Node<L2Expression>)
                                     : GslResult<AstNode, 'Msg> =
        let level2Expression = level2Wrapper.Value

        let updatedLocus =
            level2Expression.Locus
            |> GslResult.optionalResult (foldAndDropState state parameters)

        let updatedParts =
            level2Expression.Parts
            |> List.map (foldAndDropState state parameters)
            |> GslResult.collectA

        (updatedLocus, updatedParts)
        ||> GslResult.map2 (fun locus parts ->
                let updatedExpression =
                    { level2Expression with
                          Locus = locus
                          Parts = parts }

                AstNode.L2Expression
                    { level2Wrapper with
                          Value = updatedExpression })
    ///<summary>
    /// Here is the tricky business of ensuring the state accumulates over a block but resets
    /// at the end.  Because it is ambiguous which state to return if the tree branches below
    /// this level, we only cascade the state after processing the node one level below this
    /// and no further.  So, simple rule: anything that needs to involve block-scoped state update
    /// needs to be a rule that operates on an immediate child of a block.</summary>
    and internal processBlock (state: 'State)
                              (parameters: FoldMapParameters<'State, 'Msg>)
                              (blockWrapper: Node<AstNode list>)
                              : GslResult<AstNode, 'Msg> =
        /// Folding function that collects the results of node transformation while accumulating state.
        /// The resulting list of nodes needs to be reversed.
        let foldAndAccum (s, transformedNodes) n =
            let newState, transformedNode = loop s n parameters
            (newState, transformedNode :: transformedNodes)

        // The _ on the line below drops the state accumulated over the block.
        let (_, newLineResults) =
            List.fold foldAndAccum (state, []) blockWrapper.Value
        // merge the new line results into one result
        newLineResults
        |> List.rev
        |> GslResult.collectA
        |> GslResult.map (fun newLines -> AstNode.Block({ blockWrapper with Value = newLines }))


    /// Performs processing of each node in a block in parallel, with the same semantics as serial.
    and internal processBlockParallel (state: 'State)
                                      (parameters: FoldMapParameters<'State, 'Msg>)
                                      (blockWrapper: Node<AstNode list>)
                                      : GslResult<AstNode, 'Msg> =
        let nodeArray = blockWrapper.Value |> Array.ofList
        // We need to compute all of the block-accumulated state up-front and pass it in to each step.
        // This implies a bit of redundent computation as each inner call will recompute.
        let computeStateForNextNode stateIn n =
            let pretransState =
                parameters.StateUpdate PreTransform stateIn n

            parameters.StateUpdate PostTransform pretransState n

        let statesForNodes =
            nodeArray
            |> Array.fold (fun inputStates n ->
                let outputState =
                    computeStateForNextNode (List.head inputStates) n

                outputState :: inputStates) [ state ]
            |> List.tail // we don't need the output from the last node
            |> List.rev
            |> List.toArray

        assert (statesForNodes.Length = nodeArray.Length)

        let nCores = System.Environment.ProcessorCount
        // leave one for the OS, make sure at least 1!
        let useNCores = nCores - 1 |> max 1

        Array.zip statesForNodes nodeArray
        |> PSeq.map (fun (inputState, n) -> loop inputState n parameters |> snd)
        |> PSeq.withDegreeOfParallelism useNCores
        |> PSeq.toList
        |> GslResult.collectA
        |> GslResult.map (fun newLines -> AstNode.Block({ blockWrapper with Value = newLines }))

    and internal processVariableBinding (state: 'State)
                                        (parameters: FoldMapParameters<'State, 'Msg>)
                                        (node: Node<VariableBinding>)
                                        : GslResult<AstNode, 'Msg> =
        (foldAndDropState state parameters node.Value.Value)
        |> GslResult.map (fun newInner ->
            let updatedNodeValue = { node.Value with Value = newInner }

            AstNode.VariableBinding { node with Value = updatedNodeValue })

    and internal processTypedValue (state: 'State)
                                   (parameters: FoldMapParameters<'State, 'Msg>)
                                   (node: Node<GslVariableType * AstNode>)
                                   : GslResult<AstNode, 'Msg> =
        let gslType, typedValue = node.Value

        (foldAndDropState state parameters typedValue)
        |> GslResult.map (fun newInner -> AstNode.TypedValue { node with Value = gslType, newInner })

    and internal processBinaryOperation (state: 'State)
                                        (parameters: FoldMapParameters<'State, 'Msg>)
                                        (node: Node<BinaryOperation>)
                                        : GslResult<AstNode, 'Msg> =
        let newLeft =
            foldAndDropState state parameters node.Value.Left

        let newRight =
            foldAndDropState state parameters node.Value.Right

        (newLeft, newRight)
        ||> GslResult.map2 (fun newLeft newRight ->
                let updatedWrapper =
                    { node.Value with
                          Left = newLeft
                          Right = newRight }

                AstNode.BinaryOperation { node with Value = updatedWrapper })

    and internal processNegation (state: 'State)
                                 (parameters: FoldMapParameters<'State, 'Msg>)
                                 (node: Node<AstNode>)
                                 : GslResult<AstNode, 'Msg> =
        foldAndDropState state parameters node.Value
        |> GslResult.map (fun updatedValue -> AstNode.Negation({ node with Value = updatedValue }))

    ///<summary>
    /// Produce a new AST by recursively transforming nodes, keeping track of block-accumulated state.
    /// The transformation can be selected to either operate top-down or bottom-up.  State accumulation
    /// is always top-down, as there is no deterministic way to combine accumulated state from multiple
    /// branches.
    /// The transformation function accepts a data structure representing the current state of the fold.
    /// The state of the fold is updated in top-down order by applying a state update function to the
    /// current node and passing the updated state down into the recursive transform.  State update can
    /// also optionally happen again after a node has been transformed, for instance to clear some state
    /// we want to be captured by a particular type of node.
    /// The block type handles this in a special fashion, however, allowing state accumulation over
    /// successive lines of code to be cascaded.  In other words, state is "block-scoped" and accumulates
    /// over the course of processing a block.  The accumulation of state from line to line in a block does
    /// not recurse below the top level of the block.  At the end of a block, the state accumulated over
    /// the block is discarded.
    /// This function was written as a generic solution to the "variable resolution problem".
    ///</summary>
    // TODO: this function really needs a drawing to be clear.  Document this using a graph example.
    // IMPORTANT: please take great care if editing this algorithm!
    let foldMap (initialState: 'State)
                (parameters: FoldMapParameters<'State, 'Msg>)
                (tree: AstTreeHead)
                : TreeTransformResult<'Msg> =
        let _, newTree =
            loop initialState tree.wrappedNode parameters

        newTree |> GslResult.map AstTreeHead

    ///<summary>
    /// Produce a new AST by recursively applying a function to every node in the tree.
    /// The tree will be rebuilt either from the bottom-up or the top-down.
    /// Top-down order should be used when a transformation is going to be increasing the branching of
    /// some part of the tree, like resolving a variable to an arbitrary expression.
    /// Bottom-up order should be used when a transformation is going to be decreasing the branching of
    /// some part of the tree, like collapsing binary expressions into a single value.
    ///</summary>
    let map mode direction f tree =
        let parameters =
            { FoldMapParameters.Direction = direction
              Mode = mode
              StateUpdate = fun _ _ _ -> ()
              Map = fun _ -> f }

        foldMap () parameters tree


type ValidationResult = AstResult<unit>


module Validation =
    /// Validation success with no warning.
    let good: ValidationResult = GslResult.ok ()


    /// Map a validation function over the tree.
    /// If successful, passes the tree through.
    /// Enforces that the tree remains the same by only accepting a function that returns nothing.
    /// Can perform multiple validations in parallel on each node by combining the node validation
    /// functions with the &&& infix operator.
    let validate (f: AstNode -> GslResult<unit, 'a>) tree =
        // we can express this operation using map and by doctoring the inputs and outputs.
        // map requires a function that produces a transformation result.
        let fPassThru (node: AstNode) = f node |> GslResult.map (fun _ -> node) // splice the node into successful validation result which is always ()

        FoldMap.map Serial TopDown fPassThru tree