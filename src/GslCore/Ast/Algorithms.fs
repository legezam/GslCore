﻿/// Generic algorithms for operating on ASTs.
namespace GslCore.Ast.Algorithms

open GslCore.Ast.Types
open GslCore.Ast.ErrorHandling
open System.Text
open Amyris.ErrorHandling
open GslCore.Constants
open GslCore
open GslCore.Pragma
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
            | Splice _ -> failwithf "Cannot decompile a bootstrap splice marker.  The tree must be spliced first."
            // leaf nodes are easy
            | Int ({ Value = i; Positions = _ }) -> appendf "%d" i
            | Float ({ Value = i; Positions = _ }) -> appendf "%f" i
            | String ({ Value = i; Positions = _ }) -> if state.QuoteStrings then appendf "\"%s\"" i else append i
            | Docstring (dw) -> appendf "///%s" dw.Value
            | TypedVariable ({ Value = (name, _); Positions = _ }) -> appendf "&%s" name
            | TypedValue ({ Value = (_, inner); Positions = _ }) -> _print inner state
            | VariableBinding ({ Value = vb; Positions = _ }) ->
                appendf "let %s = " vb.Name
                _print vb.Value state
            | BinaryOperation ({ Value = binop; Positions = _ }) ->
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
            | Negation ({ Value = inner; Positions = _ }) ->
                append "-"
                _print inner state
            // basic parts
            | Marker _ -> append "###"
            | PartId ({ Value = name; Positions = _ }) -> appendf "@%s" name
            | InlineDna ({ Value = dna; Positions = _ }) -> appendf "/%s/" dna
            | InlineProtein ({ Value = pseq; Positions = _ }) -> appendf "/$%s/" pseq
            | HetBlock _ -> append "~"
            | Gene ({ Value = pg; Positions = _ }) ->
                match pg.Linker with
                | Some ({ Linker1 = l1
                          Linker2 = l2
                          Orient = o }) -> append (sprintf "%s-%s-%s-%s" l1 l2 o pg.Gene)
                | None -> append pg.Gene
            // part mods
            | ParseRelPos ({ Value = rp; Positions = _ }) ->
                _print rp.Item state

                match rp.Qualifier with
                | Some (rpq) -> append (Parts.relPosQualifierToString rpq)
                | None -> ()
            | RelPos ({ Value = rp; Positions = _ }) ->
                appendff
                    "%d%s"
                    rp.Position
                    (match rp.RelativeTo with
                     | FivePrime -> "S"
                     | ThreePrime -> "E")
            | Slice ({ Value = s; Positions = _ }) ->
                append "["
                if s.LeftApprox then append "~"
                _print s.Left state
                append ":"
                if s.RightApprox then append "~"
                _print s.Right state
                append "]"
            | Mutation ({ Value = mut; Positions = _ }) ->
                match mut.Type with
                | AA -> append "$"
                | NT -> append "*"

                append (sprintf "%c%d%c" mut.From mut.Location mut.To)
            | DotMod ({ Value = s; Positions = _ }) -> appendf ".%s" s
            | ParsePragma ({ Value = pp; Positions = _ }) ->
                appendf "#%s" pp.Name

                for v in pp.Values do
                    append " "
                    _print v { state with QuoteStrings = false } // don't quote string pragma values
            | Pragma ({ Value = p; Positions = _ }) ->
                appendf "#%s" p.Definition.Name

                for arg in p.Arguments do
                    appendf " %s" arg
            | FunctionDef ({ Value = fd; Positions = _ }) ->
                // declaration line
                appendff "let %s(%s) =" fd.Name (String.concat ", " fd.ArgumentNames)
                newline ()
                // print the enclosed block
                _print fd.Body state
                minorIndent ()
                append "end"
            | FunctionCall ({ Value = fc; Positions = _ }) ->
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
            | Part ({ Value = p; Positions = _ }) ->
                // Print the base part.
                // Need to make sure subassemblies are grouped in parens.
                // TODO: decide what we want to do about assemblies with only one part.
                let enclose =
                    state.IsInsideAssembly
                    && match p.BasePart with
                       | Assembly _ -> true
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
            | Assembly ({ Value = parts; Positions = _ }) ->
                // print all the parts in the assembly separated by semicolons
                printSemicolonSeparatedList parts
            | L2Id (lw) -> append lw.Value.String
            | L2Element (lw) ->
                _print lw.Value.Promoter state
                append ">"
                _print lw.Value.Target state
            | L2Expression (lw) ->
                let nParts = lw.Value.Parts.Length

                match lw.Value.Locus with
                | Some (l) ->
                    _print l state
                    append "^"
                    if nParts > 0 then append " ; "
                | None -> ()

                printSemicolonSeparatedList lw.Value.Parts
            | Roughage ({ Value = rLine; Positions = _ }) ->
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
            | Block ({ Value = lines; Positions = _ }) ->
                for l in lines do
                    let printInnerLine () =
                        _print
                            l
                            { state with
                                  IndentLevel = state.IndentLevel + 1 }

                    match l with
                    | BookkeepingNode _ -> () // ignore bookkeeping nodes in blocks
                    | Block _ ->
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

            | ParseError (x) -> failwithf "Parse error found during AST code generation: %A" x
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
        | VariableBinding (vb) -> Seq.singleton vb.Value.Value
        // typed value
        | TypedValue ({ Value = (_, n); Positions = _ }) -> Seq.singleton n
        // Simple operations on values
        | BinaryOperation ({ Value = binOp; Positions = _ }) ->
            seq {
                yield binOp.Left
                yield binOp.Right
            }
        | Negation ({ Value = n; Positions = _ }) -> Seq.singleton n
        // Slicing
        | ParseRelPos ({ Value = { Item = n; Qualifier = _ }
                         Positions = _ }) -> Seq.singleton n
        | Slice ({ Value = slice; Positions = _ }) ->
            seq {
                yield slice.Left
                yield slice.Right
            }
        // generic part with mods, pragmas, direction
        | Part ({ Value = part; Positions = _ }) ->
            seq {
                yield part.BasePart
                yield! Seq.ofList part.Modifiers
                yield! Seq.ofList part.Pragmas
            }
        // L2 elements
        | L2Element (lw) ->
            seq {
                yield lw.Value.Promoter
                yield lw.Value.Target
            }
        | L2Expression (lw) ->
            seq {
                match lw.Value.Locus with
                | Some (l) -> yield l
                | None -> ()

                yield! Seq.ofList lw.Value.Parts
            }
        // pragmas
        | ParsePragma ({ Value = pp; Positions = _ }) -> Seq.ofList pp.Values
        // Block of code
        | Block ({ Value = nodes; Positions = _ }) -> Seq.ofList nodes
        // Function definition and call
        | FunctionDef ({ Value = f; Positions = _ }) -> Seq.singleton f.Body
        | FunctionCall ({ Value = fc; Positions = _ }) -> Seq.ofList fc.Arguments
        | Assembly ({ Value = parts; Positions = _ }) -> Seq.ofList parts
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

type NodeTransformResult = Result<AstNode, AstMessage>

type TreeTransformResult = Result<AstTreeHead, AstMessage>

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

type FoldMapParameters<'State> =
    { Mode: FoldmapMode
      Direction: FoldMapDirection
      StateUpdate: StateUpdateMode -> 'State -> AstNode -> 'State
      Map: 'State -> AstNode -> NodeTransformResult }

module FoldMap =

    /// Inner recursive call.
    let rec private loop (state: 'State)
                         (node: AstNode)
                         (parameters: FoldMapParameters<'State>)
                         : 'State * Result<AstNode, AstMessage> =

        /// Fold over this node and produce an updated state.
        let updatedState =
            parameters.StateUpdate PreTransform state node

        // depending on the direction switch, either first transform the node and then its children,
        // or transform the children first and then the current node.  This is the difference between
        // rebuilding the tree top-down and bottom-up.
        let transformedTree =
            match parameters.Direction with
            | TopDown ->
                parameters.Map updatedState node
                >>= (transformChildren updatedState parameters)
            | BottomUp ->
                // transform the children of the node
                transformChildren updatedState parameters node
                // then transform the node with transformed children
                >>= parameters.Map updatedState

        // run the state update function in PostTransform mode, using the original node
        // this allows block-scoped cleanup to occur.
        let finalState =
            parameters.StateUpdate PostTransform updatedState node
        // return the transformed node and the state from folding only it, not its children
        finalState, transformedTree

    /// Make the recursive call to fold, with updated state from this node.
    /// Discard the state returned from the call.
    /// The only recursive call which should not use this variant is that made
    /// in processBlock, to ensure that state only accumulates over top-level nodes inside
    /// blocks.
    and private foldDropState (state: 'State)
                              (parameters: FoldMapParameters<'State>)
                              (node: AstNode)
                              : Result<AstNode, AstMessage> =
        snd (loop state node parameters)
    // Parts have quite a few children, so break this out as a function for cleanliness.
    and private processPart (state: 'State)
                            (parameters: FoldMapParameters<'State>)
                            (partWrapper: Node<ParsePart>)
                            : Result<AstNode, AstMessage> =
        let parsePart = partWrapper.Value

        let updatedBasePart =
            foldDropState state parameters parsePart.BasePart

        let updatedModifiers =
            parsePart.Modifiers
            |> List.map (foldDropState state parameters)
            |> collect

        let updatedPragmas =
            parsePart.Pragmas
            |> List.map (foldDropState state parameters)
            |> collect

        tupleResults3 updatedBasePart updatedModifiers updatedPragmas
        >>= fun (basePart, modifiers, pragmas) ->
                let updatedParsePart =
                    { parsePart with
                          BasePart = basePart
                          Modifiers = modifiers
                          Pragmas = pragmas }

                Part
                    { partWrapper with
                          Value = updatedParsePart }
                |> ok
    // L2 expressions are a bit complicated, so break this out for cleanliness.
    and private processL2Expression (state: 'State)
                                    (parameters: FoldMapParameters<'State>)
                                    (level2Wrapper: Node<L2Expression>)
                                    : Result<AstNode, AstMessage> =
        let level2Expression = level2Wrapper.Value

        let updatedLocus =
            level2Expression.Locus
            |> optionalResult (foldDropState state parameters)

        let updatedParts =
            level2Expression.Parts
            |> List.map (foldDropState state parameters)
            |> collect

        tupleResults updatedLocus updatedParts
        >>= fun (locus, parts) ->
                let updatedExpression =
                    { level2Expression with
                          Locus = locus
                          Parts = parts }

                L2Expression
                    { level2Wrapper with
                          Value = updatedExpression }
                |> ok
    ///<summary>
    /// Here is the tricky business of ensuring the state accumulates over a block but resets
    /// at the end.  Because it is ambiguous which state to return if the tree branches below
    /// this level, we only cascade the state after processing the node one level below this
    /// and no further.  So, simple rule: anything that needs to involve block-scoped state update
    /// needs to be a rule that operates on an immediate child of a block.</summary>
    and private processBlock (state: 'State)
                             (parameters: FoldMapParameters<'State>)
                             (blockWrapper: Node<AstNode list>)
                             : Result<AstNode, AstMessage> =
        /// Folding function that collects the results of node transformation while accumulating state.
        /// The resulting list of nodes needs to be reversed.
        let foldAndAccum (s, transformedNodes) n =
            let newState, transformedNode = loop s n parameters
            (newState, transformedNode :: transformedNodes)

        // The _ on the line below drops the state accumulated over the block.
        let (_, newLineResults) =
            List.fold foldAndAccum (state, []) blockWrapper.Value
        // merge the new line results into one result
        collect (Seq.rev newLineResults)
        >>= (fun newLines -> ok (Block({ blockWrapper with Value = newLines })))

    /// Performs processing of each node in a block in parallel, with the same semantics as serial.
    and private processBlockParallel (state: 'State)
                                     (parameters: FoldMapParameters<'State>)
                                     (blockWrapper: Node<AstNode list>)
                                     : Result<AstNode, AstMessage> =
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
            |> Array.ofList

        assert (statesForNodes.Length = nodeArray.Length)

        let nCores = System.Environment.ProcessorCount
        // leave one for the OS, make sure at least 1!
        let useNCores = nCores - 1 |> max 1

        Array.zip statesForNodes nodeArray
        |> PSeq.map (fun (inputState, n) -> loop inputState n parameters |> snd)
        |> PSeq.withDegreeOfParallelism useNCores
        |> PSeq.toArray
        |> collect
        >>= (fun newLines -> ok (Block({ blockWrapper with Value = newLines })))
    /// Recursive into the children of a node.
    and private transformChildren (state: 'State)
                                  (parameters: FoldMapParameters<'State>)
                                  (node: AstNode)
                                  : Result<AstNode, AstMessage> =
        // recurse into children of the revised node
        match node with
        | Leaf leaf -> ok leaf // leaf nodes need no recursion
        | VariableBinding variableBindingWrapper ->
            (foldDropState state parameters variableBindingWrapper.Value.Value)
            >>= fun newInner ->
                    let updatedWrapper =
                        { variableBindingWrapper.Value with
                              Value = newInner }

                    VariableBinding
                        { variableBindingWrapper with
                              Value = updatedWrapper }
                    |> ok
        | TypedValue typedValueWrapper ->
            let gslType, typedValue = typedValueWrapper.Value

            (foldDropState state parameters typedValue)
            >>= fun newInner ->
                    TypedValue
                        { typedValueWrapper with
                              Value = (gslType, newInner) }
                    |> ok

        | BinaryOperation bindaryOperationWrapper ->
            let newLeft =
                foldDropState state parameters bindaryOperationWrapper.Value.Left

            let newRight =
                foldDropState state parameters bindaryOperationWrapper.Value.Right

            tupleResults newLeft newRight
            >>= fun (newLeft, newRight) ->
                    let updatedWrapper =
                        { bindaryOperationWrapper.Value with
                              Left = newLeft
                              Right = newRight }

                    BinaryOperation
                        { bindaryOperationWrapper with
                              Value = updatedWrapper }
                    |> ok
        | Negation negationWrapper ->
            foldDropState state parameters negationWrapper.Value
            >>= fun node -> ok (Negation({ negationWrapper with Value = node }))
        // Slicing
        | ParseRelPos relativePositionWrapper ->
            foldDropState state parameters relativePositionWrapper.Value.Item
            >>= fun node ->
                    let updatedWrapper =
                        { relativePositionWrapper.Value with
                              Item = node }

                    ParseRelPos
                        { relativePositionWrapper with
                              Value = updatedWrapper }
                    |> ok
        | Slice sliceWrapper ->
            let newLeft =
                foldDropState state parameters sliceWrapper.Value.Left

            let newRight =
                foldDropState state parameters sliceWrapper.Value.Right

            tupleResults newLeft newRight
            >>= fun (newLeft, newRight) ->
                    let updatedWrapper =
                        { sliceWrapper.Value with
                              Left = newLeft
                              Right = newRight }

                    Slice
                        { sliceWrapper with
                              Value = updatedWrapper }
                    |> ok
        | Part partWrapper -> processPart state parameters partWrapper
        | L2Element level2Wrapper ->
            let newPromoter =
                foldDropState state parameters level2Wrapper.Value.Promoter

            let newTarget =
                foldDropState state parameters level2Wrapper.Value.Target

            tupleResults newPromoter newTarget
            >>= fun (newPromoter, newTarget) ->
                    let updatedWrapperValue =
                        { level2Wrapper.Value with
                              Promoter = newPromoter
                              Target = newTarget }

                    L2Element
                        ({ level2Wrapper with
                               Value = updatedWrapperValue })
                    |> ok
        | L2Expression expressionWrapper -> processL2Expression state parameters expressionWrapper
        | ParsePragma pragmaWrapper ->
            pragmaWrapper.Value.Values
            |> List.map (foldDropState state parameters)
            |> collect
            >>= fun newVals ->
                    let updated =
                        { pragmaWrapper.Value with
                              Values = newVals }

                    ParsePragma { pragmaWrapper with Value = updated }
                    |> ok
        | Block blockWrapper ->
            match parameters.Mode with
            | Parallel -> processBlockParallel state parameters blockWrapper
            | Serial -> processBlock state parameters blockWrapper
        // Function definition and call
        | FunctionDef functionDefinitionWrapper ->
            foldDropState state parameters functionDefinitionWrapper.Value.Body
            >>= fun newBody ->
                    let updated =
                        { functionDefinitionWrapper.Value with
                              Body = newBody }

                    FunctionDef
                        { functionDefinitionWrapper with
                              Value = updated }
                    |> ok
        | FunctionCall functionCallWrapper ->
            collect (List.map (foldDropState state parameters) functionCallWrapper.Value.Arguments)
            >>= fun newArgs ->
                    let updated =
                        { functionCallWrapper.Value with
                              Arguments = newArgs }

                    FunctionCall
                        { functionCallWrapper with
                              Value = updated }
                    |> ok
        | Assembly assemblyWrapper ->
            collect (List.map (foldDropState state parameters) assemblyWrapper.Value)
            >>= fun newParts ->
                    Assembly
                        { assemblyWrapper with
                              Value = newParts }
                    |> ok
        | x -> Utils.nonExhaustiveError x

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
    // FIXME: this function really needs a drawing to be clear.  Document this using a graph example.
    // IMPORTANT: please take great care if editing this algorithmn!
    let foldMap (initialState: 'State) (parameters: FoldMapParameters<'State>) (tree: AstTreeHead): TreeTransformResult =
        let _, newTree =
            loop initialState tree.wrappedNode parameters

        newTree |> lift AstTreeHead

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


type ValidationResult = Result<unit, AstMessage>


module Validation =
    /// Validation success with no warning.
    let good: ValidationResult = ok ()


    /// Map a validation function over the tree.
    /// If successful, passes the tree through.
    /// Enforces that the tree remains the same by only accepting a function that returns nothing.
    /// Can perform multiple validations in parallel on each node by combining the node validation
    /// functions with the &&& infix operator.
    let validate (f: AstNode -> ValidationResult) tree =
        // we can express this operation using map and by doctoring the inputs and outputs.
        // map requires a function that produces a transformation result.
        let fPassThru (node: AstNode) = f node >>= (fun _ -> ok node) // splice the node into successful validation result which is always ()

        FoldMap.map Serial TopDown fPassThru tree