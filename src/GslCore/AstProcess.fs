﻿/// Processing and validation of AST nodes and trees.
/// Non-bioinformatic tree transformation algorithms also live here.
module GslCore.AstProcess

open Amyris.ErrorHandling
open Amyris.Dna
open GslCore.AstTypes
open GslCore.AstErrorHandling
open GslCore.AstAlgorithms
open GslCore.RefGenome
open GslCore.Constants
open GslCore.Pragma

// ==========================
// Helper functions to ease working with pragmas and parts.
// ==========================

/// Get a part's list of built pragmas, represented as a PragmaCollection.
/// This function will fail if it finds unbuilt pragmas.
let getPragmasStrict (part: Node<ParsePart>): Result<PragmaCollection, AstMessage> =
    let getBuiltPragma (node: AstNode) =
        match node with
        | Pragma pragmaWrapper -> ok (pragmaWrapper.x)
        | ParsePragma parsePragmaWrapper -> unbuiltPragmaError None parsePragmaWrapper.x.name node
        | x -> internalTypeMismatch None "Pragma" x

    part.x.pragmas
    |> List.map getBuiltPragma
    |> collect
    >>= (fun pragmas ->
        PragmaCollection.empty
        |> PragmaCollection.mergeInPragmas pragmas
        |> ok)

/// Get a part's list of built pragmas, represented as a PragmaCollection.
/// Raises an exception if pragmas are not built.
let getPragmas (part: Node<ParsePart>): PragmaCollection =
    match getPragmasStrict part with
    | Ok (pc, _) -> pc
    | Bad (errs) ->
        failwith
            (errs
             |> List.map (fun m -> m.Summary)
             |> String.concat "\n")

///<summary>
/// Replace a part's pragmas with a converted version of a pragma collection.
/// Note that this conversion produces Ast nodes without source code positions.
/// We may want to eventually refactor pragmas to be able to remember this information,
/// or force pragma collections to use NodeWrappers instead.  We may never need this information,
/// though, so wait and see.
///</summary>
let replacePragmas (part: Node<ParsePart>) (pragmaCollection: PragmaCollection): Node<ParsePart> =
    let astPragmas =
        pragmaCollection
        |> PragmaCollection.values
        |> Seq.map (fun pragma -> Pragma(nodeWrap pragma))
        |> List.ofSeq

    { part with
          x = { part.x with pragmas = astPragmas } }

/// Merge a pragma collection into a part, clobbering existing pragmas.
/// Add a warning if there are any collisions.
let mergePragmas (parsePart: Node<ParsePart>) (pragmaCollection: PragmaCollection): Result<Node<ParsePart>, AstMessage> =
    getPragmasStrict parsePart
    >>= (fun partPragmas ->
        let namesFromPragmaCollection =
            pragmaCollection |> PragmaCollection.names

        let namesFromParseParts = partPragmas |> PragmaCollection.names

        let collidingPragmas =
            Set.intersect namesFromPragmaCollection namesFromParseParts

        let newPart =
            let mergedPragmas =
                partPragmas
                |> PragmaCollection.mergeInCollection pragmaCollection

            replacePragmas parsePart mergedPragmas

        if collidingPragmas |> Set.isEmpty then
            ok newPart
        else
            let warning =
                warningMessage
                    (sprintf "Pragma collision(s): %s" (collidingPragmas |> String.concat ", "))
                    (Part(parsePart))

            warn warning newPart)

// ====================
// validation routines
// ====================

/// Return an error if this node is a parse error.
let checkParseError node =
    match node with
    | ParseError (ew) -> error ParserError ew.x node
    | _ -> good

// ===============
// validation of parts
// ===============

let validatePart op node =
    match node with
    | Part ({ x = pp; positions = _ }) -> op pp
    | _ -> good

// FIXME: this may be either a step too far, or just on example of something we need a lot more of
// Ideally the parser structure should make this kind of check unnecessary.
let private validBasePartPP pp =
    match pp.basePart with
    | ValidBasePart _ -> good
    | x -> errorf (InternalError(PartError)) "%s is not a valid base part." x.TypeName x

let validBasePart = validatePart validBasePartPP

// validtion functions on ParseParts
let private checkModsPP pp =
    if not pp.mods.IsEmpty then
        match pp.basePart with
        | Gene _ -> good
        | PartId _ -> good
        | x -> errorf PartError "Can only apply part mods to Gene or PartId, not %s" x.TypeName x
    else
        good

let checkMods = validatePart checkModsPP

// ===================
// refusing to compile recursive function calls
// ===================

/// Maintain a stack of the function defintion context.
let private updateRecursiveCheckState mode (s: string list) node =
    match node with
    | FunctionDef (fd) ->
        match mode with
        | PreTransform -> fd.x.name :: s
        | PostTransform ->
            match s with
            | [] -> []
            | _ :: tl -> tl
    | _ -> s

/// If we find a function call to a function def we're already inside, fail.
let private checkRecursiveCall (s: string list) node =
    match node with
    | FunctionCall (fc) when s |> List.contains fc.x.name ->
        errorf
            RecursiveFunctionCall
            "Found a recursive call to '%s'. GSL does not support recursive functions."
            fc.x.name
            node
    | _ -> ok node

/// Fail if a GSL program contains recursively-defined functions.
let checkRecursiveCalls =
    foldmap Serial TopDown updateRecursiveCheckState [] checkRecursiveCall


// ===================
// variable resolution
// ===================

/// Wrapper type for variable resolution.  Node helper functions below.
type VariableResolutionWrapper =
    | VBinding of Node<VariableBinding>
    | FLocal

/// Map to keep track of what variables and function locals are in scope.
/// Function locals are items in the map with None rather that an explicit binding.
/// Shadowing is allowed, and the latest declared name takes precedence.
type VariableBindings = Map<string, VariableResolutionWrapper>

let private addBinding (bindings: VariableBindings) (vb: Node<VariableBinding>) = bindings.Add(vb.x.name, VBinding vb)
let private addFuncLocal (bindings: VariableBindings) name = bindings.Add(name, FLocal)

/// Given an AST node, update the variable resolution state.
/// We need to be a little careful here due to a tricky issue.
/// Namely, we need to ensure that the construction let bar = &bar
/// doesn't wipe out the upstream binding to bar, lest we end up
/// in an infinite recursive loop trying to resolve a self-reference.
let private updateVariableResolutionInner (s: VariableBindings) n =
    match n with
    | SelfReferentialVariable _ ->
        // variable that aliases itself from an outer scope.  Ignore this.
        s
    | VariableBinding (vb) -> addBinding s vb
    | FunctionLocals (pf) -> pf.x.names |> List.fold addFuncLocal s
    | _ -> s

let private updateVariableResolution =
    pretransformOnly updateVariableResolutionInner

type VariableResolutionMode =
    | Strict
    | AllowUnresolvedFunctionLocals

/// Elide a type for a value node, if it corresponds to a valid GslVarType.
let private elideType node =
    match node with
    | Part _ -> Some(PartType)
    | Int _ -> Some(IntType)
    | Float _ -> Some(FloatType)
    | String _ -> Some(StringType)
    | _ -> None

/// Perform type checking on a variable.
/// If the variable is untyped but has a real payload, try to elide its type.
let private typeCheck varName node targetType boundValueType boundValue =
    if targetType = NotYetTyped
       || targetType = boundValueType then
        // exact type check or destination is not strongly typed
        ok boundValue
    elif boundValueType = NotYetTyped then
        // our value doesn't have type information, see if we can elide it
        match elideType boundValue with
        | Some (elidedType) when elidedType = targetType -> // elides to correct type
            ok boundValue
        | Some (elidedType) -> // elides to incorrect type
            variableTypeMismatch varName elidedType targetType node
        | None -> // whatever this thing is, it shouldn't be inside a variable
            internalTypeMismatch (Some("variable type checking")) (targetType.ToString()) boundValue
    else
        // type mismatch
        variableTypeMismatch varName boundValueType targetType node


/// Resolve a typed variable to a variable declaration.
/// If that declaration itself was a variable aliasing (let foo = &bar), recurse
/// down until we resolve to a fully typed variable.
let rec private resolveVariableRecursive mode (s: VariableBindings) targetType (tv: Node<string * GslVarType>) node =
    let varName, _ = tv.x
    // first see if we have this guy in our bindings at all
    match s.TryFind(varName) with
    | Some (VBinding (v)) -> // this name is resolves to a bound variable
        // does it have the right type in this context?
        let declaredType = v.x.varType

        match declaredType, v.x.value with
        | NotYetTyped, TypedVariable (tvInner) ->
            // if this variable is just a reference to another variable, we need to recurse on it.
            resolveVariableRecursive mode s targetType tvInner node
        | _, boundValue ->
            // otherwise, perform type checking and resolve the variable if it type checks
            typeCheck varName node targetType declaredType boundValue
    | Some (FLocal) -> // This name resolves to a function local variable.  If we're allowing them, continue.
        match mode with
        | AllowUnresolvedFunctionLocals -> ok node
        | Strict ->
            errorf
                (InternalError(UnresolvedVariable))
                "A variable resolved to a function local during strict variable resolution: %s"
                varName
                node
    | None ->
        // unresolved variable!
        error UnresolvedVariable varName node

///Given resolution state and an AST node, possibly resolve a reference.
let private resolveVariable mode (s: VariableBindings) (n: AstNode) =
    match n with
    | TypedVariable (tv) ->
        let targetType = snd tv.x
        // might resolve to another variable, so we need to do this recursively
        resolveVariableRecursive mode s targetType tv n
    | x -> ok x

/// Transform an AST with unresolved scoped variables into a tree with resolved scoped variables.
/// Variables that resolve to function arguments are left untouched in this phase.
let resolveVariables =
    foldmap Serial TopDown updateVariableResolution Map.empty (resolveVariable AllowUnresolvedFunctionLocals)

/// Transform an AST with unresolved scoped variables into a tree with resolved scoped variables.
/// Fails on unresolved function locals.
let resolveVariablesStrict =
    foldmap Serial TopDown updateVariableResolution Map.empty (resolveVariable Strict)

// =====================
// inlining function calls
// =====================

type CollectedFunctionDefs = Map<string, ParseFunction>

type FunctionInliningState =
    { defs: CollectedFunctionDefs
      vars: VariableBindings
      insideDefDepth: int }

let initialInliningState =
    { defs = Map.empty
      vars = Map.empty
      insideDefDepth = 0 }

/// Capture a function definition.
/// Also keep track of whether or not we are inside a function declaration, as we don't inline
/// function calls inside other declarations, only at the final expanded call sites.
let private collectFunctionDef mode (s: FunctionInliningState) node =
    match node with
    | FunctionDef (fw) ->
        match mode with
        | PreTransform ->
            { s with
                  defs = s.defs.Add(fw.x.name, fw.x)
                  insideDefDepth = s.insideDefDepth + 1 }
        | PostTransform ->
            { s with
                  insideDefDepth = s.insideDefDepth - 1 }
    | _ -> s

let private updateFunctionInliningState mode (s: FunctionInliningState) node =
    let sWithNewDefs = collectFunctionDef mode s node

    let updatedVars =
        updateVariableResolution mode s.vars node

    { sWithNewDefs with vars = updatedVars }

/// Check that a function call passed the right number of arguments.
let private checkArgs fd (fc: FunctionCall) fcNode =
    let neededArgs, passedArgs = fd.argNames.Length, fc.args.Length
    // make sure we have the right number of arguments
    if passedArgs <> neededArgs
    then error
             TypeError
             (sprintf "Function '%s' expects %d arguments but received %d." fc.name neededArgs passedArgs)
             fcNode
    else ok (fd, fc)

/// Create a local variable from a typed value.
let private localVarFromTypedValueAndName (vb: VariableBindings) (name, node) =
    match node with
    | TypedValue (tvw) ->
        let (varType, v) = tvw.x
        // using the existing variable bindings, resolve any variables contained in this value
        // this ensures that function locals never resolve to each other.
        AstTreeHead(v)
        |> map Serial TopDown (resolveVariable Strict vb)
        >>= (fun (AstTreeHead (newVal)) ->
            ok
                (VariableBinding
                    ({ x =
                           { name = name
                             varType = varType
                             value = newVal }
                       positions = tvw.positions })))
    | x -> internalTypeMismatch (Some "function call") "typed value" x


/// Inline the passed function args in place of the FunctionLocals placeholder.
/// Return a revised block.
let private inlinePassedArgs (vb: VariableBindings) (fd, fc: FunctionCall) =
    match fd.body with
    | Block (bw) ->
        match bw.x with
        // We require a block whose head is a FunctionLocal or something is fishy.
        | hd :: tl when (match hd with
                         // We require a block whose head is a FunctionLocal or something is fishy.
                         | FunctionLocals _ -> true
                         // We require a block whose head is a FunctionLocal or something is fishy.
                         | _ -> false) ->
            Seq.zip fd.argNames fc.args // zip up the args with the arg names
            |> Seq.map (localVarFromTypedValueAndName vb) // map them to local variables
            |> collect
            // if unpacking and conversion succeeded, make a new block with the
            // variable declarations followed by the rest of the block
            >>= (fun vbs -> ok (Block({ bw with x = vbs @ tl })))
        | _ -> error (InternalError(TypeError)) "No function locals node found in function defintion block." fd.body
    | x -> internalTypeMismatch (Some "function body") "Block" x

/// Replace a function call with the contents of a function definition.
let private inlineFunctionCall (s: FunctionInliningState) (node: AstNode) =
    match node with
    | FunctionCall (fcw) when s.insideDefDepth = 0 -> // only do inlining if we're not inside a def
        let fc = fcw.x

        match s.defs.TryFind(fc.name) with
        | Some (fd) ->
            // Helper function to add new position to an AST node
            let addPositions (node: AstNode) =
                ok (prependPositionsAstNode fcw.positions node)

            // inline the args into the function call block
            // this new block replaces the function call
            checkArgs fd fc node
            >>= inlinePassedArgs s.vars
            |> lift AstTreeHead // needed to adapt to the map function
            >>= map Serial TopDown addPositions
            |> lift (fun treeHead -> treeHead.wrappedNode)

        | None -> error UnresolvedFunction fc.name node
    | _ -> ok node

let inlineFunctionCalls =
    foldmap Serial TopDown updateFunctionInliningState initialInliningState inlineFunctionCall

// =====================
// simplification of binary expressions
// =====================

/// Create an error message for a variable that isn't numeric.
let private numericVariableTypeError t node =
    error TypeError (sprintf "Expecting a numeric variable type, but found %O." t) node


/// Reducde a fully specified binary expression into a single node.
/// Also collapse negations while we're at it.  If we find something we can't negate, return an error.
let private reduceMathExpression node =
    // convenience function for type errors we may come across
    let wrongTypeErrorMsg whichKind (n: AstNode) =
        sprintf "'%s' is not allowed to appear in a %s." n.TypeName whichKind

    let binOpErrMsg =
        wrongTypeErrorMsg "numeric binary operation"

    let negationErrMsg = wrongTypeErrorMsg "negation"

    match node with
    | BinaryOperation ({ x = bo; positions = pos }) ->
        match bo.left, bo.right with
        | Int (l), Int (r) ->
            // two concrete integers, we can operate on them
            let result =
                match bo.op with
                | Add -> l.x + r.x
                | Subtract -> l.x - r.x
                | Multiply -> l.x * r.x
                | Divide -> l.x / r.x

            ok (Int({ x = result; positions = pos }))
        // If we don't have two ints (because one or both are still variables), we can't reduce but
        // this is an OK state of affairs.
        | AllowedInMathExpression _, AllowedInMathExpression _ -> ok node
        // One node is disallowed in a math expression, oh my.
        | AllowedInMathExpression _, x
        | x, AllowedInMathExpression _ -> error TypeError (binOpErrMsg x) x
        // Neither node is allowed here.  Wow, we sure screwed up somewhere.
        | x, y ->
            error TypeError (binOpErrMsg x) x
            |> mergeMessages [ errorMessage TypeError (binOpErrMsg y) y ]
    | Negation ({ x = inner; positions = pos }) ->
        match inner with
        | Int ({ x = i; positions = _ }) ->
            let v = -1 * i
            ok (Int({ x = v; positions = pos }))
        | Float ({ x = i; positions = _ }) ->
            let v = -1.0 * i
            ok (Float({ x = v; positions = pos }))
        // If we have a variable, it should be numeric.  If so, we're ok
        | IntVariable _
        | FloatVariable _ -> ok node
        // Non-numeric variable.  We're in trouble.
        | OtherVariable t -> numericVariableTypeError t inner
        | NotAVariable -> error TypeError (negationErrMsg inner) inner
    | _ -> ok node

let reduceMathExpressions = map Serial BottomUp reduceMathExpression

// ======================
// computing relative positions
// ======================

/// Compute relative positions for slices.
let private buildRelativePosition node =
    match node with
    | ParseRelPos (rpw) ->
        let prp = rpw.x

        let buildNode i e =
            ok
                (RelPos
                    ({ x = { Position = i; RelativeTo = e }
                       positions = rpw.positions }))

        // make sure we have a real value to work with
        match prp.i with
        | Int ({ x = i; positions = _ }) -> ok i
        | x -> internalTypeMismatch (Some "relative position building") "Int" x
        >>= (fun i ->
            match prp.qualifier with
            | None -> buildNode (i * 1<OneOffset>) FivePrime
            | Some (q) ->
                match q, prp.position with
                | S, _ -> buildNode (i * 1<OneOffset>) FivePrime
                | E, _ -> buildNode (i * 1<OneOffset>) ThreePrime
                | A, Left
                | AS, Left
                | SA, Left ->
                    if i > 0
                    then buildNode (i * 3<OneOffset> - 2<OneOffset>) FivePrime
                    else errorf ValueError "Cannot begin with a negative amino acid offset: %d" i prp.i
                | AE, Left
                | EA, Left ->
                    let ai =
                        if i > 0 then (i * 3<OneOffset> - 2<OneOffset>) else (i * 3<OneOffset>)

                    buildNode ai ThreePrime
                | A, Right
                | AS, Right
                | SA, Right ->
                    if i > 0
                    then buildNode (i * 3<OneOffset>) FivePrime
                    else errorf ValueError "Cannot offset negative amino acids from start: %d" i prp.i
                | AE, Right
                | EA, Right ->
                    let ai =
                        if i > 0 then (i * 3<OneOffset>) else (i * 3<OneOffset> + 2<OneOffset>)

                    buildNode ai ThreePrime)
    | _ -> ok node

let buildRelativePositions = map Serial TopDown buildRelativePosition

// =====================
// compiling parsed pragmas into built pragmas
// =====================

// keep track of the enclosing context to catch pragmas that are out of place.
type PragmaConstructionContext =
    | BlockLevel
    | PartLevel

let private updatePragmaConstructionContext mode (s: PragmaConstructionContext list) node =
    let context =
        match node with
        | Block _ -> Some BlockLevel
        | Part _ -> Some PartLevel
        | _ -> None

    match context with
    | Some (c) ->
        match mode with
        | PreTransform -> c :: s
        | PostTransform ->
            match s with
            | [] -> []
            | _ :: tl -> tl
    | None -> s

/// Attempt to build a real pragma from a parsed pragma.
let private compilePragma (legalCapas: Capabilities)
                          (pragmaCache: PragmaCache)
                          (contexts: PragmaConstructionContext list)
                          (node: AstNode)
                          : Result<AstNode, AstMessage> =

    let checkDeprecated (pragma: Pragma): Result<Pragma, AstMessage> =
        match PragmaDeprecation.deprecatedPragmas
              |> Map.tryFind pragma.Name with
        | Some depreciation -> // deprecated pragma, issue a warning and replace it
            let warningMsg =
                createMessage None DeprecationWarning depreciation.WarningMessage node

            let replacedPragma = depreciation.Replace pragma
            warn warningMsg replacedPragma
        | None -> ok pragma

    let checkScope (pragma: Pragma): Result<Pragma, AstMessage> =
        match contexts with
        | headContext :: _ ->
            let errCond =
                match pragma.Definition.Scope, headContext with
                | BlockOnly _, BlockLevel
                | PartOnly, PartLevel
                | BlockOrPart _, _ -> None
                | BlockOnly _, PartLevel -> Some("block-level", "part-level")
                | PartOnly, BlockLevel -> Some("part-level", "block-level")

            match errCond with
            | Some (allowedScope, usedIn) ->
                let msg =
                    sprintf "#%s is used at %s, but is restricted to %s." pragma.Name usedIn allowedScope

                error PragmaError msg node
            | None -> ok pragma
        | [] -> error (InternalError(PragmaError)) "Pragma scope context is empty." node

    // check if this pragma is a capability declaration.
    // if so, validate it.
    let checkCapa (pragma: Pragma): Result<Pragma, AstMessage> =
        if pragma |> Pragma.isCapa then
            let isNotLegalCapa =
                not (legalCapas.Contains(pragma.Arguments.[0]))

            if isNotLegalCapa then
                let goodCapas =
                    legalCapas
                    |> Set.toList
                    |> List.sort
                    |> String.concat ", "

                let msg =
                    sprintf "Undeclared capability: %s.  Declared capabilities are %s" pragma.Name goodCapas

                error PragmaError msg node
            else
                ok pragma
        else
            ok pragma

    let checkPragmaArg: AstNode -> Result<string, AstMessage> =
        function
        | String stringWrapper -> ok stringWrapper.x
        | Int intWrapper -> ok (intWrapper.x.ToString())
        | Float floatWrapper -> ok (floatWrapper.x.ToString())
        | TypedVariable ({ x = (name, _); positions = _ }) as astNode ->
            errorf (InternalError(UnresolvedVariable)) "Unresolved variable in pragma: '%s'" name astNode
        | x -> internalTypeMismatch (Some "pragma value") "String, Int, or Float" x

    match node with
    | ParsePragma pragmaWrapper ->
        let pragma = pragmaWrapper.x
        /// Building pragmas returns strings at the moment.
        /// Wrap them in an AST message.
        // TODO: fix this sad state of affairs once the big changes have landed in default.
        let wrapPragmaErrorString s = errorMessage PragmaError s node

        pragma.values
        |> List.map checkPragmaArg
        |> collect
        >>= (fun values ->
            pragmaCache
            |> Pragma.fromNameValue pragma.name values
            |> (mapMessages wrapPragmaErrorString))
        >>= checkDeprecated
        >>= checkScope
        >>= checkCapa
        >>= (fun builtPragma ->
            ok
                (Pragma
                    ({ x = builtPragma
                       positions = pragmaWrapper.positions })))
    | _ -> ok node

/// Build genuine pragmas from reduced parsed pragmas.
let buildPragmas legalCapas (pragmaCache: PragmaCache) =
    foldmap Serial TopDown updatePragmaConstructionContext [] (compilePragma legalCapas pragmaCache)


// ==========================
// collapsing nested assemblies into a single top-level part
// ==========================

/// Unpack the subparts of the assembly, ignoring anything out of place.
let private unpackParts nodes =
    nodes
    |> List.choose (fun n ->
        match n with
        | Part (pw) -> Some pw
        | _ -> None)

///<summary>
/// Shift any fuse pragmas one slot to the right.
/// Since #fuse directs the compiler to fuse the current part to the part immediately following,
/// and we are about to invert the list, this ensures that pairs of parts still correctly associate
/// around the #fuse pragma.
/// If the assembly has a trailing #fuse, fail.
/// Because the internal fold naturally reverses the list, don't un-reverse it because we need
/// to do this anyway.
///</summary>
let private shiftFusePragmaAndReverseList (parts: Node<ParsePart> list): Result<Node<ParsePart> list, AstMessage> =
    let fusablePragma =
        { Definition = BuiltIn.fusePragmaDef
          Arguments = [] }

    let shiftOne (shiftedParts: Node<ParsePart> list, addFuse: bool) (part: Node<ParsePart>): Node<ParsePart> list * bool =
        let pragmas = getPragmas part
        // if this part has a #fuse, we need to add one to the next part
        let nextNeedsFuse =
            pragmas
            |> PragmaCollection.containsPragma fusablePragma

        let newPart =
            let newPragmas =
                if addFuse then
                    pragmas
                    |> PragmaCollection.addPragma fusablePragma
                else
                    pragmas
                    |> PragmaCollection.removePragma fusablePragma

            replacePragmas part newPragmas

        (newPart :: shiftedParts, nextNeedsFuse)

    let shiftedParts, trailingFuse = parts |> List.fold shiftOne ([], false)

    if trailingFuse
    then error PragmaError "Found a trailing #fuse in an assembly that needs to flip." (Part(List.head shiftedParts))
    else ok shiftedParts

/// Replace any pragmas that invert upon reversal with their inverted version.
let private invertPragma (pragmaCache: PragmaCache) (part: Node<ParsePart>): Node<ParsePart> =
    part
    |> getPragmas
    |> PragmaCollection.values
    |> Seq.map (fun pragma ->
        pragmaCache
        |> PragmaCache.inverts pragma.Definition
        |> Option.map (fun invertsTo -> { pragma with Definition = invertsTo })
        |> Option.defaultValue pragma)
    |> fun collection -> PragmaCollection.create collection pragmaCache
    |> replacePragmas part

///<summary>
/// Explode an assembly into a flat list of parts.
/// This function encodes the logic that used to reside in MULTIPART expansion.
/// This function ignores various unexpected conditions, like nodes besides parts inside the assembly.
///</summary>
let private explodeAssembly (pragmaCache: PragmaCache)
                            (assemblyPart: Node<ParsePart>)
                            (assemblyBasePart: Node<AstNode list>)
                            =
    // This operation is trivial if the assembly is in the forward orientation.
    // If it needs to reverse, it is rather tedious.
    let subparts = unpackParts assemblyBasePart.x

    let correctlyOrientedParts =
        if assemblyPart.x.fwd then
            ok subparts
        else
            subparts
            |> List.map (fun p ->
                { p with
                      x = { p.x with fwd = not p.x.fwd } }) // flip the part
            |> List.map (invertPragma pragmaCache) // flip the pragmas
            |> shiftFusePragmaAndReverseList // shift fuse pragmas one flip to the right, reversing the list
    // now that the parts are correctly oriented, stuff the assembly pragmas into them
    correctlyOrientedParts
    >>= (fun parts ->
        parts
        |> List.map (fun p -> mergePragmas p (getPragmas assemblyPart))
        |> collect
        |> lift (List.map (fun (p: Node<ParsePart>) -> Part(p))))

/// Collapse a part whose base part is another part.
// FIXME: we should probably be more careful with mods here
let private collapseRecursivePart (outerPart: Node<ParsePart>) (innerPart: Node<ParsePart>) =
    let outerPragmas = getPragmas outerPart
    let joinedMods = innerPart.x.mods @ outerPart.x.mods
    let newDir = not (innerPart.x.fwd <> outerPart.x.fwd) // should be rev if one or the other is rev.

    mergePragmas innerPart outerPragmas
    >>= (fun newInner ->
        let newInnerWithOuterMods =
            { newInner with
                  x =
                      { newInner.x with
                            mods = joinedMods
                            fwd = newDir } }

        ok (Part(newInnerWithOuterMods)))

/// Explode any nested assemblies up into the list of parts in the parent assembly.
// FIXME: need to handle mods, and allow only if contents of assembly is a single gene part.
// should use an active pattern to match.
// FIXME: we should probably check for pragma collisions and complain about them, though this is
// before stuffing pragmas into assemblies so it may be an edge case.
let private flattenAssembly (pragmaCache: PragmaCache) node =
    match node with
    | AssemblyPart (assemblyPart, assemblyBasePart) ->
        // iterate over the parts in the assembly, accumulating lists of parts we will concatenate
        assemblyBasePart.x
        |> Seq.map (fun part ->
            match part with
            | AssemblyPart (sap, sabp) -> explodeAssembly pragmaCache sap sabp
            | x -> ok [ x ])
        |> collect
        |> lift (fun partLists ->
            let newBasePart =
                Assembly
                    ({ assemblyBasePart with
                           x = List.concat partLists })

            Part
                ({ assemblyPart with
                       x =
                           { assemblyPart.x with
                                 basePart = newBasePart } }))
    | RecursivePart (outer, inner) ->
        // flatten parts that have another part as their base part due to using a single-part variable in an assembly
        collapseRecursivePart outer inner
    | _ -> ok node

/// Moving from the bottom of the tree up, flatten nested assemblies and recursive parts.
let flattenAssemblies (pragmaCache: PragmaCache) =
    map Serial BottomUp (flattenAssembly pragmaCache)

// =====================
// determining the pragma environment at any given node; stuffing assemblies
// =====================

///<summary>
/// Keeps track of persistent pragmas, as well as transients that are unused.
/// assignedTransients are cleared at state update, while unassignedTransients are
/// moved to assigned when we update state on a Part node.  This ensures that the first
/// part node we encounter after adding a transient pragma is the only node that sees that
/// pragma in "assigned".
/// We separately track declared capabilities and deactivated warnings.
///</summary>
type PragmaEnvironment =
    { persistent: PragmaCollection
      unassignedTransients: PragmaCollection
      assignedTransients: PragmaCollection
      capabilities: Capabilities
      warnOffs: Set<string> }

let emptyPragmaEnvironment =
    { persistent = PragmaCollection.empty
      unassignedTransients = PragmaCollection.empty
      assignedTransients = PragmaCollection.empty
      capabilities = Set.empty
      warnOffs = Set.empty }

/// Update the pragma environment on both pragmas and part nodes.
/// Ignores unbuilt pragmas.
/// Also operates on blocks, to ensure that block capture transient pragmas.
let updatePragmaEnvironment (mode: StateUpdateMode) (environment: PragmaEnvironment) (node: AstNode): PragmaEnvironment =
    match mode with
    | PreTransform ->
        match node with
        | Pragma pragmaWrapper ->
            let pragma = pragmaWrapper.x
            // handle some special cases
            let isWarning = pragma |> Pragma.isWarning
            let ignoresWarning = pragma |> Pragma.ignoresWarning
            let setsCapability = pragma |> Pragma.setsCapability

            match isWarning, ignoresWarning, setsCapability with
            | true, _, _ -> environment // we print warnings in a lint pass, ignore this
            | _, Some warnOff, _ ->
                { environment with
                      warnOffs = environment.warnOffs |> Set.add warnOff }
            | _, _, Some capa ->
                { environment with
                      capabilities = environment.capabilities.Add(capa) }
            | _ -> // general pragma case
                let isTransient = pragma |> Pragma.isTransient

                if isTransient then
                    { environment with
                          unassignedTransients =
                              environment.unassignedTransients
                              |> PragmaCollection.addPragma pragma }
                else
                    { environment with
                          persistent =
                              environment.persistent
                              |> PragmaCollection.addPragma pragma }
        | Part _
        | L2Expression _ ->
            // replace assignedTransients with unassignedTransients, and empty unassignedTransients
            { environment with
                  unassignedTransients = PragmaCollection.empty
                  assignedTransients = environment.unassignedTransients }
        | _ -> environment
    | PostTransform ->
        match node with
        | Block _ ->
            // blocks "capture" transient pragmas, so we blow away the transients collections
            // after we operate on one.
            { environment with
                  unassignedTransients = PragmaCollection.empty
                  assignedTransients = PragmaCollection.empty }
        | _ -> environment

/// Helper error for pragma collision.
let private collidingPragmaError (existing: Pragma) incoming node =
    let formatPragma p = p.Arguments |> String.concat " "

    let msg =
        sprintf
            "The pragma #%s is set in this assembly as well as in the enclosing environment with conflicting values.  Incoming: '%s'.  Existing: '%s'."
            existing.Name
            (formatPragma incoming)
            (formatPragma existing)

    error PragmaError msg node

/// Check incoming pragmas for collisions with another pragma collection.
let private checkPragmaCollisions (incoming: PragmaCollection)
                                  (existing: PragmaCollection)
                                  (node: AstNode)
                                  : Result<unit, AstMessage> =
    if existing |> PragmaCollection.isEmpty |> not then
        existing
        |> PragmaCollection.values
        |> Seq.map (fun existingPragma ->
            match incoming
                  |> PragmaCollection.tryFindDefinition existingPragma.Definition with
            | Some colliding ->
                if existingPragma.Arguments <> colliding.Arguments then // pragma collision with unequal arguments
                    collidingPragmaError existingPragma colliding node
                else
                    ok () // identical arguments, ignore collision
            | None -> ok ())
        |> collectValidations
    else
        ok ()

/// Deposit collected pragmas into an assembly.
let private stuffPragmasIntoAssembly (pragmaEnvironment: PragmaEnvironment) (node: AstNode): Result<AstNode, AstMessage> =
    match node with
    | AssemblyPart (partWrapper, _) ->
        let incoming =
            pragmaEnvironment.persistent
            |> PragmaCollection.mergeInCollection pragmaEnvironment.assignedTransients

        // get a pragma collection from this assembly
        let assemblyPragmas = getPragmas partWrapper

        checkPragmaCollisions incoming assemblyPragmas node
        >>= (fun _ ->
            // no collisions, free to merge everything in.
            // start with globals, merge in transients, then merge in part pragmas
            let newPragmas =
                incoming
                |> PragmaCollection.mergeInCollection assemblyPragmas
            // if we have warn offs, make a pragma for them and add them
            let pragmasWithWarnOff =
                if not pragmaEnvironment.warnOffs.IsEmpty then
                    let warnOffPragma =
                        { Pragma.Definition = BuiltIn.warnoffPragmaDef
                          Arguments = Set.toList pragmaEnvironment.warnOffs }

                    newPragmas
                    |> PragmaCollection.addPragma warnOffPragma
                else
                    newPragmas

            ok (Part(replacePragmas partWrapper pragmasWithWarnOff)))
    | _ -> ok node

///<summary>
/// Add pragmas into assemblies.
/// We only stuff pragmas into assemblies, not individual parts.
/// This expansion step should *only* occur once we've expanded everything into literals and
/// collapsed subassemblies into a single top-level assembly composed of a list of parts.
/// Explicitly disallow pragma collisions; precedence here should already be taken care of
/// by the pragma environment collection, and we should *never* have a transient pragma on an
/// assembly that conflicts with one coming in from its environment.  This is definitely a potential
/// symptom of a bugged GSL program.
///</summary>
let stuffPragmasIntoAssemblies =
    foldmap Serial TopDown updatePragmaEnvironment emptyPragmaEnvironment stuffPragmasIntoAssembly

// ==================
// gathering and assigning docstrings
// ==================

/// Keep track of accumulated docstrings using a similar assignment system as we use for pragmas.
type DocstringEnvironment =
    { unassigned: string list
      assigned: string list }

let emptyDocstringEnvironment = { unassigned = []; assigned = [] }

/// Accumulate docstrings and assign them to assemblies.
/// This function is only used during conversion to legacy assemblies.
/// We might need to make this a bit more sophisticated to correctly ignore docstrings that
/// are just kind of floating in the document that should be ignored.
let updateDocstringEnvironmentInner s node =
    match node with
    | Docstring (dw) ->
        { s with
              unassigned = dw.x :: s.unassigned }
    | Part _ -> // assign these docs to this node, need to reverse the list
        { s with
              assigned = List.rev s.unassigned
              unassigned = [] }
    | _ -> s

let updateDocstringEnvironment =
    pretransformOnly updateDocstringEnvironmentInner

// ==================
// checking gene naming
// ==================

/// If a node is a part with a gene, validate the name of that gene.
/// Uses the pragmas of the enclosing part and the outer assembly context.
let private checkGeneName (rgs: GenomeDefs) (library: Map<string, Dna>) assemblyPragmas node =
    match node with
    | GenePart (pp, gp) ->
        let geneName = gp.x.gene.[1..].ToUpper()
        let partPragmas = getPragmas pp

        getRGNew rgs [ partPragmas; assemblyPragmas ]
        |> mapMessages (fun s -> errorMessage RefGenomeError s node)
        >>= (fun rg ->
            if rg.IsValid(geneName)
               || library.ContainsKey(geneName) then
                good
            else
                errorf PartError "Unknown gene: '%s'." geneName (pp.x.basePart))
    | _ -> good

/// Check all the gene names in the context of a single assembly.
let private checkGeneNamesInAssembly (rgs: GenomeDefs) library node =
    match node with
    | AssemblyPart (pw, aw) ->
        let assemblyPrags = getPragmas pw

        aw.x
        |> List.map (checkGeneName rgs library assemblyPrags)
        |> collectValidations
    | _ -> good

/// Validate all gene names.
let checkGeneNames rgs library =
    validate (checkGeneNamesInAssembly rgs library)

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
        let newBlockContents = bw.x |> List.choose cleaner
        Block({ bw with x = newBlockContents })
    | _ -> node

/// Strip function defintions from tree.
let stripFunctions =
    map Serial TopDown (promote (cleanBlock cleanFunction))

/// Strip variable bindings from tree.
let stripVariables =
    map Serial TopDown (promote (cleanBlock cleanVariable))

// =======================
// collecting warning messages from pragmas
// =======================

let private collectWarning (node: AstNode): Result<AstNode, AstMessage> =
    match node with
    | Pragma pragma when pragma.x |> Pragma.isWarning ->
        let msg = pragma.x.Arguments |> String.concat " "
        let warnMsg = warningMessage msg node
        warn warnMsg node // add a warning into the message stream
    | _ -> ok node

/// Add warnings into the message stream for every #warn pragma in the tree.
let collectWarnings = map Serial TopDown collectWarning

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
        let pragmas = getPragmas assemblyWrapper

        if pragmas
           |> PragmaCollection.containsPragmaDef BuiltIn.namePragmaDef then node // already named
        else let literal = decompile node |> cleanHashName

             let name =
                 literal
                     .Substring(0, min literal.Length Default.NameMaxLength)
                     .Replace("@", "(@)")

             let namePragma =
                 { Pragma.Definition = BuiltIn.namePragmaDef
                   Arguments = [ name ] }

             let mergedPragmas =
                 pragmas |> PragmaCollection.addPragma namePragma

             let namedAssembly =
                 Part(replacePragmas assemblyWrapper mergedPragmas)

             let pragmaNode = Pragma(nodeWrap namePragma)
             Block(nodeWrapWithNodePosition node [ pragmaNode; namedAssembly ])
    | _ -> node


///<summary>
/// If an assembly does not have a name, generate one and stuff it in.
/// Also prepend a name pragma, accomplished by replacing the assembly with a subblock that includes
/// the new name pragma.
///</summary>
let nameAssemblies =
    map Serial TopDown (promote nameAssembly)


// ====================
// expanding inline roughage
// ====================

// the parser outputs inline roughage sections as blocks for convenience.
// we expand each individual line into block, possibly containing pragmas, and one L2 line.
// we need the pragma context to do this

let private validateRoughageLine (rw: Node<Roughage>) =
    let r = rw.x
    // Rule 1:  must be able to work out the locus.  Locus can be either explicit (ho^) or
    //          implicit pSLN1>YNG1  but can't have just bidirectional promoters with no explicit locus  e.g.   ADH1<pGAL1-pGAL10>ADH2
    let hasLocus =
        r.locus.IsSome
        || (r.parts.Length > 0
            && not r.parts.Head.x.pt2.IsSome)

    let node = Roughage(rw)

    if not hasLocus
    then errorf ValueError "Roughage construct has indeterminate locus: %s" (decompile node) node
    else ok rw

/// Roughage expands to Level 2 GSL.  We actually do this using the AST rather than bootstrapping.
let private expandRoughage (roughageWrapper: Node<Roughage>): AstNode =
    let roughage = roughageWrapper.x
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
        let pt = ptw.x
        let promoter = L2Id(pt.promoter)
        let target = L2Id(pt.target)
        createL2Element promoter target

    // For roughage, if no marker is specified, it defaults to ura3
    let marker =
        match roughage.HasMarker with
        | None -> "ura3"
        | Some (x) -> markerMapping x

    let markerPragma =
        Pragma
            ({ x =
                   { Pragma.Definition = BuiltIn.markersetPragmaDef
                     Arguments = [ marker ] }
               positions = roughageWrapper.positions })

    let l2Elements =
        [ for p in roughage.parts do
            yield l2ElementFromRoughagePair p.x.pt1

            match p.x.pt2 with
            | Some (pt) -> yield l2ElementFromRoughagePair pt
            | None -> () ]

    let l2Locus =
        match roughage.locus with
        | Some (l) -> Some(L2Id(l))
        | None -> None

    let l2Expression = createL2Expression l2Locus l2Elements

    // wrap the marker pragma and the L2 line up in a block
    Block
        ({ x = [ markerPragma; l2Expression ]
           positions = [] })

let private expandRoughageLine node =
    match node with
    | Roughage (rw) ->
        validateRoughageLine rw
        >>= (promote expandRoughage)

    | _ -> ok node

/// Expand all inline roughage definitions into subblocks.
let expandRoughageLines = map Serial TopDown expandRoughageLine
