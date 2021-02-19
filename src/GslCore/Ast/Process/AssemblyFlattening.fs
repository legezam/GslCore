module GslCore.Ast.Process.AssemblyFlattening


open Amyris.ErrorHandling
open GslCore.Ast.Process
open GslCore.Ast.Types
open GslCore.Ast.ErrorHandling
open GslCore.Ast.Algorithms
open GslCore.Pragma


// ==========================
// collapsing nested assemblies into a single top-level part
// ==========================

/// Unpack the subparts of the assembly, ignoring anything out of place.
let private unpackParts (nodes: AstNode list): Node<ParsePart> list =
    nodes
    |> List.choose (fun node ->
        match node with
        | Part partWrapper -> Some partWrapper
        | _ -> None)

let private shiftOne (shiftedParts: Node<ParsePart> list, addFuse: bool)
                     (part: Node<ParsePart>)
                     : Node<ParsePart> list * bool =
    let fusablePragma =
        { Pragma.Definition = BuiltIn.fusePragmaDef
          Arguments = [] }

    let pragmas = ParsePart.getPragmas part
    // if this part has a #fuse, we need to add one to the next part
    let nextNeedsFuse =
        pragmas
        |> PragmaCollection.containsPragma fusablePragma

    let newPart =
        let newPragmas =
            if addFuse then
                pragmas |> PragmaCollection.add fusablePragma
            else
                pragmas
                |> PragmaCollection.removePragma fusablePragma

        ParsePart.replacePragmas part newPragmas

    (newPart :: shiftedParts, nextNeedsFuse)

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
    let shiftedParts, trailingFuse = parts |> List.fold shiftOne ([], false)

    if trailingFuse then
        AstMessage.createError
            PragmaError
            "Found a trailing #fuse in an assembly that needs to flip."
            (Part(List.head shiftedParts))
    else
        ok shiftedParts

/// Replace any pragmas that invert upon reversal with their inverted version.
let private invertPragma (pragmaCache: PragmaBuilder) (part: Node<ParsePart>): Node<ParsePart> =
    part
    |> ParsePart.getPragmas
    |> PragmaCollection.values
    |> Seq.map (fun pragma ->
        pragmaCache
        |> PragmaBuilder.inverts pragma.Definition
        |> Option.map (fun invertsTo -> { pragma with Definition = invertsTo })
        |> Option.defaultValue pragma)
    |> PragmaCollection.create
    |> ParsePart.replacePragmas part

///<summary>
/// Explode an assembly into a flat list of parts.
/// This function encodes the logic that used to reside in MULTIPART expansion.
/// This function ignores various unexpected conditions, like nodes besides parts inside the assembly.
///</summary>
let private explodeAssembly (pragmaCache: PragmaBuilder)
                            (assemblyPart: Node<ParsePart>)
                            (assemblyBasePart: Node<AstNode list>)
                            : Result<AstNode list, AstMessage> =
    // This operation is trivial if the assembly is in the forward orientation.
    // If it needs to reverse, it is rather tedious.
    let subParts = unpackParts assemblyBasePart.Value

    let correctlyOrientedParts =
        if assemblyPart.Value.IsForward then
            ok subParts
        else
            subParts
            |> List.map (fun subPart ->
                { subPart with
                      Value =
                          { subPart.Value with
                                IsForward = not subPart.Value.IsForward } }) // flip the part
            |> List.map (invertPragma pragmaCache) // flip the pragmas
            |> shiftFusePragmaAndReverseList // shift fuse pragmas one flip to the right, reversing the list
    // now that the parts are correctly oriented, stuff the assembly pragmas into them
    correctlyOrientedParts
    >>= (fun parts ->
        parts
        |> List.map (fun p -> ParsePart.mergePragmas p (ParsePart.getPragmas assemblyPart))
        |> collect
        |> lift (List.map (fun (p: Node<ParsePart>) -> Part(p))))

/// Collapse a part whose base part is another part.
// FIXME: we should probably be more careful with mods here
let private collapseRecursivePart (outerPart: Node<ParsePart>) (innerPart: Node<ParsePart>): Result<AstNode, AstMessage> =
    let outerPragmas = ParsePart.getPragmas outerPart

    let joinedMods =
        innerPart.Value.Modifiers
        @ outerPart.Value.Modifiers

    let newDir =
        not
            (innerPart.Value.IsForward
             <> outerPart.Value.IsForward) // should be rev if one or the other is rev.

    ParsePart.mergePragmas innerPart outerPragmas
    >>= (fun newInner ->
        let newInnerWithOuterMods =
            { newInner with
                  Value =
                      { newInner.Value with
                            Modifiers = joinedMods
                            IsForward = newDir } }

        ok (Part(newInnerWithOuterMods)))

/// Explode any nested assemblies up into the list of parts in the parent assembly.
// TODO: need to handle mods, and allow only if contents of assembly is a single gene part.
// should use an active pattern to match.
// TODO: we should probably check for pragma collisions and complain about them, though this is
// before stuffing pragmas into assemblies so it may be an edge case.
let private flattenAssembly (pragmaCache: PragmaBuilder) (node: AstNode): Result<AstNode, AstMessage> =
    match node with
    | AssemblyPart (assemblyPart, assemblyBasePart) ->
        // iterate over the parts in the assembly, accumulating lists of parts we will concatenate
        assemblyBasePart.Value
        |> Seq.map (fun part ->
            match part with
            | AssemblyPart (assemblyPart, assemblyBasePart) -> explodeAssembly pragmaCache assemblyPart assemblyBasePart
            | x -> ok [ x ])
        |> collect
        |> lift (fun partLists ->
            let newBasePart =
                Assembly
                    ({ assemblyBasePart with
                           Value = List.concat partLists })

            Part
                ({ assemblyPart with
                       Value =
                           { assemblyPart.Value with
                                 BasePart = newBasePart } }))
    | RecursivePart (outer, inner) ->
        // flatten parts that have another part as their base part due to using a single-part variable in an assembly
        collapseRecursivePart outer inner
    | _ -> ok node

/// Moving from the bottom of the tree up, flatten nested assemblies and recursive parts.
let flattenAssemblies (pragmaCache: PragmaBuilder): AstTreeHead -> TreeTransformResult =
    FoldMap.map Serial BottomUp (flattenAssembly pragmaCache)