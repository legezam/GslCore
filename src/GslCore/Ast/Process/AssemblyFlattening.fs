namespace GslCore.Ast.Process.AssemblyFlattening


open GslCore.Ast.Process.ParsePart
open GslCore.Ast.Types
open GslCore.Ast.Algorithms
open GslCore.GslResult
open GslCore.Pragma


// ==========================
// collapsing nested assemblies into a single top-level part
// ==========================

[<RequireQualifiedAccess>]
type ShiftFuseError =
    | PragmaArgument of PragmaArgumentError
    | GetPragma of GetPragmaError


[<RequireQualifiedAccess>]
type RecursivePartCollapseError =
    | GetPragma of GetPragmaError
    | MergePragma of MergePragmaError

[<RequireQualifiedAccess>]
type InvertPragmaError =
    | Inversion of PragmaInversionError
    | GetPragma of GetPragmaError

[<RequireQualifiedAccess>]
type AssemblyExplosionError =
    | InvertPragma of InvertPragmaError
    | FlippingTrailingFuse of part: Node<ParsePart>
    | ShiftFuse of ShiftFuseError
    | GetPragma of GetPragmaError
    | MergePragma of MergePragmaError

[<RequireQualifiedAccess>]
type AssemblyFlatteningError =
    | Explosion of AssemblyExplosionError * node: AstNode
    | RecursivePartCollapse of RecursivePartCollapseError

module AssemblyFlatteningError =
    let makeExplosionError (node: AstNode) (error: AssemblyExplosionError) =
        AssemblyFlatteningError.Explosion(error, node)

module AssemblyFlattening =

    /// Unpack the subparts of the assembly, ignoring anything out of place.
    let private unpackParts (nodes: AstNode list): Node<ParsePart> list =
        nodes
        |> List.choose (fun node ->
            match node with
            | AstNode.Part partWrapper -> Some partWrapper
            | _ -> None)

    let private shiftOne (shiftedParts: Node<ParsePart> list, addFuse: bool)
                         (part: Node<ParsePart>)
                         : GslResult<Node<ParsePart> list * bool, ShiftFuseError> =

        let fusablePragma =
            { Pragma.Definition = BuiltIn.fusePragmaDef
              Arguments = [] }

        ParsePart.getPragmasStrict part
        |> GslResult.mapError ShiftFuseError.GetPragma
        >>= fun pragmas ->
                // if this part has a #fuse, we need to add one to the next part
                let nextNeedsFuse =
                    pragmas
                    |> PragmaCollection.containsPragma fusablePragma

                let newPragmasResult =
                    if addFuse then
                        pragmas |> PragmaCollection.add fusablePragma
                    else
                        pragmas
                        |> PragmaCollection.removePragma fusablePragma
                        |> GslResult.ok

                newPragmasResult
                |> GslResult.mapError ShiftFuseError.PragmaArgument
                |> GslResult.map (fun newPragmas ->
                    let newPart = ParsePart.replacePragmas part newPragmas
                    (newPart :: shiftedParts, nextNeedsFuse))

    ///<summary>
    /// Shift any fuse pragmas one slot to the right.
    /// Since #fuse directs the compiler to fuse the current part to the part immediately following,
    /// and we are about to invert the list, this ensures that pairs of parts still correctly associate
    /// around the #fuse pragma.
    /// If the assembly has a trailing #fuse, fail.
    /// Because the internal fold naturally reverses the list, don't un-reverse it because we need
    /// to do this anyway.
    ///</summary>
    let private shiftFusePragmaAndReverseList (parts: Node<ParsePart> list)
                                              : GslResult<Node<ParsePart> list, AssemblyExplosionError> =


        parts
        |> GslResult.foldM shiftOne ([], false)
        |> GslResult.mapError AssemblyExplosionError.ShiftFuse
        >>= fun (shiftedParts, trailingFuse) ->
                if trailingFuse then
                    GslResult.err (AssemblyExplosionError.FlippingTrailingFuse(List.head shiftedParts))
                else
                    GslResult.ok shiftedParts

    /// Replace any pragmas that invert upon reversal with their inverted version.
    let private invertPragma (pragmaBuilder: PragmaFactory)
                             (part: Node<ParsePart>)
                             : GslResult<Node<ParsePart>, InvertPragmaError> =
        part
        |> ParsePart.getPragmasStrict
        |> GslResult.mapError InvertPragmaError.GetPragma
        >>= fun retrievedPragmas ->
                retrievedPragmas
                |> PragmaCollection.values
                |> Seq.map (fun pragma ->
                    pragmaBuilder
                    |> PragmaFactory.inverts pragma.Definition
                    |> GslResult.map
                        (Option.map (fun invertsTo -> { pragma with Definition = invertsTo })
                         >> Option.defaultValue pragma))
                |> Seq.toList
                |> GslResult.collectA
                |> GslResult.mapError InvertPragmaError.Inversion
                |> GslResult.map
                    (PragmaCollection.create
                     >> ParsePart.replacePragmas part)

    ///<summary>
    /// Explode an assembly into a flat list of parts.
    /// This function encodes the logic that used to reside in MULTIPART expansion.
    /// This function ignores various unexpected conditions, like nodes besides parts inside the assembly.
    ///</summary>
    let private explodeAssembly (pragmaBuilder: PragmaFactory)
                                (assemblyPart: Node<ParsePart>)
                                (assemblyBasePart: Node<AstNode list>)
                                : GslResult<AstNode list, AssemblyExplosionError> =
        // This operation is trivial if the assembly is in the forward orientation.
        // If it needs to reverse, it is rather tedious.
        let subParts = unpackParts assemblyBasePart.Value

        let correctlyOrientedParts =
            if assemblyPart.Value.IsForward then
                GslResult.ok subParts
            else
                subParts
                |> List.map (fun subPart ->
                    { subPart with
                          Value =
                              { subPart.Value with
                                    IsForward = not subPart.Value.IsForward } }) // flip the part
                |> List.map
                    (invertPragma pragmaBuilder
                     >> GslResult.mapError AssemblyExplosionError.InvertPragma) // flip the pragmas
                |> GslResult.collectA
                >>= shiftFusePragmaAndReverseList // shift fuse pragmas one flip to the right, reversing the list
        // now that the parts are correctly oriented, stuff the assembly pragmas into them
        correctlyOrientedParts
        >>= (List.map (fun parsePart ->
                 ParsePart.getPragmasStrict assemblyPart
                 |> GslResult.mapError AssemblyExplosionError.GetPragma
                 >>= (ParsePart.mergePragmas parsePart
                      >> GslResult.mapError AssemblyExplosionError.MergePragma))
             >> GslResult.collectA
             >> GslResult.map (List.map AstNode.Part))

    /// Collapse a part whose base part is another part.
    // TODO: we should probably be more careful with mods here
    let private collapseRecursivePart (outerPart: Node<ParsePart>)
                                      (innerPart: Node<ParsePart>)
                                      : GslResult<AstNode, RecursivePartCollapseError> =
        ParsePart.getPragmasStrict outerPart
        |> GslResult.mapError RecursivePartCollapseError.GetPragma
        >>= fun outerPragmas ->
                ParsePart.mergePragmas innerPart outerPragmas
                |> GslResult.mapError RecursivePartCollapseError.MergePragma
                |> GslResult.map (fun newInner ->
                    let joinedMods =
                        innerPart.Value.Modifiers
                        @ outerPart.Value.Modifiers

                    let newDir =
                        not
                            (innerPart.Value.IsForward
                             <> outerPart.Value.IsForward) // should be rev if one or the other is rev.

                    let newInnerWithOuterMods =
                        { newInner with
                              Value =
                                  { newInner.Value with
                                        Modifiers = joinedMods
                                        IsForward = newDir } }

                    AstNode.Part newInnerWithOuterMods)

    /// Explode any nested assemblies up into the list of parts in the parent assembly.
    // TODO: need to handle mods, and allow only if contents of assembly is a single gene part.
    // should use an active pattern to match.
    // TODO: we should probably check for pragma collisions and complain about them, though this is
    // before stuffing pragmas into assemblies so it may be an edge case.
    let private flattenAssembly (parameters: Phase1Parameters)
                                (node: AstNode)
                                : GslResult<AstNode, AssemblyFlatteningError> =
        match node with
        | AssemblyPart (assemblyPart, assemblyMemberParts) ->
            // iterate over the parts in the assembly, accumulating lists of parts we will concatenate
            assemblyMemberParts.Value
            |> List.map (fun part ->
                match part with
                | AssemblyPart (assemblyPart, assemblyBasePart) ->
                    explodeAssembly parameters.PragmaBuilder assemblyPart assemblyBasePart
                | nonAssemblyPart -> GslResult.ok [ nonAssemblyPart ])
            |> GslResult.collectA
            |> GslResult.mapError (AssemblyFlatteningError.makeExplosionError node)
            |> GslResult.map (fun partLists ->
                let newBasePart =
                    AstNode.Assembly
                        { assemblyMemberParts with
                              Value = List.concat partLists }

                AstNode.Part
                    { assemblyPart with
                          Value =
                              { assemblyPart.Value with
                                    BasePart = newBasePart } })
        | RecursivePart (outer, inner) ->
            // flatten parts that have another part as their base part due to using a single-part variable in an assembly
            collapseRecursivePart outer inner
            |> GslResult.mapError AssemblyFlatteningError.RecursivePartCollapse
        | _ -> GslResult.ok node

    /// Moving from the bottom of the tree up, flatten nested assemblies and recursive parts.
    let flattenAssemblies (parameters: Phase1Parameters): AstTreeHead -> GslResult<AstTreeHead, AssemblyFlatteningError> =
        FoldMap.map Serial BottomUp (flattenAssembly parameters)
