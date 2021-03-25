namespace GslCore.Ast.Process.AssemblyStuffing

open GslCore.Ast.Process
open GslCore.Ast.Types
open GslCore.Ast.Algorithms
open GslCore.GslResult
open GslCore.Pragma

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
    { Persistent: PragmaCollection
      UnassignedTransients: PragmaCollection
      AssignedTransients: PragmaCollection
      Capabilities: Capabilities
      WarnOffs: Set<string> }

module PragmaEnvironment =
    let empty =
        { Persistent = PragmaCollection.empty
          UnassignedTransients = PragmaCollection.empty
          AssignedTransients = PragmaCollection.empty
          Capabilities = Set.empty
          WarnOffs = Set.empty }

[<RequireQualifiedAccess>]
type PragmaMergeError = PragmaConflict of incoming: Pragma * existing: Pragma

[<RequireQualifiedAccess>]
type AssemblyStuffingError = PragmaMerge of node: AstNode * inner: PragmaMergeError

module AssemblyStuffingError =
    let makePragmaMergeError (node: AstNode) (pragmaCheckError: PragmaMergeError): AssemblyStuffingError =
        AssemblyStuffingError.PragmaMerge(node, pragmaCheckError)

module UpdatePragmaEnvironment =
    let private accumulatePragmas (environment: PragmaEnvironment) (node: AstNode): PragmaEnvironment =
        match node with
        | AstNode.Pragma { Node.Value = pragma; Positions = _ } ->
            let isWarning = pragma |> Pragma.isWarning

            if isWarning then
                environment
            else
                let ignoresWarning = pragma |> Pragma.ignoresWarning

                match ignoresWarning with
                | Some warnOff ->
                    { environment with
                          WarnOffs = environment.WarnOffs |> Set.add warnOff }
                | None ->
                    let setsCapability = pragma |> Pragma.setsCapability

                    match setsCapability with
                    | Some capability ->
                        { environment with
                              Capabilities = environment.Capabilities.Add(capability) }
                    | None ->
                        let isTransient = pragma |> Pragma.isTransient

                        if isTransient then
                            { environment with
                                  UnassignedTransients =
                                      environment.UnassignedTransients
                                      |> PragmaCollection.add pragma }
                        else
                            { environment with
                                  Persistent =
                                      environment.Persistent
                                      |> PragmaCollection.add pragma }

        | AstNode.Part _
        | AstNode.L2Expression _ ->
            // replace assignedTransients with unassignedTransients, and empty unassignedTransients
            { environment with
                  UnassignedTransients = PragmaCollection.empty
                  AssignedTransients = environment.UnassignedTransients }
        | _ -> environment

    let private resetTransientsOnBlock (environment: PragmaEnvironment) (node: AstNode): PragmaEnvironment =
        match node with
        | AstNode.Block _ ->
            // blocks "capture" transient pragmas, so we blow away the transients collections
            // after we operate on one.
            { environment with
                  UnassignedTransients = PragmaCollection.empty
                  AssignedTransients = PragmaCollection.empty }
        | _ -> environment

    /// Update the pragma environment on both pragmas and part nodes.
    /// Ignores unbuilt pragmas.
    /// Also operates on blocks, to ensure that block capture transient pragmas.
    let update (mode: StateUpdateMode) (environment: PragmaEnvironment) (node: AstNode): PragmaEnvironment =
        match mode with
        | PreTransform -> accumulatePragmas environment node
        | PostTransform -> resetTransientsOnBlock environment node

module AssemblyStuffing =

    /// Check incoming pragmas for collisions with another pragma collection.
    let private validatePragmaMerge (incoming: PragmaCollection)
                                    (existing: PragmaCollection)
                                    : GslResult<unit, PragmaMergeError> =
        let existingEmpty = existing |> PragmaCollection.isEmpty

        if existingEmpty then
            GslResult.ok ()
        else
            [ for existingPragma in existing |> PragmaCollection.values do
                let maybeColliding =
                    incoming
                    |> PragmaCollection.tryFind existingPragma.Definition

                match maybeColliding with
                | Some colliding ->
                    if existingPragma.Arguments <> colliding.Arguments then // pragma collision with unequal arguments
                        GslResult.err (PragmaMergeError.PragmaConflict(existingPragma, colliding))
                    else
                        ()
                | None -> () ]
            |> GslResult.collectA
            |> GslResult.ignore

    let private mergePragmas (pragmaEnvironment: PragmaEnvironment) (first: PragmaCollection) (second: PragmaCollection) =
        let newPragmas =
            first |> PragmaCollection.mergeInCollection second

        // if we have warn offs, make a pragma for them and add them
        let hasWarnOffs =
            pragmaEnvironment.WarnOffs |> Set.isEmpty |> not

        if hasWarnOffs then
            let warnOffPragma =
                { Pragma.Definition = BuiltIn.warnoffPragmaDef
                  Arguments = pragmaEnvironment.WarnOffs |> Set.toList }

            newPragmas |> PragmaCollection.add warnOffPragma
        else
            newPragmas

    /// Deposit collected pragmas into an assembly.
    let private stuffPragmasIntoAssembly (validateMerge: PragmaCollection -> PragmaCollection -> GslResult<unit, PragmaMergeError>)
                                         (pragmaEnvironment: PragmaEnvironment)
                                         (node: AstNode)
                                         : GslResult<AstNode, AssemblyStuffingError> =
        match node with
        | AssemblyPart (partWrapper, _) ->
            let incoming =
                pragmaEnvironment.Persistent
                |> PragmaCollection.mergeInCollection pragmaEnvironment.AssignedTransients

            // get a pragma collection from this assembly
            let assemblyPragmas = ParsePart.getPragmas partWrapper

            validateMerge incoming assemblyPragmas
            |> GslResult.mapError (AssemblyStuffingError.makePragmaMergeError node)
            |> GslResult.map (fun _ ->
                let newPragmas =
                    mergePragmas pragmaEnvironment incoming assemblyPragmas

                let result =
                    ParsePart.replacePragmas partWrapper newPragmas

                AstNode.Part result)
        | _ -> GslResult.ok node

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
        let foldMapParameters =
            { FoldMapParameters.Direction = TopDown
              Mode = Serial
              StateUpdate = UpdatePragmaEnvironment.update
              Map = stuffPragmasIntoAssembly validatePragmaMerge }

        FoldMap.foldMap PragmaEnvironment.empty foldMapParameters
