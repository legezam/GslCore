namespace GslCore.Ast.Process

open Amyris.ErrorHandling
open GslCore.Ast.Process
open GslCore.AstTypes
open GslCore.AstErrorHandling
open GslCore.AstAlgorithms
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

module AssemblyStuffing =
    /// Update the pragma environment on both pragmas and part nodes.
    /// Ignores unbuilt pragmas.
    /// Also operates on blocks, to ensure that block capture transient pragmas.
    let updatePragmaEnvironment (mode: StateUpdateMode)
                                (environment: PragmaEnvironment)
                                (node: AstNode)
                                : PragmaEnvironment =
        match mode with
        | PreTransform ->
            match node with
            | Pragma pragmaWrapper ->
                let pragma = pragmaWrapper.Value
                // handle some special cases
                let isWarning = pragma |> Pragma.isWarning
                let ignoresWarning = pragma |> Pragma.ignoresWarning
                let setsCapability = pragma |> Pragma.setsCapability

                match isWarning, ignoresWarning, setsCapability with
                | true, _, _ -> environment // we print warnings in a lint pass, ignore this
                | _, Some warnOff, _ ->
                    { environment with
                          WarnOffs = environment.WarnOffs |> Set.add warnOff }
                | _, _, Some capa ->
                    { environment with
                          Capabilities = environment.Capabilities.Add(capa) }
                | _ -> // general pragma case
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
            | Part _
            | L2Expression _ ->
                // replace assignedTransients with unassignedTransients, and empty unassignedTransients
                { environment with
                      UnassignedTransients = PragmaCollection.empty
                      AssignedTransients = environment.UnassignedTransients }
            | _ -> environment
        | PostTransform ->
            match node with
            | Block _ ->
                // blocks "capture" transient pragmas, so we blow away the transients collections
                // after we operate on one.
                { environment with
                      UnassignedTransients = PragmaCollection.empty
                      AssignedTransients = PragmaCollection.empty }
            | _ -> environment

    /// Helper error for pragma collision.
    let private collidingPragmaError (existing: Pragma) (incoming: Pragma) (node: AstNode): Result<unit, AstMessage> =
        let formatPragma pragma = pragma.Arguments |> String.concat " "

        let msg =
            sprintf
                "The pragma #%s is set in this assembly as well as in the enclosing environment with conflicting values.  Incoming: '%s'.  Existing: '%s'."
                existing.Name
                (formatPragma incoming)
                (formatPragma existing)

        AstMessage.createError PragmaError msg node

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
                      |> PragmaCollection.tryFind existingPragma.Definition with
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
    let private stuffPragmasIntoAssembly (pragmaEnvironment: PragmaEnvironment)
                                         (node: AstNode)
                                         : Result<AstNode, AstMessage> =
        match node with
        | AssemblyPart (partWrapper, _) ->
            let incoming =
                pragmaEnvironment.Persistent
                |> PragmaCollection.mergeInCollection pragmaEnvironment.AssignedTransients

            // get a pragma collection from this assembly
            let assemblyPragmas = ParsePart.getPragmas partWrapper

            checkPragmaCollisions incoming assemblyPragmas node
            >>= fun _ ->
                // no collisions, free to merge everything in.
                // start with globals, merge in transients, then merge in part pragmas
                let newPragmas =
                    incoming
                    |> PragmaCollection.mergeInCollection assemblyPragmas
                // if we have warn offs, make a pragma for them and add them
                let pragmasWithWarnOff =
                    if not pragmaEnvironment.WarnOffs.IsEmpty then
                        let warnOffPragma =
                            { Pragma.Definition = BuiltIn.warnoffPragmaDef
                              Arguments = Set.toList pragmaEnvironment.WarnOffs }

                        newPragmas |> PragmaCollection.add warnOffPragma
                    else
                        newPragmas

                ok (Part(ParsePart.replacePragmas partWrapper pragmasWithWarnOff))
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
        let foldMapParameters =
            { FoldMapParameters.Direction = TopDown
              Mode = Serial
              StateUpdate = updatePragmaEnvironment
              Map = stuffPragmasIntoAssembly }

        FoldMap.foldMap PragmaEnvironment.empty foldMapParameters
