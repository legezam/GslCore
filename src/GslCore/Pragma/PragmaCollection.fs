namespace GslCore.Pragma

open FsToolkit.ErrorHandling
open GslCore.GslResult
open GslCore.Pragma

// ===========================
// PragmaCollection domain type
// ===========================

///<summary>
/// A PragmaCollection is a mapping between pragma name and the actual value
/// set for that pragma.  This is the main data structure in which pragmas
/// are passed around.  It is a helpful and safe wrapping of an immutable map.
/// It should be impossible to add invalid pragmas to this structure without
/// doing it manually through the underlying map.</summary>
type PragmaCollection =
    { Pragmas: Map<string, Pragma> }

    /// Pretty-print a collection of pragmas.
    override x.ToString() =
        let ordered =
            x.Pragmas
            |> Map.toList
            |> List.sortBy fst
            |> List.map snd

        let entries =
            String.concat " " (ordered |> Seq.map (fun p -> p.ToString()))

        sprintf "PragmaCollection: %s" entries

module PragmaCollection =
    /// Add a Pragma to this collection.
    let add (pragma: Pragma) (this: PragmaCollection): GslResult<PragmaCollection, PragmaArgumentError> =
        let pragmasResult =
            match pragma.Definition.Scope with
            | BlockOnly PersistentCumulative
            | BlockOnly TransientCumulative
            | BlockOrPart PersistentCumulative
            | BlockOrPart TransientCumulative ->
                match this.Pragmas |> Map.tryFind pragma.Name with
                | None ->
                    this.Pragmas
                    |> Map.add pragma.Name pragma
                    |> GslResult.ok
                | Some existing ->

                    let newArgs = existing.Arguments @ pragma.Arguments

                    existing.Definition
                    |> Pragma.fromDefinition newArgs
                    |> GslResult.map (fun newPragma -> this.Pragmas |> Map.add pragma.Name newPragma)

            | BlockOnly Persistent
            | BlockOnly Transient
            | BlockOrPart Persistent
            | BlockOrPart Transient
            | PartOnly ->
                this.Pragmas
                |> Map.add pragma.Name pragma
                |> GslResult.ok

        pragmasResult
        |> GslResult.map (fun pragmas -> { this with Pragmas = pragmas })

    /// Remove a pragma from this collection.
    let removeName (name: string) (this: PragmaCollection): PragmaCollection =
        { this with
              Pragmas = this.Pragmas |> Map.remove name }
    /// Remove a pragma from this collection.
    let remove (definition: PragmaDefinition) (this: PragmaCollection): PragmaCollection =
        this |> removeName definition.Name

    /// Remove a pragma from this collection.
    let removePragma (pragma: Pragma) (this: PragmaCollection): PragmaCollection = this |> removeName pragma.Name

    /// Merge a list of Pragmas into this collection.
    /// The incoming pragmas will clobber any pragmas set in this collection.
    let mergeInPragmas (incoming: Pragma list) (this: PragmaCollection): PragmaCollection =
        let newPragmas =
            incoming
            |> List.fold (fun (collection: Map<string, Pragma>) (pragma: Pragma) ->
                collection |> Map.add pragma.Name pragma) this.Pragmas

        { this with Pragmas = newPragmas }
    /// Merge another PragmaCollection into this one.
    /// The incoming pragmas will clobber any pragmas set in this collection.
    let mergeInCollection (incoming: PragmaCollection) (this: PragmaCollection): PragmaCollection =
        let newPragmas =
            incoming.Pragmas
            |> Map.fold (fun (collection: Map<string, Pragma>) (name: string) (pragma: Pragma) ->
                collection |> Map.add name pragma) this.Pragmas

        { this with Pragmas = newPragmas }

    /// Has a pragma been set?
    let contains (definition: PragmaDefinition) (this: PragmaCollection): bool =
        this.Pragmas |> Map.containsKey definition.Name
    /// Has a pragma been set?
    let containsPragma (pragma: Pragma) (this: PragmaCollection) =
        this.Pragmas |> Map.containsKey (pragma.Name)

    /// Get the values associated with a pragma.
    /// Raises an exception is pName is not a registered pragma.
    let tryGetValues (definition: PragmaDefinition) (this: PragmaCollection): string list option =

        this.Pragmas
        |> Map.tryFind definition.Name
        |> Option.map (fun pragma -> pragma.Arguments)

    /// Get a single value associated with a pragma, ignoring any extras.
    /// Raises an exception is pName is not a registered pragma.
    let tryGetValue (definition: PragmaDefinition) (this: PragmaCollection): string option =
        this
        |> tryGetValues definition
        |> Option.bind List.tryHead

    /// Get a pragma by definition.
    let tryFind (definition: PragmaDefinition) (this: PragmaCollection) =
        this.Pragmas |> Map.tryFind definition.Name

    let names (this: PragmaCollection): Set<string> =
        this.Pragmas
        |> Map.toSeq
        |> Seq.map fst
        |> Set.ofSeq

    let values (this: PragmaCollection): Pragma seq = this.Pragmas |> Map.toSeq |> Seq.map snd
    let isEmpty (this: PragmaCollection): bool = this.Pragmas.IsEmpty


    let create (pragmas: seq<Pragma>) =
        let pragmas =
            pragmas
            |> Seq.map (fun pragma -> pragma.Name, pragma)
            |> Map.ofSeq

        { Pragmas = pragmas }


    let empty = { PragmaCollection.Pragmas = Map.empty }

    /// Determine the current assembly mode from pragma collection.
    let assemblyMode (collection: PragmaCollection): Platform =
        let maybePlatformDefinition =
            collection |> tryFind BuiltIn.platformPragmaDef

        match maybePlatformDefinition with
        | Some platformPragma ->
            Platform.parsePlatform platformPragma.Arguments
            |> GslResult.valueOr (fun messages -> messages |> String.concat ";" |> failwith)
        | None -> Megastitch
