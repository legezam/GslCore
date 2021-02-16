namespace GslCore.Pragma

open System
open Amyris.ErrorHandling
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
    { Pragmas: Map<string, Pragma>
      Cache: PragmaCache }

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
    let addPragma (pragma: Pragma) (this: PragmaCollection): PragmaCollection =
        let pragmas =
            (match pragma.Definition.Scope with
             | BlockOnly (PersistentCumulative)
             | BlockOrPart (PersistentCumulative)
             | BlockOnly (TransientCumulative)
             | BlockOrPart (TransientCumulative) ->
                 match this.Pragmas.TryFind(pragma.Name) with
                 | None -> this.Pragmas.Add(pragma.Name, pragma)
                 | Some (existing) ->
                     let newArgs = existing.Arguments @ pragma.Arguments // new args go on the end

                     match existing.Definition
                           |> Pragma.fromDefinition newArgs with
                     | Ok (newPragma, _messages) -> this.Pragmas.Add(pragma.Name, newPragma)
                     | Bad messages -> failwithf "%s" (String.Join(";", messages))
             | _ -> this.Pragmas.Add(pragma.Name, pragma))

        { this with Pragmas = pragmas }

    /// Add a pragma to this collection using string name and values.
    let addNameValues (name: string, values: string list) (this: PragmaCollection): Result<PragmaCollection, string> =
        this.Cache
        |> Pragma.fromNameValue name values
        >>= fun pragma -> this |> addPragma pragma |> ok

    /// Add a pragma to this collection using string name and single value.
    let addNameValue (name: string, value: string) (this: PragmaCollection): Result<PragmaCollection, string> =
        this |> addNameValues (name, [ value ])

    /// Add a pragma to this collection using string name.
    let addName (name: string) (this: PragmaCollection): Result<PragmaCollection, string> =
        this |> addNameValues (name, [])

    /// Remove a pragma from this collection.
    let removeName (name: string) (this: PragmaCollection): PragmaCollection =
        { this with
              Pragmas = this.Pragmas |> Map.remove name }
    /// Remove a pragma from this collection.
    let removeDefinition (definition: PragmaDefinition) (this: PragmaCollection): PragmaCollection =
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
    /// Raises an exception if pName is not a registered pragma.
    let containsName (name: string) (this: PragmaCollection): bool =
        this.Cache |> PragmaCache.validatePragmaName name
        this.Pragmas |> Map.containsKey name
    /// Has a pragma been set?
    let containsPragmaDef (definition: PragmaDefinition) (this: PragmaCollection): bool =
        this.Pragmas |> Map.containsKey definition.Name
    /// Has a pragma been set?
    let containsPragma (pragma: Pragma) (this: PragmaCollection) =
        this.Pragmas |> Map.containsKey (pragma.Name)

    /// Get the values associated with a pragma.
    /// Raises an exception is pName is not a registered pragma.
    let tryGetValues (name: string) (this: PragmaCollection): string list option =
        this.Cache |> PragmaCache.validatePragmaName name

        this.Pragmas
        |> Map.tryFind name
        |> Option.map (fun pragma -> pragma.Arguments)

    /// Get a single value associated with a pragma, ignoring any extras.
    /// Raises an exception is pName is not a registered pragma.
    let tryGetValue (name: string) (this: PragmaCollection): string option =
        this.Cache |> PragmaCache.validatePragmaName name

        this
        |> tryGetValues name
        |> Option.bind List.tryHead

    /// Get a pragma.
    /// Raises an exception is pName is not a registered pragma.
    let tryFindName (name: string) (this: PragmaCollection): Pragma option =
        this.Cache |> PragmaCache.validatePragmaName name
        this.Pragmas.TryFind name

    /// Get a pragma by definition.
    let tryFindDefinition (definition: PragmaDefinition) (this: PragmaCollection) =
        this.Pragmas |> Map.tryFind definition.Name

    let names (this: PragmaCollection): Set<string> =
        this.Pragmas
        |> Map.toSeq
        |> Seq.map fst
        |> Set.ofSeq

    let values (this: PragmaCollection): Pragma seq = this.Pragmas |> Map.toSeq |> Seq.map snd
    let isEmpty (this: PragmaCollection): bool = this.Pragmas.IsEmpty


    let create (pragmas: seq<Pragma>) (cache: PragmaCache) =
        let pragmas =
            pragmas
            |> Seq.map (fun pragma -> pragma.Name, pragma)
            |> Map.ofSeq

        { PragmaCollection.Cache = cache
          Pragmas = pragmas }


    let empty =
        { PragmaCollection.Pragmas = Map.empty
          Cache = PragmaCache.builtin }

    /// Determine the current assembly mode from pragma collection.
    let assemblyMode (collection: PragmaCollection): Platform =
        let maybePlatformDefinition =
            collection
            |> tryFindName BuiltIn.platformPragmaDef.Name

        match maybePlatformDefinition with
        | Some platformPragma ->
            Platform.parsePlatform platformPragma.Arguments
            |> returnOrFail
        | None -> Megastitch

