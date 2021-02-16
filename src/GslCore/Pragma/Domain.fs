namespace GslCore.Pragma.Domain

open System
open Amyris.ErrorHandling
open GslCore.Pragma

module PragmaDefinition =

    /// Format a pragma definition.
    let format (p: PragmaDefinition): string =
        let argDescFormat v = sprintf "<a%d>" v

        let makeArgDesc (n: int) =
            [ 0 .. n - 1 ]
            |> Seq.map argDescFormat
            |> String.concat " "

        let argDesc =
            match p.Shape with
            | Zero -> ""
            | One -> makeArgDesc 1
            | Exactly (n) -> makeArgDesc n
            | AtLeast (n) -> (makeArgDesc n) + " ..."
            | Range (n, m) ->
                (makeArgDesc n)
                + " (..."
                + (argDescFormat (m - 1))
                + ")"
            | ExactlySet (v) -> sprintf " <arg shapes: %A>" v

        let firstLine = sprintf "#%s %s" p.Name argDesc

        let descLines =
            p.Description.Split [| '\n' |]
            |> List.ofArray
            |> List.map (fun d -> "    " + d)

        let scopeLine =
            sprintf "    Scoping: %s" p.Scope.ToString

        (firstLine :: scopeLine :: descLines)
        |> String.concat "\n"

module PragmaCache =
    /// Check that a pragma inverts to a legal pragma.  Returns the pragma it inverts to or raises an exception.
    let inverts (pragmaDefinition: PragmaDefinition) (cache: PragmaCache): PragmaDefinition option =
        match pragmaDefinition.InvertsTo with
        | None -> None
        | Some name ->
            match cache.Pragmas.TryFind name with
            | None -> failwithf "Pragma %s inverts to an unknown pragma %s" (pragmaDefinition.Name) name
            | Some result -> // inverts to a known pragma, make sure they have the same shape
                if pragmaDefinition.Shape <> result.Shape then
                    failwithf
                        "Pragma %s inverts to %s but they have differing argShapes."
                        (pragmaDefinition.Name)
                        (result.Name)

                Some result

    let create (pragmas: PragmaDefinition list): PragmaCache =
        let pragmaDefinitions =
            pragmas
            |> List.distinctBy LanguagePrimitives.PhysicalHash

        let pragsByName =
            pragmaDefinitions
            |> List.map (fun pragma -> pragma.Name, pragma)
            |> Map.ofList

        // Idiot check that we don't have any duplicate pragmas.
        if pragsByName.Count <> pragmaDefinitions.Length then
            failwithf
                "%d pragmas were defined but size of legalPragmas map is only %d. Name aliases?"
                (pragmaDefinitions.Length)
                (pragsByName.Count)

        let cache = PragmaCache(pragsByName)
        // Make sure any pragmas that invert do it sensibly.
        // Raises an exception if any one doesn't validate.
        for pragmaDefinition in pragmaDefinitions do
            cache |> inverts pragmaDefinition |> ignore

        cache

    let createWithBuiltinPragmas (pragmas: PragmaDefinition list) = pragmas @ BuiltIn.all |> create

    let builtin = BuiltIn.all |> create

    /// Print all available pragmas.
    let pragmaUsage (cache: PragmaCache) =
        let orderedPragmas =
            cache.Pragmas
            |> Map.toList
            |> List.sortBy fst // sort the pairs by name
            |> List.map snd // pull out just the sorted pragmas

        for p in orderedPragmas do
            printfn "%s" (PragmaDefinition.format p)

    /// Raise an exception if pName is not among the registered pragmas.
    let validatePragmaName (pragmaName: string) (cache: PragmaCache): unit =
        if cache.Pragmas |> Map.containsKey pragmaName |> not
        then failwithf "Requested unknown pragma '#%s'." pragmaName

module Pragma =
    let getName (this: Pragma): string = this.Definition.Name

    let isTransient (this: Pragma): bool =
        match this.Definition.Scope with
        | BlockOnly (Persistent)
        | BlockOrPart (Persistent)
        | BlockOnly (PersistentCumulative)
        | BlockOrPart (PersistentCumulative) -> false
        | _ -> true
    /// Does this pragma announce the availability of an extension capability?
    let setsCapability (this: Pragma): string option =
        if this.Definition = BuiltIn.capaPragmaDef then Some(this.Arguments.[0].ToLower()) else None

    /// Is this pragma a warning message?
    let isWarning (this: Pragma): bool =
        this.Definition = BuiltIn.warningPragmaDef
    /// Is this pragma a flag to deactivate a warning?
    let ignoresWarning (this: Pragma): string option =
        if this.Definition = BuiltIn.warnoffPragmaDef
        then Some(this.Arguments.[0])
        else None
    /// Is this pragma a #capa directive?
    let isCapa (this: Pragma): bool = this.Definition = BuiltIn.capaPragmaDef
    /// Helper function to check the list of args for a particular value.
    let hasVal (value: string) (this: Pragma): bool = List.contains value this.Arguments


    /// Validated pragma construction during parsing
    let fromDefinition (values: string list) (pDef: PragmaDefinition): Result<Pragma, string> =
        let name = pDef.Name

        // check that the right number of arguments were supplied
        let nArg = values.Length

        let checkNArgs n =
            if nArg <> n
            then fail (sprintf "Pragma #%s expected %d argument(s) but got %d: %A" name n nArg values)
            else ok ()

        let checkMinArgs min =
            if nArg < min
            then fail (sprintf "Pragma #%s expected at least %d argument(s) but got %d: %A" name min nArg values)
            else ok ()

        let checkMaxArgs max _ =
            if nArg > max
            then fail (sprintf "Pragma #%s expected at most %d argument(s) but got %d: %A" name max nArg values)
            else ok ()

        let checkArgShape () =
            match pDef.Shape with
            | Zero -> checkNArgs 0
            | One -> checkNArgs 1
            | Exactly (n) -> checkNArgs n
            | AtLeast (n) -> checkMinArgs n
            | Range (min, max) -> checkMinArgs min >>= checkMaxArgs max
            | ExactlySet (vals) ->
                if not (vals |> List.contains nArg) then
                    fail
                        (sprintf
                            "Pragma %s expected any number of arguments in the set %A but got %d: %A"
                             name
                             vals
                             nArg
                             values)
                else
                    ok ()

        let validateArgs () = pDef.Validate values

        // validation pipeline
        checkArgShape ()
        >>= validateArgs
        >>= (fun () ->
            ok
                { Definition = pDef
                  Arguments = values })

    /// Try to build a pragma from a name and values.
    let fromNameValue (name: string) (values: string list) (cache: PragmaCache): Result<Pragma, string> =
        // try to get the pragma defintion
        match cache.Pragmas |> Map.tryFind name with
        | Some pDef -> pDef |> fromDefinition values
        | None -> fail (sprintf "Unknown or invalid pragma: '#%s'" name)

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

module PragmaDeprecation =
    let private replaceStaticPlatformPragma (which: string) (_target: Pragma): Pragma =
        { Pragma.Definition = BuiltIn.platformPragmaDef
          Arguments = [ which ] }

    let private stitchPragmaDeprecation =
        { PragmaDeprecation.Name = "stitch"
          Replacement = "platform"
          Replace = replaceStaticPlatformPragma "stitch"
          ExtraMessage = Some("This pragma will be interpreted as '#platform stitch'.") }

    let private megastitchPragmaDeprecation =
        { PragmaDeprecation.Name = "megastitch"
          Replacement = "platform"
          Replace = replaceStaticPlatformPragma "megastitch"
          ExtraMessage = Some("This pragma will be interpreted as '#platform megastitch'.") }

    let deprecatedPragmas: Map<string, PragmaDeprecation> =
        [ stitchPragmaDeprecation
          megastitchPragmaDeprecation ]
        |> Seq.map (fun depreciation -> depreciation.Name, depreciation)
        |> Map.ofSeq
