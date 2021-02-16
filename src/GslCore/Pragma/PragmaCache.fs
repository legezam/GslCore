namespace GslCore.Pragma
open Amyris.ErrorHandling
type PragmaCache(pragmas: Map<string, PragmaDefinition>) =
    member this.Pragmas = pragmas
    
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
        
    /// Try to build a pragma from a name and values.
    let pragmaFromNameValue (name: string) (values: string list) (cache: PragmaCache): Result<Pragma, string> =
        // try to get the pragma defintion
        match cache.Pragmas |> Map.tryFind name with
        | Some pDef -> pDef |> Pragma.fromDefinition values
        | None -> fail (sprintf "Unknown or invalid pragma: '#%s'" name)         