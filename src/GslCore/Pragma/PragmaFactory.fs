namespace GslCore.Pragma

open GslCore.GslResult


type PragmaFactory(pragmas: Map<string, PragmaDefinition>) =
    member this.Pragmas = pragmas

[<RequireQualifiedAccess>]
type PragmaFactoryError =
    | MissingDefinition of name: string
    | PragmaCreation of PragmaArgumentError

[<RequireQualifiedAccess>]
type PragmaInversionError =
    | UnknownTarget of PragmaDefinition * expectedTarget: string
    | IncompatibleTarget of source: PragmaDefinition * target: PragmaDefinition

module PragmaFactory =
    /// Check that a pragma inverts to a legal pragma.  Returns the pragma it inverts to or raises an exception.
    let inverts (pragmaDefinition: PragmaDefinition)
                (cache: PragmaFactory)
                : GslResult<PragmaDefinition option, PragmaInversionError> =
        match pragmaDefinition.InvertsTo with
        | None -> GslResult.ok None
        | Some name ->
            match cache.Pragmas.TryFind name with
            | None -> GslResult.err (PragmaInversionError.UnknownTarget(pragmaDefinition, name))
            | Some result -> // inverts to a known pragma, make sure they have the same shape
                if pragmaDefinition.Shape <> result.Shape then
                    GslResult.err (PragmaInversionError.IncompatibleTarget(pragmaDefinition, result))
                else
                    GslResult.ok (Some result)

    let create (pragmas: PragmaDefinition list): PragmaFactory =
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

        let cache = PragmaFactory(pragsByName)
        // Make sure any pragmas that invert do it sensibly.
        // Raises an exception if any one doesn't validate.
        for pragmaDefinition in pragmaDefinitions do
            cache |> inverts pragmaDefinition |> ignore

        cache

    let createWithBuiltinPragmas (pragmas: PragmaDefinition list) = pragmas @ BuiltIn.all |> create

    let builtin = BuiltIn.all |> create

    /// Print all available pragmas.
    let printPragmaUsage (cache: PragmaFactory) =
        let orderedPragmas =
            cache.Pragmas
            |> Map.toList
            |> List.sortBy fst // sort the pairs by name
            |> List.map snd // pull out just the sorted pragmas

        for p in orderedPragmas do
            printfn "%s" (PragmaDefinition.format p)

    /// Try to build a pragma from a name and values.
    let createPragmaFromNameValue (name: string)
                                  (values: string list)
                                  (cache: PragmaFactory)
                                  : GslResult<Pragma, PragmaFactoryError> =
        match cache.Pragmas |> Map.tryFind name with
        | Some definition ->
            definition
            |> Pragma.fromDefinition values
            |> GslResult.mapError PragmaFactoryError.PragmaCreation
        | None -> GslResult.err (PragmaFactoryError.MissingDefinition name)
