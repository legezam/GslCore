module GslCore.Core.Expansion.MutationExpansion

open GslCore.Ast.Types
open GslCore.Ast.ErrorHandling
open GslCore.Ast.Algorithms
open GslCore.Ast.LegacyParseTypes
open GslCore.Core.DnaCreation
open GslCore.Core.Expansion
open GslCore.Pragma
open GslCore.Core
open GslCore.Reference
open GslCore.Core.PluginTypes

// =========================
// expanding mutations
// =========================

let modIsMutation m =
    match m with
    | MUTATION (m) -> Some(m)
    | _ -> None

/// Remove mutation definitions and replace with a lower level representation.
/// Note that mutations can expand to more than a single line of GSL, so bootstrap these as a block.
let private expandMut (parameters: Phase2Parameters) (assembly: Assembly) =
    /// Rewrite an individual part p in an assembly a.
    /// Assembly is passed in for context if needed
    let rewritePPP (a: Assembly) (p: PPP) =
        let aName =
            match a.name with
            | Some (n) -> n
            | None -> a.parts.Head.part.ToString()
        // FIXME: we should have already expanded names, so this shouldn't be necessary
        // Refactor Assembly to have an unconditional name.
        match p.part with
        | HETBLOCK -> p // don't expect this but just in case
        | INLINEDNA _ -> p
        | INLINEPROT _ -> p
        | MARKERPART -> p
        | SOURCE_CODE _ -> p
        | PARTID part ->
            // Does it contain a mutation modification
            match part.mods |> List.choose modIsMutation with
            | [] -> p
            | [ mutMod ] ->
                let rg' = getRG a parameters.References p.pr

                let asAACheck =
                    match a.pragmas
                          |> PragmaCollection.tryFind BuiltIn.warnoffPragmaDef with
                    | Some pragma -> pragma |> Pragma.hasVal "asaacheck" |> not
                    | None -> true

                if parameters.Verbose then printfn "***** %A" a.pragmas

                // Leave pragmas intact
                { p with
                      part = SOURCE_CODE(AlleleSwaps.expandSimpleMut asAACheck rg' part mutMod) }
            | tooManyMuts -> failwithf "Internal error, found more than one mutation mod: %A" tooManyMuts

        | GENEPART (gp) ->
            // Does it contain a mutation modification?
            // Make sure we have only one if so.
            // TODO: this restriction may not be necessary
            match gp.part.mods |> List.choose modIsMutation with
            | [] -> p
            | [ mutMod ] ->
                if (not (gp.part.gene.[0] = 'G' || gp.part.gene.[0] = 'g')) then
                    failwithf
                        "Allele swap gene must be g-type  e.g gABC1$x1234y.  '%c' is not a legal prefix for %s"
                        (gp.part.gene.[0])
                        gp.part.gene

                let rg' = getRG a parameters.References p.pr

                // FIXME: unclear if this was the right behavior, as the rg is selected from both
                // assembly and part pragmas yet the actual ref genome selected here was only using
                // the assembly pragmas.  Uggh.
                //// Need to select a codon usage table
                //let refGenome = chooseRefGenome (a.pragmas)

                let codonUsage =
                    parameters.CodonTableCache.GetCodonLookupTable(rg')

                let endPref =
                    match p.pr
                          |> PragmaCollection.tryGetValue BuiltIn.swapEndPragmaDef with
                    | Some ("5") -> NTERM
                    | Some ("3") -> CTERM
                    | Some _ -> failwithf "#swapend argument should be 5 or 3"
                    | None -> NONETERM

                if parameters.Verbose then
                    printf "Mutation endpreference: %s\n"
                        (match endPref with
                         | NTERM -> "Nterm"
                         | CTERM -> "Cterm"
                         | NONETERM -> "No end preference")

                if not
                    (rg'
                     |> GenomeDefinition.isValidFeature gp.part.gene.[1..]) then
                    failwithf "Undefined gene '%s' %O\n" (gp.part.gene.[1..]) (gp.part.where)

                let asAACheck =
                    match a.pragmas
                          |> PragmaCollection.tryFind BuiltIn.warnoffPragmaDef with
                    | Some (p) -> not (p |> Pragma.hasVal "asaacheck")
                    | None -> true

                // Check if there is a style pragma on the part itself.  User can designate
                // short or long allele swap styles at the part level
                let longStyle =
                    match p.pr
                          |> PragmaCollection.tryGetValue BuiltIn.stylePragmaDef with
                    | None -> true // default is long style
                    | Some ("long") -> true // long style
                    | Some ("short") -> false // short style
                    | Some (x) -> failwithf "Undefined allele swap style '%s', options are long or short" x

                let swapImpl =
                    AlleleSwaps.expandAS
                        parameters.AlleleSwapProviders
                        asAACheck
                        aName
                        parameters.Verbose
                        rg'
                        codonUsage
                        gp.part.gene
                        mutMod
                        endPref
                        a.capabilities
                        (p.pr
                         |> PragmaCollection.mergeInCollection a.pragmas)  // combine the part and assembly pragmas to pass to the swap
                        longStyle

                { p with part = SOURCE_CODE(swapImpl) } // Leave pragmas intact
            | tooManyMuts -> failwithf "Internal error, found more than one mutation mod: %A" tooManyMuts

    // To perform an allele swap, we require an assembly with exactly one part.
    // Check this now and fail if we can't do it.
    let newParts =
        match assembly.parts with
        | [] -> [] // empty assembly; weird but no reason to blow up here.
        | [ singlePart ] -> // we can handle this case
            [ rewritePPP assembly singlePart ]
        | x ->
            failwithf
                "Tried to perform a mutation expansion on an assembly with more than one part: %A.  This is currently not supported."
                x

    prettyPrintAssembly { assembly with parts = newParts }
// note that the assembly which contains a part which is expanded to literal GSL that actually may
// amount to more than a single line.  Be careful.

/// Expand all mutations in an AST.
let expandMutations (parameters: Phase2Parameters) (tree: AstTreeHead) =

    let assemblyExpansion = expandMut parameters

    let bootstrapOperation =
        let phase1Params =
            parameters |> Phase1Parameters.fromPhase2

        Bootstrapping.bootstrapExpandLegacyAssembly
            MutationError
            assemblyExpansion
            (Bootstrapping.bootstrapPhase1 phase1Params)

    let expansionOnlyOnNodesWithMutations =

        BoostrapSelection.maybeBypassBootstrap ExpandMutation bootstrapOperation

    Bootstrapping.executeBootstrap expansionOnlyOnNodesWithMutations Serial tree
