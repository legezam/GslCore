module GslCore.Core.Expansion.MutationExpansion

open GslCore.Ast.Phase1
open GslCore.Ast.Types
open GslCore.Ast.Algorithms
open GslCore.Constants
open GslCore.GslResult
open GslCore.Legacy.Types
open GslCore.Core.Expansion.Bootstrapping
open GslCore.Legacy
open GslCore.Core.DnaCreation
open GslCore.Core.Expansion
open GslCore.Pragma
open GslCore.Core
open GslCore.Reference
open GslCore.Core.PluginTypes
// TODO although it seems there are no errors this file is returning, actually there are a lot since the file uses failwith for errohandling
// =========================
// expanding mutations
// =========================

let modIsMutation: Modifier -> Mutation option =
    function
    | Modifier.Mutation m -> Some m
    | _ -> None

/// Rewrite an individual part p in an assembly a.
/// Assembly is passed in for context if needed
let rewritePPP (parameters: Phase2Parameters) (assembly: Assembly) (partPlusPragma: PartPlusPragma) =
    let assemblyName =
        match assembly.Name with
        | Some name -> name
        | None -> assembly.Parts.Head.Part.ToString()
    // TODO: we should have already expanded names, so this shouldn't be necessary
    // Refactor Assembly to have an unconditional name.
    match partPlusPragma.Part with
    | Part.HeterologyBlock // don't expect this but just in case
    | Part.InlineDna _
    | Part.InlineProtein _
    | Part.MarkerPart
    | Part.SourceCode _ -> partPlusPragma
    | Part.PartId part ->
        // Does it contain a mutation modification
        match part.Modifiers |> List.choose modIsMutation with
        | [] -> partPlusPragma
        | [ mutMod ] ->
            let rg' =
                getReferenceGenome assembly parameters.References partPlusPragma.Pragma

            let asAACheck =
                match assembly.Pragmas
                      |> PragmaCollection.tryFind BuiltIn.warnoffPragmaDef with
                | Some pragma -> pragma |> Pragma.hasVal "asaacheck" |> not
                | None -> true

            if parameters.Verbose then
                printfn "***** %A" assembly.Pragmas

            // Leave pragmas intact
            { partPlusPragma with
                  Part = Part.SourceCode(AlleleSwaps.expandSimpleMut asAACheck rg' part mutMod) }
        | tooManyMuts -> failwithf "Internal error, found more than one mutation mod: %A" tooManyMuts

    | Part.GenePart gp ->
        // Does it contain a mutation modification?
        // Make sure we have only one if so.
        // TODO: this restriction may not be necessary
        match gp.Part.Modifiers |> List.choose modIsMutation with
        | [] -> partPlusPragma
        | [ mutMod ] ->
            if (not (gp.Part.Gene.[0] = 'G' || gp.Part.Gene.[0] = 'g')) then
                failwithf
                    "Allele swap gene must be g-type  e.g gABC1$x1234y.  '%c' is not a legal prefix for %s"
                    (gp.Part.Gene.[0])
                    gp.Part.Gene

            let rg' =
                getReferenceGenome assembly parameters.References partPlusPragma.Pragma

            // TODO: unclear if this was the right behavior, as the rg is selected from both
            // assembly and part pragmas yet the actual ref genome selected here was only using
            // the assembly pragmas.  Uggh.
            //// Need to select a codon usage table
            //let refGenome = chooseRefGenome (a.pragmas)

            let codonUsage =
                parameters.CodonTableCache.GetCodonLookupTable(rg')

            let endPref =
                match partPlusPragma.Pragma
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
                 |> GenomeDefinition.isValidFeature gp.Part.Gene.[1..]) then
                failwithf "Undefined gene '%s' %O\n" (gp.Part.Gene.[1..]) (gp.Part.Where)

            let asAACheck =
                match assembly.Pragmas
                      |> PragmaCollection.tryFind BuiltIn.warnoffPragmaDef with
                | Some p -> not (p |> Pragma.hasVal "asaacheck")
                | None -> true

            // Check if there is a style pragma on the part itself.  User can designate
            // short or long allele swap styles at the part level
            let longStyle =
                match partPlusPragma.Pragma
                      |> PragmaCollection.tryGetValue BuiltIn.stylePragmaDef with
                | None -> true // default is long style
                | Some ("long") -> true // long style
                | Some ("short") -> false // short style
                | Some (x) -> failwithf "Undefined allele swap style '%s', options are long or short" x

            let swapImpl =
                AlleleSwaps.expandAS
                    parameters.AlleleSwapProviders
                    asAACheck
                    assemblyName
                    parameters.Verbose
                    rg'
                    codonUsage
                    gp.Part.Gene
                    mutMod
                    endPref
                    assembly.Capabilities
                    (partPlusPragma.Pragma
                     |> PragmaCollection.mergeInCollection assembly.Pragmas)  // combine the part and assembly pragmas to pass to the swap
                    longStyle

            { partPlusPragma with
                  Part = Part.SourceCode(swapImpl) }
        | tooManyMuts -> failwithf "Internal error, found more than one mutation mod: %A" tooManyMuts

type MutationExpansionError = MultiPartAssemblyError of parts: PartPlusPragma list

/// Remove mutation definitions and replace with a lower level representation.
/// Note that mutations can expand to more than a single line of GSL, so bootstrap these as a block.
let private expandMut (parameters: Phase2Parameters)
                      (assembly: Assembly)
                      : GslResult<GslSourceCode, MutationExpansionError> =

    // To perform an allele swap, we require an assembly with exactly one part.
    // Check this now and fail if we can't do it.
    let newParts =
        match assembly.Parts with
        | [] -> GslResult.ok []
        | [ singlePart ] -> // we can handle this case
            [ rewritePPP parameters assembly singlePart ]
            |> GslResult.ok
        | x ->
            //            failwithf
//                "Tried to perform a mutation expansion on an assembly with more than one part: %A.  This is currently not supported."
//                x
            GslResult.err (MultiPartAssemblyError x)

    newParts
    |> GslResult.map (fun newParts -> LegacyPrettyPrint.assembly { assembly with Parts = newParts })
// note that the assembly which contains a part which is expanded to literal GSL that actually may
// amount to more than a single line.  Be careful.

/// Expand all mutations in an AST.
let expandMutations (parameters: Phase2Parameters)
                    (tree: AstTreeHead)
                    : GslResult<AstTreeHead, BootstrapExecutionError<BootstrapExpandAssemblyError<BootstrapError<Phase1Error>, MutationExpansionError>>> =

    let assemblyExpansion = expandMut parameters

    let bootstrapOperation =
        let phase1Params =
            parameters |> Phase1Parameters.fromPhase2

        Bootstrapping.bootstrapExpandLegacyAssembly assemblyExpansion (Bootstrapping.bootstrapPhase1 phase1Params)

    let expansionOnlyOnNodesWithMutations =
        BoostrapSelection.maybeBypassBootstrap ExpandMutation bootstrapOperation

    Bootstrapping.executeBootstrap expansionOnlyOnNodesWithMutations Serial tree
