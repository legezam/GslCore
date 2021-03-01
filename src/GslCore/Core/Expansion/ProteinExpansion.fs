module GslCore.Core.Expansion.ProteinExpansion

open GslCore.Ast.Phase1
open GslCore.Ast.Types
open GslCore.Constants
open GslCore.Ast.Algorithms
open GslCore.Core.Expansion.Bootstrapping
open GslCore.GslResult
open GslCore.Legacy.Types
open GslCore.Legacy
open GslCore.Core
open GslCore.Core.Expansion
open GslCore.Pragma
open GslCore.Reference
open GslCore.Core.PluginTypes

// ====================
// expanding inline protein sequences
// ====================

type ProteinExpansionError = IllegalAminoAcid of char

let rewritePPP (parameters: Phase2Parameters)
               (codonProvider: ICodonProvider)
               (refGenome: string)
               (p: PartPlusPragma)
               : GslResult<PartPlusPragma, ProteinExpansionError> =
    match p.Part with
    | Part.InlineProtein s ->
        // Check amino acid sequence is legal
        for c in s do
            if not (Default.ValidAminoAcids.Contains(c)) then
                failwithf "Protein sequence contains illegal amino acid '%c'" c

        let refGenome' =
            match p.Pragma
                  |> PragmaCollection.tryGetValue BuiltIn.refGenomePragmaDef with
            | Some rg -> rg
            | None -> refGenome

        match parameters.References
              |> GenomeDefinitions.get
              |> Map.tryFind refGenome' with
        | None -> failwithf "Unable to load refgenome %s to determine environment" refGenome'
        | Some (genomeDef) ->
            // Check to see if there is a local #seed parameter and extract it else
            // fall back on the version in the codon opt parameters globally
            let seedOverride =
                match p.Pragma
                      |> PragmaCollection.tryGetValue BuiltIn.seedPragmaDef with
                | None -> None
                | Some (seed) ->
                    match System.Int32.TryParse seed with
                    | true, s -> Some(s)
                    | _ -> failwithf "#seed argument '%s' is not a valid integer" seed

            let codonOptTask =
                { CodonOptTask.IsVerbose = parameters.Verbose
                  SeedOverride = seedOverride
                  RefGenome = genomeDef
                  AminoAcidSequence = s }

            let result = codonProvider.DoCodonOpt codonOptTask
            GslResult.ok { p with Part = Part.InlineDna(result) }
    | _ -> GslResult.ok p


/// Take inline protein sequences and expand them out to DNA sequences
let private expandProtein (parameters: Phase2Parameters)
                          (assembly: Assembly)
                          : GslResult<GslSourceCode, ProteinExpansionError> =

    let refGenome =
        GenomeDefinitions.chooseReferenceGenome assembly.Pragmas

    let configuredCodonProvider =
        parameters.CodonTableCache.Setup(assembly.Pragmas)

    assembly.Parts
    |> List.map (rewritePPP parameters configuredCodonProvider refGenome)
    |> GslResult.collectA
    |> GslResult.map (fun expandedParts ->
        { assembly with Parts = expandedParts }
        |> LegacyPrettyPrint.assembly)



/// Expand all inline protein sequences in an AST.
let expandInlineProteins (parameters: Phase2Parameters)
                         (tree: AstTreeHead)
                         : GslResult<AstTreeHead, BootstrapExecutionError<BootstrapExpandAssemblyError<BootstrapError<Phase1Error>, ProteinExpansionError>>> =

    let mode =
        if parameters.Parallel then Parallel else Serial

    let assemblyExpansion = expandProtein parameters

    let bootstrapOperation =
        let phase1Params =
            parameters |> Phase1Parameters.fromPhase2

        Bootstrapping.bootstrapExpandLegacyAssembly assemblyExpansion (Bootstrapping.bootstrapPhase1 phase1Params)

    let expansionOnlyOnNodesWithProteins =
        BoostrapSelection.maybeBypassBootstrap ExpandProtein bootstrapOperation

    Bootstrapping.executeBootstrap expansionOnlyOnNodesWithProteins mode tree
