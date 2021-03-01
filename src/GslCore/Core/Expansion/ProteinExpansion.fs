namespace GslCore.Core.Expansion.ProteinExpansion

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

[<RequireQualifiedAccess>]
type ProteinExpansionError =
    | IllegalAminoAcid of char
    | MissingReferenceGenome of string
    | IllegalSeedPragmaArgument of string

module ProteinExpansion =
    let rewritePPP (parameters: Phase2Parameters)
                   (codonProvider: ICodonProvider)
                   (defaultRefGenome: string)
                   (partPlusPragma: PartPlusPragma)
                   : GslResult<PartPlusPragma, ProteinExpansionError> =
        match partPlusPragma.Part with
        | Part.InlineProtein proteinSequence ->

            // Check to see if there is a local #seed parameter and extract it else
            // fall back on the version in the codon opt parameters globally
            let seedOverride =
                match partPlusPragma.Pragma
                      |> PragmaCollection.tryGetValue BuiltIn.seedPragmaDef with
                | None -> GslResult.ok None
                | Some seed ->
                    match System.Int32.TryParse seed with
                    | true, s -> GslResult.ok (Some s)
                    | _ -> GslResult.err (ProteinExpansionError.IllegalSeedPragmaArgument seed)

            let validateProteinSequence =
                proteinSequence
                |> Seq.map (fun aminoAcid ->
                    if not (Default.ValidAminoAcids.Contains(aminoAcid)) then
                        GslResult.err (ProteinExpansionError.IllegalAminoAcid aminoAcid)
                    else
                        GslResult.ok ())
                |> Seq.toList
                |> GslResult.collectM
                |> GslResult.ignore

            validateProteinSequence
            >>= (fun _ -> seedOverride)
            >>= fun seedOverride ->
                    let determinedReferenceGenome =
                        partPlusPragma.Pragma
                        |> PragmaCollection.tryGetValue BuiltIn.refGenomePragmaDef
                        |> Option.defaultValue defaultRefGenome

                    parameters.References
                    |> GenomeDefinitions.get
                    |> Map.tryFind determinedReferenceGenome
                    |> Option.map (fun referenceGenome ->

                        let codonOptTask =
                            { CodonOptTask.IsVerbose = parameters.Verbose
                              SeedOverride = seedOverride
                              RefGenome = referenceGenome
                              AminoAcidSequence = proteinSequence }

                        let result = codonProvider.DoCodonOpt codonOptTask

                        GslResult.ok
                            { partPlusPragma with
                                  Part = Part.InlineDna(result) })
                    |> Option.defaultValue
                        (GslResult.err (ProteinExpansionError.MissingReferenceGenome determinedReferenceGenome))


        | _ -> GslResult.ok partPlusPragma


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
