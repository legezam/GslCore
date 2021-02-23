namespace GslCore.Reference
open System

open GslCore.Constants
open GslCore.Pragma

type GenomeDefinitions =
    | GenomeDefinitions of Map<string, GenomeDefinition>

module GenomeDefinitions =

    let create = GenomeDefinitions
    
    let empty = Map.empty |> create
    
    let get (GenomeDefinitions this) = this
    
    /// Get a reference genome from an ordered set of pragma collections.
    let getReferenceGenome (GenomeDefinitions genomeDefinitions) (pragmaCollections: PragmaCollection list): Result<GenomeDefinition, string> =
        let refGenomeName =

            pragmaCollections
            |> List.tryPick (fun pragma ->
                pragma
                |> PragmaCollection.tryGetValue BuiltIn.refGenomePragmaDef)
            |> Option.defaultValue Default.RefGenome


        match genomeDefinitions |> Map.tryFind refGenomeName with
        | Some genomeDef -> Ok genomeDef
        | None when refGenomeName = Default.RefGenome ->
            Error
                (sprintf
                    "ERROR: unable to load default genome '%s' <currently loaded: %s>"
                     Default.RefGenome
                     (if genomeDefinitions.Count = 0
                      then "none"
                      else String.Join(",", [ for k in genomeDefinitions -> k.Key ])))
        | _ ->
            Error
                (sprintf "ERROR: no such refgenome '%s', options are\n%s" refGenomeName
                     (String.Join("\n", seq { for k in genomeDefinitions -> sprintf "    '%s'" k.Key })))


    let refGenomeWarning () = "" // DISABLED FOR NOW  ..  // sprintf "Warning, defaulting to %s codon usage, no #refgenome specified\n" defaultRefGenome

    /// If a ref genome is specified in pragmas, return it.
    /// Otherwise return the default ref genome.
    // TODO: warning should be swallowed up by ROP rather than printed.
    let chooseReferenceGenome (pragmaCollection: PragmaCollection): string =
        match pragmaCollection
              |> PragmaCollection.tryGetValue BuiltIn.refGenomePragmaDef with
        | Some refGenome -> refGenome
        | None ->
            printf "%s" (refGenomeWarning ())
            Default.RefGenome // Warning - defualts to yeast codon usage