/// IO routines for loading the reference file format
namespace GslCore.Reference

open System.Collections.Generic
open System.IO
open System
open Amyris.Bio
open Amyris.Bio.sgd
open Amyris.Bio.utils
open Amyris.Dna
open GslCore.Constants

type UninitializedGenomeDefinition = { LibraryPath: string; Name: string }

type InitializedGenomeDefinition =
    { UninitializedState: UninitializedGenomeDefinition
      Environment: Map<string, string> }

type LoadedGenomeDefinition =
    { Fasta: Dictionary<string, Dna>
      Features: Feature []
      FeatureIndex: Map<string, int>
      SuffixTreePath: string
      ReferenceDirectory: string
      InitState: InitializedGenomeDefinition }

type GenomeDefinition =
    | Uninitialized of UninitializedGenomeDefinition
    | Initialized of InitializedGenomeDefinition
    | Loaded of LoadedGenomeDefinition

module GenomeDefinition =
    let private refDir (state: UninitializedGenomeDefinition) = opj state.LibraryPath state.Name

    let private loadConfiguration (path: string): Map<string, string> =
        if File.Exists(path) then
            eachLineIn path
            |> Seq.map (fun x -> x.Split([| "=" |], StringSplitOptions.None))
            |> Seq.choose (fun cols ->
                match cols with
                | [| a; b |] -> Some(a.Trim(), b.Trim())
                | _ ->
                    printf "WARNING: bad config entry '%A'" cols
                    None)
            |> Map.ofSeq
        else
            Map.empty

    let createLazy (libDir: string) (name: string): GenomeDefinition =
        { UninitializedGenomeDefinition.Name = name
          LibraryPath = libDir }
        |> Uninitialized

    let private initializeInternal (state: UninitializedGenomeDefinition): InitializedGenomeDefinition =
        let refDir = state |> refDir
        let config =
            opj refDir "config.txt"
            |> loadConfiguration

        { InitializedGenomeDefinition.Environment = config
          UninitializedState = state }


    let private loadInternal (state: InitializedGenomeDefinition): LoadedGenomeDefinition =
        let initialState = state.UninitializedState
        let refDir = initialState |> refDir
        let name = initialState.Name

        let featsPath =
            opj refDir (sprintf "%s_features.tab" name)

        let fastaPath = opj refDir (sprintf "%s.fsa" name)
        let suffixTreePath = opj refDir "suffixTree.st"

        let fasta =
            let result = Dictionary<_, _>()

            for kv in biolib.readReference fastaPath do
                result.Add(kv.Key, (Dna(kv.Value, true, AllowAmbiguousBases)))

            result

        let feats = sgd.loadFeatures featsPath

        let i1 =
            feats |> Array.mapi (fun i f -> f.sysName, i)

        let i2 =
            feats |> Array.mapi (fun i f -> f.gene, i)

        let featIndex =
            Array.concat [ i1; i2 ]
            |> Seq.filter (fun (x, _) -> x <> "")
            |> Map.ofSeq


        { LoadedGenomeDefinition.Fasta = fasta
          Features = feats
          FeatureIndex = featIndex
          SuffixTreePath = suffixTreePath
          ReferenceDirectory = refDir
          InitState = state }

    let getLoadedState: GenomeDefinition -> LoadedGenomeDefinition * GenomeDefinition =
        function
        | Loaded state as loaded -> state, loaded
        | Initialized state ->
            let updatedState = loadInternal state
            updatedState, Loaded updatedState
        | Uninitialized state ->
            let updatedState = state |> initializeInternal |> loadInternal
            updatedState, Loaded updatedState

    let getInitializedState: GenomeDefinition -> InitializedGenomeDefinition * GenomeDefinition =
        function
        | Loaded state as originalState -> state.InitState, originalState
        | Initialized state as originalState -> state, originalState
        | Uninitialized state ->
            let updatedState = state |> initializeInternal
            updatedState, Initialized updatedState

    let initialize (this: GenomeDefinition): GenomeDefinition = this |> getInitializedState |> snd
    
    let load (this: GenomeDefinition): GenomeDefinition = this |> getLoadedState |> snd
    
    let createEager (libDir: string) (name: string): GenomeDefinition =
        createLazy libDir name
        |> load
    
    let getUninitializedState: GenomeDefinition -> UninitializedGenomeDefinition =
        function
        | Loaded state -> state.InitState.UninitializedState
        | Initialized state -> state.UninitializedState
        | Uninitialized state -> state

    let getFeatureLazy (feature: string) (this: GenomeDefinition): Feature * GenomeDefinition =
        let (loadedState, genomeDefinitionState) = this |> getLoadedState

        let result =
            loadedState.Features.[loadedState.FeatureIndex.[feature]]

        result, genomeDefinitionState
        
    let getFeature (feature: string) (this: GenomeDefinition): Feature = getFeatureLazy feature this |> fst
    
    let isValidFeatureLazy (feature: string) (this: GenomeDefinition): bool * GenomeDefinition =
        let (loadedState, genomeDefinitionState) = this |> getLoadedState

        let result = loadedState.FeatureIndex.ContainsKey feature

        result, genomeDefinitionState
        
    let isValidFeature (feature: string) (this: GenomeDefinition): bool = isValidFeatureLazy feature this |> fst        

    let getName (this: GenomeDefinition): string =
        let state = this |> getUninitializedState
        let result = state.Name
        result

    let getEnvLazy (this: GenomeDefinition): Map<string, string> * GenomeDefinition =
        let (initializedState, genomeDefinitionState) = this |> getInitializedState
        let result = initializedState.Environment
        result, genomeDefinitionState

    let getEnv (this: GenomeDefinition): Map<string, string> = getEnvLazy this |> fst
    
    let envLenLookupLazy (name: string)
                     (defaultValue: int)
                     (this: GenomeDefinition)
                     : int<OneOffset> * GenomeDefinition =

        let (initializedState, genomeDefinitionState) = this |> getInitializedState

        let result =
            if initializedState.Environment.ContainsKey(name) then
                (initializedState.Environment.[name] |> int)
                * 1<OneOffset>
            else
                defaultValue * 1<OneOffset>

        result, genomeDefinitionState
        
    let envLenLookup (name: string)
                     (defaultValue: int)
                     (this: GenomeDefinition)
                     : int<OneOffset> = envLenLookupLazy name defaultValue this |> fst        
        
    /// default or custom length for flanking regions
    let getFlankLazy (this: GenomeDefinition) =
        this |> envLenLookupLazy "flanklen" Default.FlankLength
        
    let getFlank = getFlankLazy >> fst
    /// default or custom length for stand alone terminator pieces
    let getTermLenLazy (this: GenomeDefinition) =
        this |> envLenLookupLazy "termlen" Default.TerminatorLength
        
    let getTermLen = getTermLenLazy >> fst
    /// default or custom length for terminator part of an mRNA type part
    let getTermLenMRNALazy (this: GenomeDefinition) =
        this |> envLenLookupLazy "termlenmrna" Default.MRNATerminatorLength
        
    let getTermLenMRNA = getTermLenMRNALazy >> fst
    /// default or custom length for promoter
    let getPromLenLazy (this: GenomeDefinition) =
        this |> envLenLookupLazy "promlen" Default.PromoterLength
        
    let getPromLen = getPromLenLazy >> fst

    /// Return a list of codons to avoid, if this reference genome has them defined.
    let getCodonAvoidLazy (this: GenomeDefinition): Dna list * GenomeDefinition =

        let (initializedState, genomeDefinitionState) = this |> getInitializedState

        let result =
            match initializedState.Environment.TryFind("codonavoid") with
            | None -> []
            | Some (x) ->
                x.Split([| ' '; '\t' |])
                |> List.ofArray
                |> List.map (fun s -> s.Replace('U', 'T'))
                |> List.map Dna

        result, genomeDefinitionState
        
    let getCodonAvoid = getCodonAvoidLazy >> fst

    let getAllFeaturesLazy (this: GenomeDefinition): Feature [] * GenomeDefinition =
        let (loadedState, genomeDefinitionState) = this |> getLoadedState

        let result =
            let distinctIndices =
                [ for kv in loadedState.FeatureIndex -> kv.Value ]
                |> List.distinct

            [| for i in distinctIndices -> loadedState.Features.[i] |]

        result, genomeDefinitionState

    let getAllFeatures = getAllFeaturesLazy >> fst
    
    let getDnaLazy (errorContext: string, chr: string, left: int<ZeroOffset>, right: int<ZeroOffset>)
               (this: GenomeDefinition)
               : Dna * GenomeDefinition =
        let (loadedState, genomeDefinitionState) = this |> getLoadedState

        let result =
            let left = left / 1<ZeroOffset>
            let right = right / 1<ZeroOffset>

            if not (right >= left) then
                failwithf "ERROR: For %s Attempt to retrieve  DNA slice with reversed coordinates %s:%d-%d\n"
                    errorContext chr left right

            if not (loadedState.Fasta.ContainsKey(chr))
            then failwithf "ERROR: For %s unknown chromsome '%s'" errorContext chr

            if left < 0 || left >= loadedState.Fasta.[chr].Length then
                failwithf
                    "ERROR: For %s coordinate '%d' outside chromsome %s length = %d"
                    errorContext
                    left
                    chr
                    (loadedState.Fasta.[chr].Length)

            if right < 0
               || right >= loadedState.Fasta.[chr].Length then
                failwithf
                    "ERROR: For %s coordinate '%d' outside chromsome %s length = %d"
                    errorContext
                    right
                    chr
                    (loadedState.Fasta.[chr].Length)

            loadedState.Fasta.[chr].[left..right]

        result, genomeDefinitionState
        
    let getDna foo this = getDnaLazy foo this |> fst
