module GslCore.Core.DnaCreation

open System

open FsToolkit.ErrorHandling
open GslCore.Ast.Types
open GslCore.Constants
open GslCore.Ast.LegacyParseTypes
open GslCore.Core.ResolveExtPart
open GslCore.GslResult
open GslCore.Pragma
open GslCore.Core.Types
open GslCore.Core
open Amyris.Bio
open Amyris.Dna
open GslCore.Reference
open GslCore.Core.Ryse
open GslCore.Core.PluginTypes

// ================================================================================================
// Slice manipulation routines for getting from gene notation down to specific genomics coordinates
// ================================================================================================
let validateMods (errorRef: string) (where: SourcePosition list) (modifiers: Mod list): unit =
    for modifier in modifiers do
        match modifier with
        | SLICE slice ->
            if slice.left.RelativeTo = slice.right.RelativeTo
               && slice.left.Position > slice.right.Position then
                // TODO: better to report coordinates of slice text rather than gene
                failwithf
                    "slice left %A greater than right %A %O in %s"
                    slice.left.Position
                    slice.right.Position
                    (SourcePosition.formatSourcePositionList where)
                    errorRef
        | _ -> () // No checks for now TODO

/// Given a feature and a relative position, find the zero based physical
/// coordinate for the position.
/// Determine range of DNA needed, translating into physical coordinates
/// Final start of the piece.  Determine which end we are working relative to
let adjustToPhysical (feature: sgd.Feature) (relativePosition: RelativePosition): int<ZeroOffset> =
    let featureOffset =
        match relativePosition.RelativeTo, feature.fwd with
        | FivePrime, true
        | ThreePrime, false -> feature.l
        | ThreePrime, true
        | FivePrime, false -> feature.r

    let geneRelativeOffset =
        let position = relativePosition.Position / 1<OneOffset>

        match relativePosition.RelativeTo, position with
        | FivePrime, position -> if position > 0 then position - 1 else position
        | ThreePrime, position -> if position > 0 then position else position + 1
        * (if feature.fwd then 1 else -1)

    (featureOffset + geneRelativeOffset)
    * 1<ZeroOffset>

/// Generate logical coordinates for the start and end of the gene part
/// these are relative to the gene, not the genome for now.  We transform
/// to genomic coordinates below.
/// Transform all non gXXX forms of gene into gXX forms.
let translateGenePrefix (pragmas: PragmaCollection) (genomeDefinition: GenomeDefinition): StandardSlice -> Slice =
    function
    | StandardSlice.Promoter ->
        let leftPosition =
            let position =
                match pragmas
                      |> PragmaCollection.tryFind BuiltIn.promLenPragmaDef with
                | None -> -(genomeDefinition |> GenomeDefinition.getPromLen)
                | Some pragma -> pragma.Arguments.[0] |> int |> (*) -1<OneOffset>

            { RelativePosition.Position = position
              RelativeTo = FivePrime }

        let rightPosition =
            { RelativePosition.Position = -1<OneOffset>
              RelativeTo = FivePrime }

        { Slice.left = leftPosition
          lApprox = true
          rApprox = false
          right = rightPosition }
    | StandardSlice.Upstream ->
        { Slice.left =
              { RelativePosition.Position = -(genomeDefinition |> GenomeDefinition.getFlank)
                RelativeTo = FivePrime }
          lApprox = true
          rApprox = false
          right =
              { RelativePosition.Position = -1<OneOffset>
                RelativeTo = FivePrime } }
    | StandardSlice.Terminator ->
        { Slice.left =
              { RelativePosition.Position = 1<OneOffset>
                RelativeTo = ThreePrime }
          lApprox = false
          rApprox = true
          right =
              { RelativePosition.Position =
                    match pragmas
                          |> PragmaCollection.tryFind BuiltIn.termLenPragmaDef with
                    | None -> genomeDefinition |> GenomeDefinition.getTermLen
                    | Some p -> p.Arguments.[0] |> int |> (*) 1<OneOffset>
                RelativeTo = ThreePrime } }
    | StandardSlice.Downstream ->
        { Slice.left =
              { RelativePosition.Position = 1<OneOffset>
                RelativeTo = ThreePrime }
          lApprox = false
          rApprox = true
          right =
              { RelativePosition.Position = genomeDefinition |> GenomeDefinition.getFlank
                RelativeTo = ThreePrime } }
    | StandardSlice.FusableOrf ->
        { Slice.left =
              { RelativePosition.Position = 1<OneOffset>
                RelativeTo = FivePrime }
          lApprox = false
          rApprox = false
          right =
              { RelativePosition.Position = -4<OneOffset>
                RelativeTo = ThreePrime } }
    | StandardSlice.Orf ->
        { Slice.left =
              { RelativePosition.Position = 1<OneOffset>
                RelativeTo = FivePrime }
          lApprox = false
          rApprox = false
          right =
              { RelativePosition.Position = -1<OneOffset>
                RelativeTo = ThreePrime } }
    | StandardSlice.Gene ->
        { Slice.left =
              { RelativePosition.Position = 1<OneOffset>
                RelativeTo = FivePrime }
          lApprox = false
          rApprox = false
          right =
              { RelativePosition.Position = -1<OneOffset>
                RelativeTo = ThreePrime } }
    | StandardSlice.MRNA ->
        { Slice.left =
              { RelativePosition.Position = 1<OneOffset>
                RelativeTo = FivePrime }
          lApprox = false
          rApprox = true
          right =
              { RelativePosition.Position =
                    match pragmas
                          |> PragmaCollection.tryFind BuiltIn.termLenMrnaPragmaDef with
                    | None ->
                        genomeDefinition
                        |> GenomeDefinition.getTermLenMRNA
                    | Some p -> p.Arguments.[0] |> int |> (*) 1<OneOffset>
                RelativeTo = ThreePrime } }


/// Translate gene part label.  Raises an exception for errors.
let lookupGenePart (errorDescription: string) (prefix: char) (modifierList: Mod list): StandardSlice =
    let sliceType =
        match StandardSlice.charToSliceType prefix with
        | Some slice -> slice
        | None ->
            failwithf
                "unknown gene prefix '%c' should be one of %s in %s"
                prefix
                (StandardSlice.sliceTypeChars.ToString())
                errorDescription

    let dotModList =
        [ for modifier in modifierList do
            match modifier with
            | DOTMOD dotMod -> yield dotMod
            | _ -> () ]

    match dotModList with
    | [] -> sliceType
    | [ dotMod ] ->
        // if it's a dot mod, it must be on a 'g' part (gYNG2) (For now...)
        if sliceType <> StandardSlice.Gene
        then failwithf "cannot apply DOTMODS to non-gene parts (must be gXYZ). Used %c" prefix

        match dotMod with
        | "up" -> StandardSlice.Upstream
        | "down" -> StandardSlice.Downstream
        | "mrna" -> StandardSlice.MRNA
        | x -> failwithf "unimplemented DOTMOD %s. Options: up, down, mrna" x
    | _ -> failwithf "multiple DOTMODS applied to %s" errorDescription


/// Get the reference genome for a given assembly and set of pragmas.
/// Exception on error.
let getReferenceGenome (assembly: Assembly)
                       (referenceGenomes: GenomeDefinitions)
                       (pragmas: PragmaCollection)
                       : GenomeDefinition =
    // Prefer reference genome from passed-in pragmas over assembly.
    let pragmas = [ pragmas; assembly.pragmas ]

    GenomeDefinitions.getReferenceGenome referenceGenomes pragmas
    |> Result.valueOr failwith

/// Take a genepart and slices and get the actual DNA sequence.
let realizeSequence (verbose: bool)
                    (pragmas: PragmaCollection)
                    (isForward: bool)
                    (referenceGenome: GenomeDefinition)
                    (genePartWithLinker: GenePartWithLinker)
                    : Dna =

    if verbose
    then printf "realizeSequence:  fetch fwd=%s %s\n" (if isForward then "y" else "n") genePartWithLinker.part.gene

    // Inspect prefix of gene e.g g,t,o,p and see what type of gene part we are starting with
    // Description of part to give in case of error
    let errorDesc = genePartWithLinker.part.gene

    let genePart =
        lookupGenePart errorDesc (genePartWithLinker.part.gene.[0]) (genePartWithLinker.part.mods)

    // Come up with an initial slice based on the gene prefix type

    let finalSlice =
        translateGenePrefix pragmas referenceGenome genePart
        |> ApplySlices.applySlices verbose genePartWithLinker.part.mods

    // Lookup gene location
    let feature =
        referenceGenome
        |> GenomeDefinition.getFeature genePartWithLinker.part.gene.[1..]

    let left = adjustToPhysical feature finalSlice.left

    let right =
        adjustToPhysical feature finalSlice.right

    // One final adjustment needed.  We have defined left and right relative to
    // the gene, but if the gene is on the crick strand, we need to both flip
    // the coordinates and reverse complement the resulting DNA to keep it
    // oriented with the construction orientation.
    if (left > right && feature.fwd)
       || (right > left && (not feature.fwd)) then
        failwithf "[realizeSequence] slice results in negatively lengthed DNA piece gene=%s slice=%s\n" feature.gene
            (printSlice finalSlice)

    let left', right' =
        if feature.fwd then left, right else right, left

    referenceGenome
    |> GenomeDefinition.getDna (errorDesc, sprintf "%d" feature.chr, left', right')
    |> DnaOps.revCompIf (not feature.fwd)
    |> DnaOps.revCompIf (not isForward)


/// Extract slice name from a PPP, if it has one.
let getSliceName (ppp: PPP): string =
    ppp.pr
    |> PragmaCollection.tryGetValue BuiltIn.namePragmaDef
    |> Option.defaultValue ""


/// Extract URI from a PPP, if it has one.
let getUri (ppp: PPP): string option =
    ppp.pr
    |> PragmaCollection.tryGetValue BuiltIn.uriPragmaDef

let expandInlineDna (dnaSource: string) (ppp: PPP) (dnaFwd: Dna): DNASlice =

    let dna = dnaFwd |> DnaOps.revCompIf (not ppp.fwd)

    { DNASlice.Id = None
      ExternalId = None
      SliceName = getSliceName ppp
      Uri = getUri ppp
      Dna = dna
      SourceChromosome = "inline"
      SourceFrom = 0<ZeroOffset>
      SourceTo = (dna.Length - 1) * 1<ZeroOffset>
      SourceForward = true
      SourceFromApprox = false
      SourceToApprox = false
      // NB - for now allow that this might be amplified, but could change later
      Template = Some dna
      IsAmplified = false
      // Don't assign coordinates to pieces until later when we decide how they are getting joined up
      DestinationFrom = 0<ZeroOffset>
      DestinationTo = 0<ZeroOffset>
      DestinationForward = ppp.fwd
      Description = (if ppp.fwd then dnaFwd.str else "!" + dnaFwd.str)
      Type = SliceType.Inline
      DnaSource = dnaSource
      Pragmas = ppp.pr
      Breed = Breed.Inline
      MaterializedFrom = Some(ppp)
      Annotations = [] }

let expandGenePart (verbose: bool)
                   (referenceGenomes: GenomeDefinitions)
                   (sequenceLookup: SequenceLibrary)
                   (assembly: Assembly)
                   (specifiedDnaSource: string)
                   (ppp: PPP)
                   (genePartWithLinker: GenePartWithLinker)
                   : DNASlice =

    match genePartWithLinker.linker with
    | None -> () // No linkers were present
    | Some linker -> Ryse.checkLinker linker // Test the linkers

    // If the dna source is empty, then we are going to pull the DNA
    // part from the default reference genome, so we should make the
    // dnaSource field reflect this
    let dnaSource =
        if specifiedDnaSource = "" then Default.RefGenome else specifiedDnaSource


    // Check the genes are legal
    //let prefix = gp.part.gene.[0]
    let gene =
        genePartWithLinker.part.gene.[1..].ToUpper()

    let referenceGenome =
        getReferenceGenome assembly referenceGenomes ppp.pr

    if not (referenceGenome |> GenomeDefinition.isValidFeature gene) then
        // Not a genomic reference but might still be in our library
        if sequenceLookup |> Map.containsKey gene then
            // Yes! =- make up a little island of sequence for it
            let dna = sequenceLookup.[gene]

            // Need to adjust for any slicing carefully since the DNA island is small
            // Validate mods to gene
            let errorRef =
                match assembly.name with
                | None -> sprintf "%A" assembly
                | Some (x) -> x

            validateMods errorRef genePartWithLinker.part.where genePartWithLinker.part.mods

            // Come up with an initial slice based on the gene prefix type

            // Get standard slice range for a gene

            let finalSlice =
                translateGenePrefix assembly.pragmas referenceGenome StandardSlice.Gene
                |> ApplySlices.applySlices verbose genePartWithLinker.part.mods

            // Ban approx slices to stay sane for now
            if finalSlice.lApprox || finalSlice.rApprox then
                failwithf "sorry, approximate slices of library genes not supported yet in %A\n"
                    (prettyPrintAssembly assembly)

            let sliceContext = Library(genePartWithLinker.part.gene)

            let x, y =
                getBoundsFromSlice finalSlice dna.Length sliceContext
                |> Result.valueOr failwith

            let finalDNA =
                dna.[(x / 1<OneOffset>) - 1..(y / 1<OneOffset>) - 1]
                |> DnaOps.revCompIf (not ppp.fwd)

            let orfAnnotation =
                OrfAnnotation.orfAnnotationFromSlice finalSlice finalDNA.Length ppp.fwd sliceContext

            let name1 =
                if genePartWithLinker.part.mods.Length = 0 then
                    genePartWithLinker.part.gene
                else
                    (genePartWithLinker.part.gene
                     + (printSlice finalSlice))

            let name2 = if ppp.fwd then name1 else "!" + name1

            { DNASlice.Id = None
              ExternalId = None
              SliceName = getSliceName ppp
              Uri = getUri ppp // TODO: should we also check a returned library part for a URI?
              Dna = finalDNA
              SourceChromosome = "library"
              SourceFrom =
                  (finalSlice.left.Position / (1<OneOffset>) - 1)
                  * 1<ZeroOffset>
              SourceTo =
                  (finalSlice.right.Position / (1<OneOffset>) - 1)
                  * 1<ZeroOffset>
              SourceForward = true
              SourceFromApprox = false
              SourceToApprox = false
              IsAmplified = false
              // This is what we are expecting to amplify from (library part)
              Template = Some finalDNA
              // Don't assign coordinates to pieces until later when we decide how they are getting joined up
              DestinationFrom = 0<ZeroOffset>
              DestinationTo = 0<ZeroOffset>
              DestinationForward = ppp.fwd
              Description = name2
              Type = SliceType.Regular
              DnaSource = dnaSource
              Pragmas = ppp.pr
              Breed = Breed.X
              MaterializedFrom = Some(ppp)
              Annotations = [ Orf(orfAnnotation) ] }
        else
            failwithf "undefined gene '%s' %O\n" gene genePartWithLinker.part.where
    else
        // Inspect prefix of gene e.g g,t,o,p and see what type of gene part we are starting with
        let errorDesc = genePartWithLinker.part.gene

        let genePart =
            lookupGenePart errorDesc (genePartWithLinker.part.gene.[0]) (genePartWithLinker.part.mods)


        let feature = referenceGenome |> GenomeDefinition.getFeature gene

        let breed1 =
            match genePart with
            | StandardSlice.Promoter -> Breed.Promoter
            | StandardSlice.Terminator -> Breed.Terminator
            | StandardSlice.MRNA -> Breed.GST
            | StandardSlice.Downstream -> Breed.Downstream
            | StandardSlice.Upstream -> Breed.Upstream
            | StandardSlice.FusableOrf -> Breed.FusableOrf
            | StandardSlice.Gene -> Breed.X
            | StandardSlice.Orf -> Breed.GS

        // WARNING - very similar logic in realizeSequence and both versions
        // should be considered when changing logic.
        // FIXME: this common logic should be refactored into a generic function
        // and called in both places.

        // Validate mods to gene
        let errorRef =
            match assembly.name with
            | None -> sprintf "%A" assembly
            | Some (x) -> x

        validateMods errorRef genePartWithLinker.part.where genePartWithLinker.part.mods
        if verbose then printf "log: processing %A\n" assembly

        // finalSlice is the consolidated gene relative coordinate of desired piece
        let finalSlice =
            translateGenePrefix assembly.pragmas referenceGenome genePart
            |> ApplySlices.applySlices verbose genePartWithLinker.part.mods

        // Gene relative coordinates for the gene slice we want
        let finalSliceWithApprox =
            // Calculate some adjusted boundaries in case the left/right edges are approximate
            let leftAdj =
                { finalSlice.left with
                      Position =
                          finalSlice.left.Position
                          - (Default.ApproxMargin * 1<OneOffset>) }

            let rightAdj =
                { finalSlice.right with
                      Position =
                          finalSlice.right.Position
                          + (Default.ApproxMargin * 1<OneOffset>) }

            { Slice.lApprox = finalSlice.lApprox
              left = (if finalSlice.lApprox then leftAdj else finalSlice.left)
              rApprox = finalSlice.rApprox
              right = if finalSlice.rApprox then rightAdj else finalSlice.right }

        if verbose then
            printf "log: finalSlice: %s%s %s%s\n" (if finalSlice.lApprox then "~" else "") (printRP finalSlice.left)
                (if finalSlice.rApprox then "~" else "") (printRP finalSlice.right)

            printf "log: finalSliceWA: %s%s %s%s\n" (if finalSliceWithApprox.lApprox then "~" else "")
                (printRP finalSliceWithApprox.left) (if finalSliceWithApprox.rApprox then "~" else "")
                (printRP finalSliceWithApprox.right)

        // FinalSliceWithApprox is a gene relative coordinate system, but we
        // need genomic coordinates for the gene

        /// fivePrime is the genomic start of the element (can be > stop)
        let fivePrime =
            adjustToPhysical feature finalSliceWithApprox.left
        /// threePrime is the genomic end of the element
        let threePrime =
            adjustToPhysical feature finalSliceWithApprox.right

        assert ((feature.fwd && fivePrime <= threePrime)
                || (not feature.fwd && fivePrime >= threePrime))

        if verbose then
            printf "log: gene: %s %d %d %s\n" feature.gene feature.l feature.r (if feature.fwd then "fwd" else "rev")
            printf "log: prefinal: %s %A %A\n" feature.gene fivePrime threePrime

        // One final adjustment needed.  We have defined left and right relative
        // to the gene, but if the gene is on the crick strand, we need to both
        // flip the coordinates and reverse complement the resulting DNA to keep
        // it oriented with the construction orientation.

        if (fivePrime > threePrime && feature.fwd)
           || (threePrime > fivePrime && (not feature.fwd)) then
            failwithf "slice results in negatively lengthed DNA piece for %s\n"
                (genePartWithLinker.part.gene
                 + (printSlice finalSlice))

        /// left' is the genomic coordinate of the genomic left
        let left', right' =
            if feature.fwd then fivePrime, threePrime else threePrime, fivePrime

        if verbose
        then printf "log: final: %s %A %A\n" feature.gene left' right'

        assert (left' <= right')

        // TOO BLUNT gp.part.gene = "gURA3"
        // TODO: hard coded detection of split marker
        let isMarker = false

        if verbose
        then printf "gettingdna for %s fwd=%s\n" feature.gene (if ppp.fwd then "y" else "n")

        let dna =
            referenceGenome
            |> GenomeDefinition.getDna (errorDesc, sprintf "%d" feature.chr, left', right')
            |> DnaOps.revCompIf (not feature.fwd)
            // One potential final flip if user wants DNA backwards
            |> DnaOps.revCompIf (not ppp.fwd)

        let description1 =
            match genePartWithLinker.part.mods with
            | [] -> genePartWithLinker.part.gene
            | [ DOTMOD (d) ] ->
                match d with
                | "up" -> "u" + genePartWithLinker.part.gene.[1..]
                | "down" -> "d" + genePartWithLinker.part.gene.[1..]
                | "mrna" -> "m" + genePartWithLinker.part.gene.[1..]
                | x -> failwithf "unimplemented DOTMOD %s" x
            | _ ->
                "g"
                + genePartWithLinker.part.gene.[1..]
                + (printSlice finalSlice)

        let description2 =
            if ppp.fwd then description1 else "!" + description1

        let promStart =
            { Position = -300<OneOffset>
              RelativeTo = FivePrime }

        let promEnd =
            { Position = -1<OneOffset>
              RelativeTo = FivePrime }

        let termStart =
            { Position = 1<OneOffset>
              RelativeTo = ThreePrime }

        let termEnd =
            { Position = 150<OneOffset>
              RelativeTo = ThreePrime }

        let near (a: RelativePosition) (b: RelativePosition) (tolerance) =
            a.RelativeTo = b.RelativeTo
            && abs ((a.Position - b.Position) / 1<OneOffset>) < tolerance

        let breed =
            match breed1 with
            | Breed.X ->
                let z = finalSliceWithApprox

                if near z.left termStart 1
                   && near z.right termEnd 100 then
                    Breed.Terminator
                elif near z.left promStart 400
                     && near z.right promEnd 40 then
                    Breed.Promoter
                elif z.left.Position = 1<OneOffset>
                     && z.left.RelativeTo = FivePrime
                     && near z.right termEnd 100 then
                    Breed.GST
                else
                    Breed.X
            | x -> x

        let orfAnnotation =
            OrfAnnotation.orfAnnotationFromSlice finalSlice feature.Length ppp.fwd Genomic

        // Note regarding orientation: We are currently building a single piece
        // of final DNA left to right. There is no consideration for stitch
        // orientation, so even (in RYSE world) B stitch parts are laid out left
        // to right from marker (innermost 9 linker) through to the outer linker.
        // If something is reversed then, it points towards the middle, marker
        // part of the stitch.
        { DNASlice.Id = None
          ExternalId = None
          SliceName = getSliceName ppp
          Uri = getUri ppp
          Dna = dna
          SourceChromosome = feature.chr |> string
          /// NB: sourceFr is the origin of the left hand end of the placed part in final part orientation
          SourceFrom = left'
          /// NB: sourceTo is the origin of the right hand end of the placed part in final part orientation
          SourceTo = right'
          SourceForward = feature.fwd
          IsAmplified = true
          Template = Some dna // This is what we are expecting to amplify from genome
          // Don't assign coordinates to pieces until later when we decide how
          // they are getting joined up. Left and Right are absolute genomic
          // coordinate relative (i.e left has a smaller coordinate than left)

          // This logic is a bit twisted. SourceFrApprox is misleading, this
          // designates whether the *left* end is approx or now and that operation
          // occurs before the orientation of the rabit is applied, so
          // !gABC1[100:~200E] has a sourceFrApprox = false initially.  We flipped
          // the actual piece of DNA so the l and r approx need to move with the DNA
          SourceFromApprox = (if ppp.fwd then finalSlice.lApprox else finalSlice.rApprox)
          SourceToApprox = (if ppp.fwd then finalSlice.rApprox else finalSlice.lApprox)
          DestinationFrom = 0<ZeroOffset>
          DestinationTo = 0<ZeroOffset>
          DestinationForward = ppp.fwd
          Description = description2
          DnaSource = dnaSource
          Type = (if isMarker then SliceType.Marker else SliceType.Regular)
          Pragmas = ppp.pr
          Breed = breed
          MaterializedFrom = Some(ppp)
          Annotations = [ Orf(orfAnnotation) ] }

let private determineTopology (pragmas: PragmaCollection): Topology =
    match pragmas
          |> PragmaCollection.tryFind BuiltIn.topologyPragmaDef with
    | Some pragma ->
        pragma.Arguments
        |> Topology.parse
        |> GslResult.valueOr (fun messages -> messages |> String.concat ";" |> failwith)
    | None -> Linear

/// Take a parsed assembly definition and translate it
/// to underlying DNA pieces, checking the structure in the process.
/// Raises an exception on error.
let expandAssembly (verbose: bool)
                   (markerProviders: IMarkerProvider list)
                   (referenceGenomes: GenomeDefinitions)
                   (library: SequenceLibrary)
                   (index: int)
                   (assembly: Assembly)
                   : DnaAssembly =

    let rec expandPPPList (pppList: PPP seq): DNASlice list =
        seq {
            // NOTE: have access to part.pragmas to the extent they influence generation
            for ppp in pppList do
                let dnaSource =
                    match ppp.pr
                          |> PragmaCollection.tryGetValue BuiltIn.dnaSrcPragmaDef with
                    | Some (d) -> d
                    | None ->
                        // specifying a different reference genome implies a non standard
                        // DNA source, so we can use that too (they can override with dnasrc)
                        match ppp.pr
                              |> PragmaCollection.tryGetValue BuiltIn.refGenomePragmaDef with
                        | Some (rg) -> rg
                        | None ->
                            match assembly.pragmas
                                  |> PragmaCollection.tryGetValue BuiltIn.refGenomePragmaDef with
                            | Some (rg) -> rg
                            | None -> "" // Revert to the current default part origin

                match ppp.part with
                | MARKERPART ->
                    // Choose provider
                    let providers =
                        markerProviders
                        |> List.choose (fun provider ->
                            match provider.ScoreJob assembly.capabilities with
                            | None -> None
                            | Some (score) -> Some(score, provider))

                    if providers = []
                    then failwithf "MARKERPART substitution, no marker provider available"

                    let _, markerProvider = providers |> List.maxBy (fst)

                    let markerSet =
                        match assembly.pragmas
                              |> PragmaCollection.tryGetValue BuiltIn.markersetPragmaDef with
                        | Some (x) ->
                            let x' = x.ToLower()

                            if not (markerProvider.IsLegal x') then
                                failwithf "ERROR: unknown marker set %s, expected one of %s\n" x'
                                    (String.Join(",", markerProvider.ListMarkers()))
                            else
                                x'
                        | None -> "default"

                    let task =
                        { DnaSource = dnaSource
                          PartPlusPragma = ppp
                          MarkerSet = markerSet }

                    yield markerProvider.CreateDna(task) // expandMarkerPart library dnaSource ppp
                | PARTID partId -> yield ResolveExtPart.fetchSequence verbose library ppp partId
                | INLINEDNA dna -> yield expandInlineDna dnaSource ppp dna
                | INLINEPROT _ -> failwith "unexpanded protein inline encountered during DNA generation"
                | HETBLOCK -> failwith "unexpanded heterology block encountered during DNA generation"
                | SOURCE_CODE _ -> ()
                | GENEPART gp -> yield expandGenePart verbose referenceGenomes library assembly dnaSource ppp gp
                //
                // Might also want to yield a fusion slice
                //
                if ppp.pr
                   |> PragmaCollection.contains BuiltIn.fusePragmaDef then
                    yield DnaAssembly.fusionSliceConstant
        }
        |> List.ofSeq
        |> DNASlice.recalculatOffset

    let materializedParts = expandPPPList assembly.parts

    let assemblyName =
        match assembly.name with
        | None -> sprintf "A%d" index
        | Some (s) -> s

    let topology = assembly.pragmas |> determineTopology

    { DnaAssembly.Id = Some index
      DnaParts = materializedParts
      Name = assemblyName
      Uri = assembly.uri
      LinkerHint = assembly.linkerHint
      Pragmas = assembly.pragmas
      DesignParams = assembly.designParams
      DocStrings = assembly.docStrings
      MaterializedFrom = assembly
      Tags = Set.empty
      Topology = topology }
