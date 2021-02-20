module GslCore.Core.DnaCreation

open System

open GslCore.Ast.Types
open GslCore.Constants
open GslCore.Ast.LegacyParseTypes
open GslCore.Core.ResolveExtPart
open GslCore.Pragma
open GslCore.Core.Types
open GslCore.Core
open Amyris.Bio
open Amyris.ErrorHandling
open Amyris.Dna
open GslCore.Reference
open GslCore.Core.Ryse
open GslCore.Core.PluginTypes

// ================================================================================================
// Slice manipulation routines for getting from gene notation down to specific genomics coordinates
// ================================================================================================
let validateMods errorRef (where: SourcePosition list) (mods: Mod list) =
    for m in mods do
        match m with
        | SLICE (s) ->
            if s.left.RelativeTo = s.right.RelativeTo
               && s.left.Position > s.right.Position then
                // TODO: better to report coordinates of slice text rather than gene
                failwithf
                    "slice left %A greater than right %A %O in %s"
                    s.left.Position
                    s.right.Position
                    (SourcePosition.formatSourcePositionList where)
                    errorRef
        | _ -> () // No checks for now TODO

/// Given a feature and a relative position, find the zero based physical
/// coordinate for the position.
/// Determine range of DNA needed, translating into physical coordinates
/// Final start of the piece.  Determine which end we are working relative to
let adjustToPhysical (feat: sgd.Feature) (f: RelativePosition) =
    let featOffset =
        match f.RelativeTo, feat.fwd with
        | FivePrime, true
        | ThreePrime, false -> feat.l
        | ThreePrime, true
        | FivePrime, false -> feat.r

    let geneRelativeOffset =
        match f.RelativeTo, f.Position / 1<OneOffset> with
        | FivePrime, o when o > 0 -> o - 1
        | FivePrime, o -> o
        | ThreePrime, o when o > 0 -> o
        | ThreePrime, o -> o + 1
        * (if feat.fwd then 1 else -1)

    (featOffset + geneRelativeOffset) * 1<ZeroOffset>

/// Generate logical coordinates for the start and end of the gene part
/// these are relative to the gene, not the genome for now.  We transform
/// to genomic coordinates below.
/// Transform all non gXXX forms of gene into gXX forms.
let translateGenePrefix (pragmas: PragmaCollection) (gd: GenomeDefinition) (gPart: StandardSlice) =
    match gPart with
    | StandardSlice.Promoter ->
        { left =
              { Position =
                    match pragmas
                          |> PragmaCollection.tryFind BuiltIn.promLenPragmaDef with
                    | None -> -(gd |> GenomeDefinition.getPromLen)
                    | Some p -> p.Arguments.[0] |> int |> (*) -1<OneOffset>
                RelativeTo = FivePrime }
          lApprox = true
          rApprox = false
          right =
              { Position = -1<OneOffset>
                RelativeTo = FivePrime } }
    | StandardSlice.Upstream ->
        { left =
              { Position = -(gd |> GenomeDefinition.getFlank)
                RelativeTo = FivePrime }
          lApprox = true
          rApprox = false
          right =
              { Position = -1<OneOffset>
                RelativeTo = FivePrime } }
    | StandardSlice.Terminator ->
        { left =
              { Position = 1<OneOffset>
                RelativeTo = ThreePrime }
          lApprox = false
          rApprox = true
          right =
              { Position =
                    match pragmas
                          |> PragmaCollection.tryFind BuiltIn.termLenPragmaDef with
                    | None -> gd |> GenomeDefinition.getTermLen
                    | Some p -> p.Arguments.[0] |> int |> (*) 1<OneOffset>
                RelativeTo = ThreePrime } }
    | StandardSlice.Downstream ->
        { left =
              { Position = 1<OneOffset>
                RelativeTo = ThreePrime }
          lApprox = false
          rApprox = true
          right =
              { Position = gd |> GenomeDefinition.getFlank
                RelativeTo = ThreePrime } }
    | StandardSlice.FusableOrf ->
        { left =
              { Position = 1<OneOffset>
                RelativeTo = FivePrime }
          lApprox = false
          rApprox = false
          right =
              { Position = -4<OneOffset>
                RelativeTo = ThreePrime } }
    | StandardSlice.Orf ->
        { left =
              { Position = 1<OneOffset>
                RelativeTo = FivePrime }
          lApprox = false
          rApprox = false
          right =
              { Position = -1<OneOffset>
                RelativeTo = ThreePrime } }
    | StandardSlice.Gene ->
        { left =
              { Position = 1<OneOffset>
                RelativeTo = FivePrime }
          lApprox = false
          rApprox = false
          right =
              { Position = -1<OneOffset>
                RelativeTo = ThreePrime } }
    | StandardSlice.MRNA ->
        { left =
              { Position = 1<OneOffset>
                RelativeTo = FivePrime }
          lApprox = false
          rApprox = true
          right =
              { Position =
                    match pragmas
                          |> PragmaCollection.tryFind BuiltIn.termLenMrnaPragmaDef with
                    | None -> gd |> GenomeDefinition.getTermLenMRNA
                    | Some p -> p.Arguments.[0] |> int |> (*) 1<OneOffset>
                RelativeTo = ThreePrime } }


/// Translate gene part label.  Raises an exception for errors.
let lookupGenePart errorDescription prefix (modList: Mod list) =
    let sliceType =
        match StandardSlice.charToSliceType prefix with
        | Some (t) -> t
        | None ->
            failwithf
                "unknown gene prefix '%c' should be one of %s in %s"
                prefix
                (StandardSlice.sliceTypeChars.ToString())
                errorDescription

    let dotModList =
        seq {
            for m in modList do
                match m with
                | DOTMOD (dm) -> yield dm
                | _ -> ()
        }
        |> List.ofSeq

    match dotModList with
    | [] -> sliceType
    | [ dm ] ->
        // if it's a dot mod, it must be on a 'g' part (gYNG2) (For now...)
        if sliceType <> StandardSlice.Gene
        then failwithf "cannot apply DOTMODS to non-gene parts (must be gXYZ). Used %c" prefix

        match dm with
        | "up" -> StandardSlice.Upstream
        | "down" -> StandardSlice.Downstream
        | "mrna" -> StandardSlice.MRNA
        | x -> failwithf "unimplemented DOTMOD %s. Options: up, down, mrna" x
    | _ -> failwithf "multiple DOTMODS applied to %s" errorDescription


/// Get the reference genome for a given assembly and set of pragmas.
/// Exception on error.
let getRG (a: Assembly) (rgs: GenomeDefinitions) (pr: PragmaCollection) =
    // Prefer reference genome from passed-in pragmas over assembly.
    let prags = [ pr; a.pragmas ]

    match GenomeDefinitions.getReferenceGenome rgs prags with
    | Ok (g, _) -> g
    | Bad msgs -> failwith msgs.[0]

/// Take a genepart and slices and get the actual DNA sequence.
let realizeSequence verbose (pragmas: PragmaCollection) fwd (rg: GenomeDefinition) (gp: GenePartWithLinker) =

    if verbose
    then printf "realizeSequence:  fetch fwd=%s %s\n" (if fwd then "y" else "n") gp.part.gene

    // Inspect prefix of gene e.g g,t,o,p and see what type of gene part we are starting with
    // Description of part to give in case of error
    let errorDesc = gp.part.gene

    let genePart =
        lookupGenePart errorDesc (gp.part.gene.[0]) (gp.part.mods)

    // Lookup gene location
    let feat = rg |> GenomeDefinition.getFeature gp.part.gene.[1..]

    // Come up with an initial slice based on the gene prefix type
    let s = translateGenePrefix pragmas rg genePart

    let finalSlice = ApplySlices.applySlices verbose gp.part.mods s
    let left = adjustToPhysical feat finalSlice.left
    let right = adjustToPhysical feat finalSlice.right

    // One final adjustment needed.  We have defined left and right relative to
    // the gene, but if the gene is on the crick strand, we need to both flip
    // the coordinates and reverse complement the resulting DNA to keep it
    // oriented with the construction orientation.
    if (left > right && feat.fwd)
       || (right > left && (not feat.fwd)) then
        failwithf "[realizeSequence] slice results in negatively lengthed DNA piece gene=%s slice=%s\n" feat.gene
            (printSlice finalSlice)

    let left', right' =
        if feat.fwd then left, right else right, left

    rg
    |> GenomeDefinition.getDna (errorDesc, sprintf "%d" feat.chr, left', right')
    |> DnaOps.revCompIf (not feat.fwd)
    |> DnaOps.revCompIf (not fwd)


/// Extract slice name from a PPP, if it has one.
let getSliceName (ppp: PPP) =
    ppp.pr
    |> PragmaCollection.tryGetValue BuiltIn.namePragmaDef
    |> Option.defaultValue ""


/// Extract URI from a PPP, if it has one.
let getUri (ppp: PPP) =
    ppp.pr
    |> PragmaCollection.tryGetValue BuiltIn.uriPragmaDef

let expandInlineDna dnaSource (ppp: PPP) (dnaFwd: Dna) =

    let dna = dnaFwd |> DnaOps.revCompIf (not ppp.fwd)

    { Id = None
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

let expandGenePart verbose
                   (rgs: GenomeDefinitions)
                   (library: SequenceLibrary)
                   (a: Assembly)
                   specifiedDnaSource
                   (ppp: PPP)
                   (gp: GenePartWithLinker)
                   =

    match gp.linker with
    | None -> () // No linkers were present
    | Some (l) -> Ryse.checkLinker l // Test the linkers

    // If the dna source is empty, then we are going to pull the DNA
    // part from the default reference genome, so we should make the
    // dnaSource field reflect this
    let dnaSource =
        if specifiedDnaSource = "" then Default.RefGenome else specifiedDnaSource


    // Check the genes are legal
    //let prefix = gp.part.gene.[0]
    let g = gp.part.gene.[1..].ToUpper()
    let rg' = getRG a rgs ppp.pr

    if not (rg' |> GenomeDefinition.isValidFeature g) then
        // Not a genomic reference but might still be in our library
        if library.ContainsKey(g) then
            // Yes! =- make up a little island of sequence for it
            let dna = library.[g]

            // Need to adjust for any slicing carefully since the DNA island is small
            // Validate mods to gene
            let errorRef =
                match a.name with
                | None -> sprintf "%A" a
                | Some (x) -> x

            validateMods errorRef gp.part.where gp.part.mods

            // Come up with an initial slice based on the gene prefix type

            // Get standard slice range for a gene
            let s = translateGenePrefix a.pragmas rg' StandardSlice.Gene
            let finalSlice = ApplySlices.applySlices verbose gp.part.mods s

            // Ban approx slices to stay sane for now
            if finalSlice.lApprox || finalSlice.rApprox
            then failwithf "sorry, approximate slices of library genes not supported yet in %A\n"
                     (prettyPrintAssembly a)

            let sliceContext = Library(gp.part.gene)

            let x, y =
                getBoundsFromSlice finalSlice dna.Length sliceContext
                |> returnOrFail

            let finalDNA =
                dna.[(x / 1<OneOffset>) - 1..(y / 1<OneOffset>) - 1]
                |> DnaOps.revCompIf (not ppp.fwd)

            let orfAnnotation =
                OrfAnnotation.orfAnnotationFromSlice finalSlice finalDNA.Length ppp.fwd sliceContext

            let name1 =
                if gp.part.mods.Length = 0 then gp.part.gene else (gp.part.gene + (printSlice finalSlice))

            let name2 = if ppp.fwd then name1 else "!" + name1

            { Id = None
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
            failwithf "undefined gene '%s' %O\n" g gp.part.where
    else
        // Inspect prefix of gene e.g g,t,o,p and see what type of gene part we are starting with
        let errorDesc = gp.part.gene

        let genePart =
            lookupGenePart errorDesc (gp.part.gene.[0]) (gp.part.mods)
        // Lookup gene location
        let rg' = getRG a rgs ppp.pr

        let feat = rg' |> GenomeDefinition.getFeature g

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
            match a.name with
            | None -> sprintf "%A" a
            | Some (x) -> x

        validateMods errorRef gp.part.where gp.part.mods
        // Come up with an initial slice based on the gene prefix type
        let s =
            translateGenePrefix a.pragmas rg' genePart

        if verbose then printf "log: processing %A\n" a

        // finalSlice is the consolidated gene relative coordinate of desired piece
        let finalSlice = ApplySlices.applySlices verbose gp.part.mods s

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

            { lApprox = finalSlice.lApprox
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
            adjustToPhysical feat finalSliceWithApprox.left
        /// threePrime is the genomic end of the element
        let threePrime =
            adjustToPhysical feat finalSliceWithApprox.right

        assert ((feat.fwd && fivePrime <= threePrime)
                || (not feat.fwd && fivePrime >= threePrime))

        if verbose then
            printf "log: gene: %s %d %d %s\n" feat.gene feat.l feat.r (if feat.fwd then "fwd" else "rev")
            printf "log: prefinal: %s %A %A\n" feat.gene fivePrime threePrime

        // One final adjustment needed.  We have defined left and right relative
        // to the gene, but if the gene is on the crick strand, we need to both
        // flip the coordinates and reverse complement the resulting DNA to keep
        // it oriented with the construction orientation.

        if (fivePrime > threePrime && feat.fwd)
           || (threePrime > fivePrime && (not feat.fwd)) then
            failwithf "slice results in negatively lengthed DNA piece for %s\n" (gp.part.gene + (printSlice finalSlice))

        /// left' is the genomic coordinate of the genomic left
        let left', right' =
            if feat.fwd then fivePrime, threePrime else threePrime, fivePrime

        if verbose
        then printf "log: final: %s %A %A\n" feat.gene left' right'

        assert (left' <= right')

        // TOO BLUNT gp.part.gene = "gURA3"
        // TODO: hard coded detection of split marker
        let isMarker = false
        let rg' = getRG a rgs ppp.pr

        if verbose
        then printf "gettingdna for %s fwd=%s\n" feat.gene (if ppp.fwd then "y" else "n")

        let dna =
            rg'
            |> GenomeDefinition.getDna (errorDesc, sprintf "%d" feat.chr, left', right')
            |> DnaOps.revCompIf (not feat.fwd)
            // One potential final flip if user wants DNA backwards
            |> DnaOps.revCompIf (not ppp.fwd)

        let description1 =
            match gp.part.mods with
            | [] -> gp.part.gene
            | [ DOTMOD (d) ] ->
                match d with
                | "up" -> "u" + gp.part.gene.[1..]
                | "down" -> "d" + gp.part.gene.[1..]
                | "mrna" -> "m" + gp.part.gene.[1..]
                | x -> failwithf "unimplemented DOTMOD %s" x
            | _ -> "g" + gp.part.gene.[1..] + (printSlice finalSlice)

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
            OrfAnnotation.orfAnnotationFromSlice finalSlice feat.Length ppp.fwd Genomic

        // Note regarding orientation: We are currently building a single piece
        // of final DNA left to right. There is no consideration for stitch
        // orientation, so even (in RYSEworld) B stitch parts are laid out left
        // to right from marker (innermost 9 linker) through to the outler linker.
        // If something is reversed then, it points towards the middle, marker
        // part of the stitch.
        { Id = None
          ExternalId = None
          SliceName = getSliceName ppp
          Uri = getUri ppp
          Dna = dna
          SourceChromosome = feat.chr |> string
          /// NB: sourceFr is the origin of the left hand end of the placed part in final part orientation
          SourceFrom = left'
          /// NB: sourceTo is the origin of the right hand end of the placed part in final part orientation
          SourceTo = right'
          SourceForward = feat.fwd
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
    | Some pragma -> pragma.Arguments |> Topology.parse |> returnOrFail
    | None -> Linear

/// Take a parsed assembly definition and translate it
/// to underlying DNA pieces, checking the structure in the process.
/// Raises an exception on error.
let expandAssembly (verbose: bool)
                   (markerProviders: IMarkerProvider list)
                   (rgs: GenomeDefinitions)
                   (library: SequenceLibrary)
                   (index: int)
                   (a: Assembly)
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
                            match a.pragmas
                                  |> PragmaCollection.tryGetValue BuiltIn.refGenomePragmaDef with
                            | Some (rg) -> rg
                            | None -> "" // Revert to the current default part origin

                match ppp.part with
                | MARKERPART ->
                    // Choose provider
                    let providers =
                        markerProviders
                        |> List.choose (fun provider ->
                            match provider.ScoreJob a.capabilities with
                            | None -> None
                            | Some (score) -> Some(score, provider))

                    if providers = []
                    then failwithf "MARKERPART substitution, no marker provider available"

                    let _, markerProvider = providers |> List.maxBy (fst)

                    let markerSet =
                        match a.pragmas
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
                | GENEPART gp -> yield expandGenePart verbose rgs library a dnaSource ppp gp
                //
                // Might also want to yield a fusion slice
                //
                if ppp.pr
                   |> PragmaCollection.contains BuiltIn.fusePragmaDef then
                    yield DnaAssembly.fusionSliceConstant
        }
        |> List.ofSeq
        |> DNASlice.recalculatOffset

    let materializedParts = expandPPPList a.parts

    let assemblyName =
        match a.name with
        | None -> sprintf "A%d" index
        | Some (s) -> s

    let topology = a.pragmas |> determineTopology

    { Id = Some index
      DnaParts = materializedParts
      Name = assemblyName
      Uri = a.uri
      LinkerHint = a.linkerHint
      Pragmas = a.pragmas
      DesignParams = a.designParams
      DocStrings = a.docStrings
      MaterializedFrom = a
      Tags = Set.empty
      Topology = topology }
