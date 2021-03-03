namespace GslCore.Core.DnaCreation

open System

open FsToolkit.ErrorHandling
open GslCore.Ast.Types
open GslCore.Constants
open GslCore.Legacy.Types
open GslCore.Legacy
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

[<RequireQualifiedAccess>]
type ModifierValidationError =
    | SliceCoordinateCollision of slice: Slice * positions: SourcePosition list

[<RequireQualifiedAccess>]
type GenePartExpansionError =
    | ModifierValidation of ModifierValidationError
    
    | UnknownGene of gene: string * positions: SourcePosition list
    | UnsupportedApproximateSlices
    | SliceWithNegativeLength of slice: Slice * genePart: GenePart
    | UnsupportedDotModifier of modifier: string


module GenePartExpansionError =
    let makeModifierValidation (err: ModifierValidationError) =
        GenePartExpansionError.ModifierValidation err

[<RequireQualifiedAccess>]
type MarkerPartExpansionError =
    | NoProvidersProvided
    | UnknownMarkerSet of providedMarker: string * markers: string list

[<RequireQualifiedAccess>]
type PartExpansionError =
    | GenePart of err: GenePartExpansionError
    | MarkerPart of err: MarkerPartExpansionError
    | UnexpandedHeterologyBlock
    | UnexpandedInlineProteinSequence
    | ExternalPartResolution of ExternalPartResolutionError

[<RequireQualifiedAccess>]
type AssemblyExpansionError = PartExpansion of assembly: Assembly * PartExpansionError

module AssemblyExpansionError =
    let makePartExpansion assembly err =
        AssemblyExpansionError.PartExpansion(assembly, err)

module DnaCreation =
    // ================================================================================================
    // Slice manipulation routines for getting from gene notation down to specific genomics coordinates
    // ================================================================================================
    let private validateModifiers (where: SourcePosition list)
                                  (modifiers: Modifier list)
                                  : GslResult<unit, ModifierValidationError> =
        [ for modifier in modifiers do
            match modifier with
            | Modifier.Slice slice ->
                if slice.Left.RelativeTo = slice.Right.RelativeTo
                   && slice.Left.Position > slice.Right.Position then
                    GslResult.err (ModifierValidationError.SliceCoordinateCollision(slice, where))
                else
                    GslResult.ok ()
            | _ -> GslResult.ok () ]
        |> GslResult.collectA
        |> GslResult.ignore

    /// Given a feature and a relative position, find the zero based physical
    /// coordinate for the position.
    /// Determine range of DNA needed, translating into physical coordinates
    /// Final start of the piece.  Determine which end we are working relative to
    let private adjustToPhysical (feature: sgd.Feature) (relativePosition: RelativePosition): int<ZeroOffset> =
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
    let internal translateGenePrefix (pragmas: PragmaCollection)
                                     (genomeDefinition: GenomeDefinition)
                                     : StandardSlice -> Slice =
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

            { Slice.Left = leftPosition
              LeftApprox = true
              RightApprox = false
              Right = rightPosition }
        | StandardSlice.Upstream ->
            { Slice.Left =
                  { RelativePosition.Position = -(genomeDefinition |> GenomeDefinition.getFlank)
                    RelativeTo = FivePrime }
              LeftApprox = true
              RightApprox = false
              Right =
                  { RelativePosition.Position = -1<OneOffset>
                    RelativeTo = FivePrime } }
        | StandardSlice.Terminator ->
            { Slice.Left =
                  { RelativePosition.Position = 1<OneOffset>
                    RelativeTo = ThreePrime }
              LeftApprox = false
              RightApprox = true
              Right =
                  { RelativePosition.Position =
                        match pragmas
                              |> PragmaCollection.tryFind BuiltIn.termLenPragmaDef with
                        | None -> genomeDefinition |> GenomeDefinition.getTermLen
                        | Some p -> p.Arguments.[0] |> int |> (*) 1<OneOffset>
                    RelativeTo = ThreePrime } }
        | StandardSlice.Downstream ->
            { Slice.Left =
                  { RelativePosition.Position = 1<OneOffset>
                    RelativeTo = ThreePrime }
              LeftApprox = false
              RightApprox = true
              Right =
                  { RelativePosition.Position = genomeDefinition |> GenomeDefinition.getFlank
                    RelativeTo = ThreePrime } }
        | StandardSlice.FusableOrf ->
            { Slice.Left =
                  { RelativePosition.Position = 1<OneOffset>
                    RelativeTo = FivePrime }
              LeftApprox = false
              RightApprox = false
              Right =
                  { RelativePosition.Position = -4<OneOffset>
                    RelativeTo = ThreePrime } }
        | StandardSlice.Orf ->
            { Slice.Left =
                  { RelativePosition.Position = 1<OneOffset>
                    RelativeTo = FivePrime }
              LeftApprox = false
              RightApprox = false
              Right =
                  { RelativePosition.Position = -1<OneOffset>
                    RelativeTo = ThreePrime } }
        | StandardSlice.Gene ->
            { Slice.Left =
                  { RelativePosition.Position = 1<OneOffset>
                    RelativeTo = FivePrime }
              LeftApprox = false
              RightApprox = false
              Right =
                  { RelativePosition.Position = -1<OneOffset>
                    RelativeTo = ThreePrime } }
        | StandardSlice.MRNA ->
            { Slice.Left =
                  { RelativePosition.Position = 1<OneOffset>
                    RelativeTo = FivePrime }
              LeftApprox = false
              RightApprox = true
              Right =
                  { RelativePosition.Position =
                        match pragmas
                              |> PragmaCollection.tryFind BuiltIn.termLenMrnaPragmaDef with
                        | None ->
                            genomeDefinition
                            |> GenomeDefinition.getTermLenMRNA
                        | Some p -> p.Arguments.[0] |> int |> (*) 1<OneOffset>
                    RelativeTo = ThreePrime } }


    /// Translate gene part label.  Raises an exception for errors.
    let private lookupGenePart (errorDescription: string) (prefix: char) (modifierList: Modifier list): StandardSlice =
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
                | Modifier.Dot dotMod -> yield dotMod
                | _ -> () ]

        match dotModList with
        | [] -> sliceType
        | [ dotMod ] ->
            // if it's a dot mod, it must be on a 'g' part (gYNG2) (For now...)
            if sliceType <> StandardSlice.Gene then
                failwithf "cannot apply DOTMODS to non-gene parts (must be gXYZ). Used %c" prefix

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
        let pragmas = [ pragmas; assembly.Pragmas ]

        GenomeDefinitions.getReferenceGenome referenceGenomes pragmas
        |> Result.valueOr failwith

    /// Take a genepart and slices and get the actual DNA sequence.
    let realizeSequence (verbose: bool)
                        (pragmas: PragmaCollection)
                        (isForward: bool)
                        (referenceGenome: GenomeDefinition)
                        (genePartWithLinker: GenePartWithLinker)
                        : Dna =

        if verbose then
            printf "realizeSequence:  fetch fwd=%s %s\n" (if isForward then "y" else "n") genePartWithLinker.Part.Gene

        // Inspect prefix of gene e.g g,t,o,p and see what type of gene part we are starting with
        // Description of part to give in case of error
        let errorDesc = genePartWithLinker.Part.Gene

        let genePart =
            lookupGenePart errorDesc (genePartWithLinker.Part.Gene.[0]) (genePartWithLinker.Part.Modifiers)

        // Come up with an initial slice based on the gene prefix type

        let finalSlice =
            translateGenePrefix pragmas referenceGenome genePart
            |> ApplySlices.applySlices verbose genePartWithLinker.Part.Modifiers

        // Lookup gene location
        let feature =
            referenceGenome
            |> GenomeDefinition.getFeature genePartWithLinker.Part.Gene.[1..]

        let left = adjustToPhysical feature finalSlice.Left

        let right =
            adjustToPhysical feature finalSlice.Right

        // One final adjustment needed.  We have defined left and right relative to
        // the gene, but if the gene is on the crick strand, we need to both flip
        // the coordinates and reverse complement the resulting DNA to keep it
        // oriented with the construction orientation.
        if (left > right && feature.fwd)
           || (right > left && (not feature.fwd)) then
            failwithf "[realizeSequence] slice results in negatively lengthed DNA piece gene=%s slice=%s\n" feature.gene
                (LegacyPrettyPrint.slice finalSlice)

        let left', right' =
            if feature.fwd then left, right else right, left

        referenceGenome
        |> GenomeDefinition.getDna (errorDesc, sprintf "%d" feature.chr, left', right')
        |> DnaOps.revCompIf (not feature.fwd)
        |> DnaOps.revCompIf (not isForward)


    /// Extract slice name from a PPP, if it has one.
    let internal getSliceName (ppp: PartPlusPragma): string =
        ppp.Pragma
        |> PragmaCollection.tryGetValue BuiltIn.namePragmaDef
        |> Option.defaultValue ""


    /// Extract URI from a PPP, if it has one.
    let internal getUri (ppp: PartPlusPragma): string option =
        ppp.Pragma
        |> PragmaCollection.tryGetValue BuiltIn.uriPragmaDef

    let private expandInlineDna (dnaSource: string) (ppp: PartPlusPragma) (dnaFwd: Dna): DNASlice =

        let dna =
            dnaFwd |> DnaOps.revCompIf (not ppp.IsForward)

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
          DestinationForward = ppp.IsForward
          Description = (if ppp.IsForward then dnaFwd.str else "!" + dnaFwd.str)
          Type = SliceType.Inline
          DnaSource = dnaSource
          Pragmas = ppp.Pragma
          Breed = Breed.Inline
          MaterializedFrom = Some(ppp)
          Annotations = [] }

    let internal expandGenePart (verbose: bool)
                                (referenceGenomes: GenomeDefinitions)
                                (sequenceLookup: SequenceLibrary)
                                (assembly: Assembly)
                                (specifiedDnaSource: string)
                                (ppp: PartPlusPragma)
                                (genePartWithLinker: GenePartWithLinker)
                                : GslResult<DNASlice, GenePartExpansionError> =

        match genePartWithLinker.Linker with
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
            genePartWithLinker.Part.Gene.[1..].ToUpper()

        let referenceGenome =
            getReferenceGenome assembly referenceGenomes ppp.Pragma


        validateModifiers genePartWithLinker.Part.Where genePartWithLinker.Part.Modifiers
        |> GslResult.mapError GenePartExpansionError.ModifierValidation
        >>= fun _ ->
            if not
                (referenceGenome
                 |> GenomeDefinition.isValidFeature gene) then
                // Not a genomic reference but might still be in our library
                if sequenceLookup |> Map.containsKey gene then
                    // Yes! =- make up a little island of sequence for it
                    let dna = sequenceLookup.[gene]

                    // Come up with an initial slice based on the gene prefix type

                    // Get standard slice range for a gene

                    let finalSlice =
                        translateGenePrefix assembly.Pragmas referenceGenome StandardSlice.Gene
                        |> ApplySlices.applySlices verbose genePartWithLinker.Part.Modifiers

                    // Ban approx slices to stay sane for now
                    if finalSlice.LeftApprox || finalSlice.RightApprox then
                        GslResult.err GenePartExpansionError.UnsupportedApproximateSlices
                    else
                        let sliceContext = Library(genePartWithLinker.Part.Gene)

                        let x, y =
                            LegacySliceContext.getBoundsFromSlice finalSlice dna.Length sliceContext
                            |> Result.valueOr failwith

                        let finalDNA =
                            dna.[(x / 1<OneOffset>) - 1..(y / 1<OneOffset>) - 1]
                            |> DnaOps.revCompIf (not ppp.IsForward)

                        let orfAnnotation =
                            OrfAnnotation.orfAnnotationFromSlice finalSlice finalDNA.Length ppp.IsForward sliceContext

                        let name1 =
                            if genePartWithLinker.Part.Modifiers.Length = 0 then
                                genePartWithLinker.Part.Gene
                            else
                                (genePartWithLinker.Part.Gene
                                 + (LegacyPrettyPrint.slice finalSlice))

                        let name2 =
                            if ppp.IsForward then name1 else "!" + name1

                        { DNASlice.Id = None
                          ExternalId = None
                          SliceName = getSliceName ppp
                          Uri = getUri ppp // TODO: should we also check a returned library part for a URI?
                          Dna = finalDNA
                          SourceChromosome = "library"
                          SourceFrom =
                              (finalSlice.Left.Position / (1<OneOffset>) - 1)
                              * 1<ZeroOffset>
                          SourceTo =
                              (finalSlice.Right.Position / (1<OneOffset>) - 1)
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
                          DestinationForward = ppp.IsForward
                          Description = name2
                          Type = SliceType.Regular
                          DnaSource = dnaSource
                          Pragmas = ppp.Pragma
                          Breed = Breed.X
                          MaterializedFrom = Some(ppp)
                          Annotations = [ Orf(orfAnnotation) ] }
                        |> GslResult.ok
                else
                    GslResult.err (GenePartExpansionError.UnknownGene(gene, genePartWithLinker.Part.Where))
            else
                // Inspect prefix of gene e.g g,t,o,p and see what type of gene part we are starting with
                let errorDesc = genePartWithLinker.Part.Gene

                let genePart =
                    lookupGenePart errorDesc (genePartWithLinker.Part.Gene.[0]) (genePartWithLinker.Part.Modifiers)


                let feature =
                    referenceGenome
                    |> GenomeDefinition.getFeature gene

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
                // TODO: this common logic should be refactored into a generic function
                // and called in both places.

                // Validate mods to gene
                let errorRef =
                    assembly.Name
                    |> Option.defaultValue (sprintf "%A" assembly)

                if verbose then
                    printf "log: processing %A\n" assembly

                // finalSlice is the consolidated gene relative coordinate of desired piece
                let finalSlice =
                    translateGenePrefix assembly.Pragmas referenceGenome genePart
                    |> ApplySlices.applySlices verbose genePartWithLinker.Part.Modifiers

                // Gene relative coordinates for the gene slice we want
                let finalSliceWithApprox =
                    // Calculate some adjusted boundaries in case the left/right edges are approximate
                    let leftAdj =
                        { finalSlice.Left with
                              Position =
                                  finalSlice.Left.Position
                                  - (Default.ApproxMargin * 1<OneOffset>) }

                    let rightAdj =
                        { finalSlice.Right with
                              Position =
                                  finalSlice.Right.Position
                                  + (Default.ApproxMargin * 1<OneOffset>) }

                    { Slice.LeftApprox = finalSlice.LeftApprox
                      Left = (if finalSlice.LeftApprox then leftAdj else finalSlice.Left)
                      RightApprox = finalSlice.RightApprox
                      Right = if finalSlice.RightApprox then rightAdj else finalSlice.Right }

                if verbose then
                    printf "log: finalSlice: %s%s %s%s\n" (if finalSlice.LeftApprox then "~" else "")
                        (LegacyPrettyPrint.relativePosition finalSlice.Left)
                        (if finalSlice.RightApprox then "~" else "")
                        (LegacyPrettyPrint.relativePosition finalSlice.Right)

                    printf "log: finalSliceWA: %s%s %s%s\n"
                        (if finalSliceWithApprox.LeftApprox then
                            "~"
                         else
                             "") (LegacyPrettyPrint.relativePosition finalSliceWithApprox.Left)
                        (if finalSliceWithApprox.RightApprox then
                            "~"
                         else
                             "") (LegacyPrettyPrint.relativePosition finalSliceWithApprox.Right)

                // FinalSliceWithApprox is a gene relative coordinate system, but we
                // need genomic coordinates for the gene

                /// fivePrime is the genomic start of the element (can be > stop)
                let fivePrime =
                    adjustToPhysical feature finalSliceWithApprox.Left
                /// threePrime is the genomic end of the element
                let threePrime =
                    adjustToPhysical feature finalSliceWithApprox.Right

                assert ((feature.fwd && fivePrime <= threePrime)
                        || (not feature.fwd && fivePrime >= threePrime))

                if verbose then
                    printf "log: gene: %s %d %d %s\n" feature.gene feature.l feature.r
                        (if feature.fwd then "fwd" else "rev")

                    printf "log: prefinal: %s %A %A\n" feature.gene fivePrime threePrime

                // One final adjustment needed.  We have defined left and right relative
                // to the gene, but if the gene is on the crick strand, we need to both
                // flip the coordinates and reverse complement the resulting DNA to keep
                // it oriented with the construction orientation.

                if (fivePrime > threePrime && feature.fwd)
                   || (threePrime > fivePrime && (not feature.fwd)) then
                    GslResult.err (GenePartExpansionError.SliceWithNegativeLength(finalSlice, genePartWithLinker.Part))
                else

                    /// left' is the genomic coordinate of the genomic left
                    let left', right' =
                        if feature.fwd then fivePrime, threePrime else threePrime, fivePrime

                    if verbose then
                        printf "log: final: %s %A %A\n" feature.gene left' right'

                    assert (left' <= right')

                    // TOO BLUNT gp.part.gene = "gURA3"
                    // TODO: hard coded detection of split marker
                    let isMarker = false

                    if verbose then
                        printf "gettingdna for %s fwd=%s\n" feature.gene (if ppp.IsForward then "y" else "n")

                    let dna =
                        referenceGenome
                        |> GenomeDefinition.getDna (errorDesc, sprintf "%d" feature.chr, left', right')
                        |> DnaOps.revCompIf (not feature.fwd)
                        // One potential final flip if user wants DNA backwards
                        |> DnaOps.revCompIf (not ppp.IsForward)

                    (match genePartWithLinker.Part.Modifiers with
                     | [] -> genePartWithLinker.Part.Gene |> GslResult.ok
                     | [ Modifier.Dot (d) ] ->
                         match d with
                         | "up" ->
                             "u" + genePartWithLinker.Part.Gene.[1..]
                             |> GslResult.ok
                         | "down" ->
                             "d" + genePartWithLinker.Part.Gene.[1..]
                             |> GslResult.ok
                         | "mrna" ->
                             "m" + genePartWithLinker.Part.Gene.[1..]
                             |> GslResult.ok
                         | x -> GslResult.err (GenePartExpansionError.UnsupportedDotModifier x)
                     | _ ->
                         "g"
                         + genePartWithLinker.Part.Gene.[1..]
                         + (LegacyPrettyPrint.slice finalSlice)
                         |> GslResult.ok)
                    >>= fun description1 ->
                            let description2 =
                                if ppp.IsForward then description1 else "!" + description1

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

                                    if near z.Left termStart 1
                                       && near z.Right termEnd 100 then
                                        Breed.Terminator
                                    elif near z.Left promStart 400
                                         && near z.Right promEnd 40 then
                                        Breed.Promoter
                                    elif z.Left.Position = 1<OneOffset>
                                         && z.Left.RelativeTo = FivePrime
                                         && near z.Right termEnd 100 then
                                        Breed.GST
                                    else
                                        Breed.X
                                | x -> x

                            let orfAnnotation =
                                OrfAnnotation.orfAnnotationFromSlice finalSlice feature.Length ppp.IsForward Genomic

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
                              SourceFromApprox =
                                  (if ppp.IsForward then finalSlice.LeftApprox else finalSlice.RightApprox)
                              SourceToApprox = (if ppp.IsForward then finalSlice.RightApprox else finalSlice.LeftApprox)
                              DestinationFrom = 0<ZeroOffset>
                              DestinationTo = 0<ZeroOffset>
                              DestinationForward = ppp.IsForward
                              Description = description2
                              DnaSource = dnaSource
                              Type = (if isMarker then SliceType.Marker else SliceType.Regular)
                              Pragmas = ppp.Pragma
                              Breed = breed
                              MaterializedFrom = Some(ppp)
                              Annotations = [ Orf(orfAnnotation) ] }
                            |> GslResult.ok

    let private determineTopology (pragmas: PragmaCollection): Topology =
        match pragmas
              |> PragmaCollection.tryFind BuiltIn.topologyPragmaDef with
        | Some pragma ->
            pragma.Arguments
            |> Topology.parse
            |> GslResult.valueOr (fun messages -> messages |> String.concat ";" |> failwith)
        | None -> Linear

    let rec private expandPPP (verbose: bool)
                              (markerProviders: IMarkerProvider list)
                              (referenceGenomes: GenomeDefinitions)
                              (library: SequenceLibrary)
                              (assembly: Assembly)
                              (ppp: PartPlusPragma)
                              : GslResult<DNASlice list, PartExpansionError> =
        let dnaSource =
            ppp.Pragma
            |> PragmaCollection.tryGetValue BuiltIn.dnaSrcPragmaDef
            |> Option.defaultValue
                (ppp.Pragma
                 |> PragmaCollection.tryGetValue BuiltIn.refGenomePragmaDef
                 |> Option.defaultValue
                     (assembly.Pragmas
                      |> PragmaCollection.tryGetValue BuiltIn.refGenomePragmaDef
                      |> Option.defaultValue ""))

        let isFusionSliceNeeded =
            ppp.Pragma
            |> PragmaCollection.contains BuiltIn.fusePragmaDef

        [ match ppp.Part with
          | Part.MarkerPart ->
              // Choose provider
              let providers =
                  markerProviders
                  |> List.choose (fun provider ->
                      match provider.ScoreJob assembly.Capabilities with
                      | None -> None
                      | Some (score) -> Some(score, provider))

              if providers = [] then
                  GslResult.err MarkerPartExpansionError.NoProvidersProvided
                  |> GslResult.mapError PartExpansionError.MarkerPart
              else

                  let _, markerProvider = providers |> List.maxBy (fst)

                  let markerSetResult =
                      match assembly.Pragmas
                            |> PragmaCollection.tryGetValue BuiltIn.markersetPragmaDef with
                      | Some (x) ->
                          let x' = x.ToLower()

                          if not (markerProvider.IsLegal x') then
                              GslResult.err
                                  (MarkerPartExpansionError.UnknownMarkerSet(x', markerProvider.ListMarkers()))
                          else
                              x' |> GslResult.ok
                      | None -> "default" |> GslResult.ok


                  markerSetResult
                  |> GslResult.map (fun markerSet ->
                      let task =
                          { DnaSource = dnaSource
                            PartPlusPragma = ppp
                            MarkerSet = markerSet }

                      markerProvider.CreateDna(task))

                  |> GslResult.mapError PartExpansionError.MarkerPart // expandMarkerPart library dnaSource ppp
          | Part.PartId partId ->

              ResolveExtPart.fetchSequence verbose library ppp partId
              |> GslResult.mapError PartExpansionError.ExternalPartResolution
          | Part.InlineDna dna -> expandInlineDna dnaSource ppp dna |> GslResult.ok
          | Part.InlineProtein _ -> GslResult.err (PartExpansionError.UnexpandedInlineProteinSequence)
          | Part.HeterologyBlock -> GslResult.err (PartExpansionError.UnexpandedHeterologyBlock)
          | Part.SourceCode _ -> ()
          | Part.GenePart gp ->
              expandGenePart verbose referenceGenomes library assembly dnaSource ppp gp
              |> GslResult.mapError PartExpansionError.GenePart
          //
          // Might also want to yield a fusion slice
          //
          if isFusionSliceNeeded then
              DnaAssembly.fusionSliceConstant |> GslResult.ok ]
        |> GslResult.collectA

    /// Take a parsed assembly definition and translate it
    /// to underlying DNA pieces, checking the structure in the process.
    /// Raises an exception on error.
    let expandAssembly (verbose: bool)
                       (markerProviders: IMarkerProvider list)
                       (referenceGenomes: GenomeDefinitions)
                       (library: SequenceLibrary)
                       (index: int)
                       (assembly: Assembly)
                       : GslResult<DnaAssembly, AssemblyExpansionError> =

        assembly.Parts
        |> List.map (expandPPP verbose markerProviders referenceGenomes library assembly)
        |> GslResult.collectA
        |> GslResult.mapError (AssemblyExpansionError.makePartExpansion assembly)
        |> GslResult.map (List.collect id >> DNASlice.recalculatOffset)
        |> GslResult.map (fun materializedParts ->
            let assemblyName =
                assembly.Name
                |> Option.defaultValue (sprintf "A%d" index)

            let topology = assembly.Pragmas |> determineTopology

            { DnaAssembly.Id = Some index
              DnaParts = materializedParts
              Name = assemblyName
              Uri = assembly.Uri
              LinkerHint = assembly.LinkerHint
              Pragmas = assembly.Pragmas
              DesignParams = assembly.DesignParams
              DocStrings = assembly.DocStrings
              MaterializedFrom = assembly
              Tags = Set.empty
              Topology = topology })
