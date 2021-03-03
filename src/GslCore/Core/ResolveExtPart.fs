namespace GslCore.Core.ResolveExtPart

open FsToolkit.ErrorHandling
open GslCore.Ast.ErrorHandling
open GslCore.Core.Types
open GslCore.GslResult
open GslCore.Pragma
open GslCore.Legacy.Types
open GslCore.Legacy
open GslCore.Core.Ryse
open GslCore.Core
open GslCore.Constants
open Amyris.Dna

type ExtFetchSeq =
    { Id: string
      Dna: Dna
      Source: string
      Name: string }

[<RequireQualifiedAccess>]
type ExternalPartResolutionError =    
    | UnresolvableReference of partId: string
    | UnprocessableRabitModifiers of partId: LegacyPartId
    | UnimplementedExternalPartSpace of partSpace: string

module ResolveExtPart =
    let legalPrefixes = [ ("r", "rabit"); ("b", "biobrick") ]

    /// Does this part id start with a legal external part prefix
    let legalPartPrefix (pid: string) =
        let pidLower = pid.ToLower()

        let rec checkPrefix (prefs: (string * string) list) =
            match prefs with
            | [] -> None
            | (tag, name) :: _ when pidLower.StartsWith(tag) -> Some(name, pid.[tag.Length..])
            | _ :: tl -> checkPrefix tl

        checkPrefix legalPrefixes

    let fetchSequence (verbose: bool)
                      (library: SequenceLibrary)
                      (ppp: PartPlusPragma)
                      (partId: LegacyPartId)
                      : GslResult<DNASlice, ExternalPartResolutionError> =
        // Sequence can come either from the libary or preferably from the hutch directly
        let pid = partId.Id

        let sliceName =
            match ppp.Pragma
                  |> PragmaCollection.tryGetValue BuiltIn.namePragmaDef with
            | Some (name) -> name
            | None -> ""

        let uri =
            ppp.Pragma
            |> PragmaCollection.tryGetValue BuiltIn.uriPragmaDef

        match legalPartPrefix pid with
        | None -> GslResult.err (ExternalPartResolutionError.UnresolvableReference pid)
        | Some (partSpace, _) ->
            match partSpace with
            | "rabit" ->
                let libName = "@" + pid.ToUpper()

                if not (library.ContainsKey(libName)) then
                    let hr = Ryse.getRabit (int (pid.[1..]))

                    // Have part from the hutch.  We might just use it verbatim or we might be
                    // some modifications to it to make a new part
                    let rabit = hr.RabitSpecs.[0]

                    let dna =
                        Dna(rabit.DnaElementSpecs.[0].DnaSequence)

                    // Check for slice modifications.  We can't handle any other type of mod at this point, so
                    // ensure there are none.
                    if partId.Modifiers
                       |> List.exists (fun m ->
                           match m with
                           | Modifier.Slice _ -> false
                           | _ -> true) then
                        GslResult.err (ExternalPartResolutionError.UnprocessableRabitModifiers partId)

                    // Look for simple case.  If we are just using the part from the hutch unadulterated, then
                    // we specify things differently, referring to the external id
                    else if partId.Modifiers.Length = 0 then
                        let dna =
                            if ppp.IsForward then dna else dna.RevComp()

                        { DNASlice.Id = None
                          ExternalId = Some(pid.[1..])
                          SliceName = sliceName
                          Uri = uri // TODO: use the URI of rabit from hutch here instead?
                          Dna = dna
                          SourceChromosome = "library"
                          SourceFrom = 0<ZeroOffset>
                          SourceTo = (dna.Length - 1) * 1<ZeroOffset>
                          SourceForward = ppp.IsForward
                          SourceFromApprox = false
                          SourceToApprox = false
                          // Don't assign coordinates to pieces until later when we decide
                          // how they are getting joined up
                          DestinationFrom = 0<ZeroOffset>
                          DestinationTo = 0<ZeroOffset>
                          DestinationForward = ppp.IsForward
                          Description = rabit.Name
                          Type = SliceType.Regular
                          IsAmplified = false
                          Template = Some dna // not amplifying from this
                          DnaSource =
                              match ppp.Pragma
                                    |> PragmaCollection.tryGetValue BuiltIn.dnaSrcPragmaDef with
                              | Some (d) -> d
                              | None -> pid
                          Pragmas = ppp.Pragma
                          Breed = Breed.X // will be replaced at final submission
                          MaterializedFrom = Some(ppp)
                          Annotations = [] }
                        |> GslResult.ok
                    else
                        // Otherwise, they are taking a hutch part and doing something to it,
                        // so the hutch is just another DNA source and they are effectively
                        // building a new rabit

                        // Start off assuming it's the full DNA slice
                        let startSlice =
                            { Left =
                                  { Position = 1<OneOffset>
                                    RelativeTo = FivePrime }
                              LeftApprox = false
                              RightApprox = false
                              Right =
                                  { Position = -1<OneOffset>
                                    RelativeTo = ThreePrime } }

                        // Apply the slice(s) to get a final coordinate range
                        let finalSlice =
                            ApplySlices.applySlices verbose partId.Modifiers startSlice

                        // Find the left and right hand ends of the slice
                        let x, y =
                            LegacySliceContext.getBoundsFromSlice finalSlice dna.Length (Library(partId.Id))
                            |> Result.valueOr failwith

                        let finalDNA =
                            dna.[(x / 1<OneOffset>) - 1..(y / 1<OneOffset>) - 1]
                            |> DnaOps.revCompIf (not ppp.IsForward)

                        let name1 =
                            if partId.Modifiers.Length = 0 then
                                rabit.Name
                            else
                                (rabit.Name + (LegacyPrettyPrint.slice finalSlice))

                        let name2 =
                            if ppp.IsForward then name1 else "!" + name1

                        { Id = None
                          ExternalId = None
                          SliceName = sliceName
                          Uri = uri // TODO: use URI from hutch part?  mint new URI?
                          Dna = finalDNA
                          IsAmplified = false
                          Template = Some finalDNA
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
                          // Don't assign coordinates to pieces until later when we decide how they are getting joined up
                          DestinationFrom = 0<ZeroOffset>
                          DestinationTo = 0<ZeroOffset>
                          DestinationForward = ppp.IsForward
                          Description = name2
                          Type = SliceType.Regular
                          DnaSource =
                              match ppp.Pragma
                                    |> PragmaCollection.tryGetValue BuiltIn.dnaSrcPragmaDef with
                              | Some (d) -> d
                              | None -> pid
                          Pragmas = ppp.Pragma
                          Breed = Breed.X // they are hacking rabit, all bets are off
                          MaterializedFrom = Some(ppp)
                          Annotations = [] }
                        |> GslResult.ok
                else
                    // Part is in the library
                    let dna = library.[libName]

                    { Id = None
                      ExternalId = Some(pid.[1..])
                      SliceName = sliceName
                      Uri = uri // TODO: mint new URI if None?
                      Dna = dna
                      Template = Some dna
                      IsAmplified = false
                      SourceChromosome = "library"
                      SourceFrom = 0<ZeroOffset>
                      SourceTo = (dna.Length - 1) * 1<ZeroOffset>
                      SourceForward = true
                      SourceFromApprox = false
                      SourceToApprox = false
                      // Don't assign coordinates to pieces until later when we decide
                      // how they are getting joined up
                      DestinationFrom = 0<ZeroOffset>
                      DestinationTo = 0<ZeroOffset>
                      DestinationForward = ppp.IsForward
                      Description = libName
                      Type = SliceType.Regular
                      DnaSource = "library"
                      Pragmas = ppp.Pragma
                      Breed = Breed.X
                      MaterializedFrom = Some(ppp)
                      Annotations = [] }
                    |> GslResult.ok

            | x -> GslResult.err (ExternalPartResolutionError.UnimplementedExternalPartSpace x)


    /// Get the full part sequence for this external reference, don't apply any slice mods to it
    let fetchFullPartSequence (_ (* verbose*) : bool) (library: SequenceLibrary) (partId: LegacyPartId) =
        // Sequence can come either from the libary or preferably from the hutch directly
        let pid = partId.Id

        match legalPartPrefix pid with
        | None ->
            Error(sprintf "ERROR: partId reference %s isn't a defined alias and doesn't start with r for rabit\n" pid)
        | Some (partSpace, _) ->
            match partSpace with
            | "rabit" ->
                let libName = "@" + pid.ToUpper()

                if not (library.ContainsKey(libName)) then
                    let hr = Ryse.getRabit (int (pid.[1..]))

                    // Have part from the hutch.  We might just use it verbatim or we might be
                    // some modifications to it to make a new part
                    let rabit = hr.RabitSpecs.[0]

                    let dna =
                        Dna(rabit.DnaElementSpecs.[0].DnaSequence)

                    Ok
                        { Dna = dna
                          Source = "hutch"
                          Id = pid
                          Name = rabit.Name }
                else
                    // Part is in the library
                    Ok
                        { Dna = library.[libName]
                          Source = "library"
                          Id = pid
                          Name = libName }
            | x -> failwithf "ERROR: unimplemented external partSpace %s\n" x

    let getExtPartSlice (verbose: bool) (partId: LegacyPartId) =
        // Start off assuming it's the full DNA slice
        let startSlice =
            { Left =
                  { Position = 1<OneOffset>
                    RelativeTo = FivePrime }
              LeftApprox = false
              RightApprox = false
              Right =
                  { Position = -1<OneOffset>
                    RelativeTo = ThreePrime } }

        // Apply the slice(s) to get a final coordinate range
        let finalSlice =
            ApplySlices.applySlices verbose partId.Modifiers startSlice

        finalSlice

    let applySliceToExtSequence (_ (* verbose*) : bool)
                                (extPart: ExtFetchSeq)
                                (pr: PragmaCollection)
                                (fwd: bool)
                                (partId: LegacyPartId)
                                (finalSlice: Slice)
                                =
        let sliceName =
            match pr
                  |> PragmaCollection.tryGetValue BuiltIn.namePragmaDef with
            | Some (n) -> n
            | None -> ""

        let uri =
            pr
            |> PragmaCollection.tryGetValue BuiltIn.uriPragmaDef

        if partId.Modifiers.Length = 0 then
            let dna =
                extPart.Dna |> DnaOps.revCompIf (not fwd)

            { Id = None
              ExternalId = Some(extPart.Id.[1..])
              SliceName = sliceName
              Uri = uri // TODO: mint new URI if None?
              Dna = dna
              Template = None
              IsAmplified = false
              SourceChromosome = extPart.Source
              SourceFrom = 0<ZeroOffset>
              SourceTo = (extPart.Dna.Length - 1) * 1<ZeroOffset>
              SourceForward = fwd
              SourceFromApprox = false
              SourceToApprox = false
              // Don't assign coordinates to pieces until later when we decide how they are getting joined up
              DestinationFrom = 0<ZeroOffset>
              DestinationTo = 0<ZeroOffset>
              DestinationForward = fwd
              Description = extPart.Name
              Type = SliceType.Regular
              DnaSource =
                  pr
                  |> PragmaCollection.tryGetValue BuiltIn.dnaSrcPragmaDef
                  |> Option.defaultValue extPart.Id

              Pragmas = pr
              Breed = Breed.X
              MaterializedFrom = None
              Annotations = [] }
        else
            // Otherwise, they are taking a hutch part and doing something to it, so the hutch is just another
            // DNA source and they are effectively building a new rabit
            // Find the left and right hand ends of the slice
            let x, y =
                LegacySliceContext.getBoundsFromSlice finalSlice extPart.Dna.Length (Library(extPart.Id))
                |> Result.valueOr failwith

            let finalDNA =
                extPart.Dna.[(x / 1<OneOffset>) - 1..(y / 1<OneOffset>) - 1]
                |> DnaOps.revCompIf (not fwd)

            let name1 =
                if partId.Modifiers.Length = 0 then
                    extPart.Name
                else
                    (extPart.Name
                     + (LegacyPrettyPrint.slice finalSlice))

            let name2 = if fwd then name1 else "!" + name1

            { Id = None
              ExternalId = None
              SliceName = sliceName
              Uri = uri // TODO: mint new URI if None?
              Dna = finalDNA
              Template = Some finalDNA
              IsAmplified = true
              SourceChromosome = extPart.Source
              SourceFrom =
                  (finalSlice.Left.Position / (1<OneOffset>) - 1)
                  * 1<ZeroOffset>
              SourceTo =
                  (finalSlice.Right.Position / (1<OneOffset>) - 1)
                  * 1<ZeroOffset>
              SourceForward = true
              SourceFromApprox = false
              SourceToApprox = false
              // Don't assign coordinates to pieces until later when we decide how they are getting joined up
              DestinationFrom = 0<ZeroOffset>
              DestinationTo = 0<ZeroOffset>
              DestinationForward = fwd
              Description = name2
              Type = SliceType.Regular
              DnaSource =
                  pr
                  |> PragmaCollection.tryGetValue BuiltIn.dnaSrcPragmaDef
                  |> Option.defaultValue extPart.Id
              Pragmas = pr
              Breed = Breed.X
              MaterializedFrom = None
              Annotations = [] }
