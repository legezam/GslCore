﻿module GslCore.Ryse

open System.IO
open System
open GslCore.Pragma
open GslCore.CommonTypes
open Amyris.Bio.utils
open Amyris.Dna

open System.Collections.Concurrent
open GslCore.Constants
open GslCore.Uri
open GslCore.SbolExample
open GslCore.AstTypes

// ==================================================================
// RYSE megastitch architecture
// ==================================================================

/// RYSE verbose flag
let private verbose = false

type HutchRabit =
    { id: int
      name: string
      five: string
      three: string
      orient: Orientation
      breed: string
      dnaSource: string }

/// Load name\tsequence text file of RYSE linkers and return a map
let loadRyseLinkers (f: string) =
    if not (File.Exists f)
    then failwithf "could not locate reference file '%s'\n" f

    eachLineIn f
    |> Seq.choose (fun l ->
        match l.Split([| ',' |]) with
        | [| "" |] -> None
        | [| n; s |] -> Some(n, { name = n; dna = Dna(s) })
        | _ -> failwithf "Bad linker specification: '%s'" l)
    |> Map.ofSeq

let extractLinker (s: string) =
    if s.StartsWith("Linker_")
    then s.[7..]
    else failwithf "ERROR: unable to parse linker name '%s'" s

let checkLinker (l: Linker) =
    if not (Default.ValidLinkers.Contains(l.l1))
    then failwithf "ERROR: linker %s not a legal linker" l.l1

    if not (Default.ValidLinkers.Contains(l.l2))
    then failwithf "ERROR: linker %s not a legal linker" l.l2


/// Get auxillary cached information about key rabits for thumper rabits
let loadThumperRef (f: string) =
    if not (File.Exists f)
    then failwithf "could not locate thumper reference file%s" f

    eachLineIn f
    |> Seq.skip 1
    |> Seq.map (tabSplit)
    |> Seq.choose (fun cols ->
        match cols with
        | [| id; name; five; three; orient; breed |] ->
            Some
                ({ id = int id
                   name = name
                   five = five
                   three = three
                   orient = if orient.[0] = '0' then FWD else REV
                   breed = breed
                   dnaSource = sprintf "R%s" id })
        | x ->
            printf "WARNING: bad hutch line %A\n" x
            None)
    |> Seq.map (fun hr -> (hr.id, hr))
    |> Map.ofSeq


let thumper = "http://thumper.amyris.local"

// FIXME: this cache is global and mutable and can become stale when GSLC is embedded in a long-
// running application.
let private fetchCache =
    ConcurrentDictionary<string, RycodExample.ThumperRycod.RyseComponentRequest>()

/// Global flag to activate or deactivate caching of part fetch.
/// Long-running clients of GSLC should set this flag to false to avoid building up a large cache
/// that can become stale over time.
let mutable useCache = true

/// Hutch interaction: fetch part defs from RYCOd service and cache them.
let fetch (url: string) =

    let lookup () =
        try
            use wc = new System.Net.WebClient()
            let s = wc.DownloadString(url)
            let res = RycodExample.ThumperRycod.Parse(s)
            res
        with ex -> failwithf "from thumper %s\nMight be rabit id that does not exist.\n%s\n" url ex.Message

    if useCache then
        match fetchCache.TryGetValue(url) with
        | (true, x) -> x
        | (false, _) ->
            let result = lookup ()
            fetchCache.TryAdd(url, result) |> ignore
            result
    else
        lookup ()

let getMS msId =
    sprintf "%s/rycod/megastitch_spec/%d" thumper msId
    |> fetch

let getStitch stitchId =
    sprintf "%s/rycod/stitch_spec/%d" thumper stitchId
    |> fetch

/// Get spec for rabit from hutch given rabit id
let getRabit rId =
    sprintf "%s/rycod/rabit_spec/%d" thumper rId
    |> fetch

/// Retrieve a Rabit specifiction from local cache or by making a thumper call.
let getHutchInfoViaWeb ri =
    let hr = getRabit ri
    let rabit = hr.RabitSpecs.[0]
    assert (rabit.Id.StartsWith("R."))

    { id = int (rabit.Id.[2..])
      name = rabit.Name
      five =
          (match rabit.UpstreamLink.String with
           | Some (x) -> x
           | None -> "")
      three =
          (match rabit.DownstreamLink.String with
           | None -> ""
           | Some (x) -> x)
      orient =
          match rabit.Direction with
          | "FWD" -> FWD
          | "REV" -> REV
          | _ -> failwithf "inconceivable direction %A\n" rabit.Direction
      breed = rabit.Breed
      dnaSource = sprintf "R%d" ri }

/// Determine which sets of linkers to use for a design
let getLinkerSetsForDesign (assembly: DnaAssembly): string list * string list =
    let defaultLinkers = [ "0"; "2"; "A"; "3"; "9" ]

    if assembly.linkerHint = "" then
        defaultLinkers, defaultLinkers
    else
        let splitOnComma (s: string) =
            s.Trim().Split([| ',' |])
            |> Array.map (fun s -> s.Trim())
            |> List.ofArray

        let validLinkerSet (s: string list) =
            s.Head = "0" && (List.rev s |> List.head = "9")

        let invalidLinkerWarn (a: string list) (b: string list): bool =
            let linkerWarnOff =
                assembly.pragmas
                |> PragmaCollection.tryFind BuiltIn.warnoffPragmaDef
                |> Option.map (Pragma.hasVal "zeronine")
                |> Option.defaultValue false

            not linkerWarnOff
            && (not (validLinkerSet a) || not (validLinkerSet b))

        let (altLinkers1, altLinkers2) =
            let linkerSets =
                assembly.linkerHint.Split([| '|' |])
                |> Array.map splitOnComma

            match linkerSets with
            | [| a; b |] ->
                if invalidLinkerWarn a b
                then printf "linker sets must start with linker 0 and end with linker 9.  %s fails\n"
                         assembly.linkerHint
                // FIXME: this error condition just prints a message, should it blow up?
                a, b
            | _ -> failwithf "bad #linkers structure, should be one part with | sep, not %s" (assembly.linkerHint)

        if verbose
        then printf "Using alternative linkers: %A,%A" altLinkers1 altLinkers2

        altLinkers1, altLinkers2

/// active pattern for picking out inline slice types
let (|InlineSlice|_|) =
    function
    | x when x.sliceType = INLINEST -> Some(x)
    | _ -> None

let (|RegularSlice|_|) =
    function
    | x when x.sliceType = REGULAR -> Some(x)
    | _ -> None

let (|FusionSlice|_|) =
    function
    | x when x.sliceType = FUSIONST -> Some(x)
    | _ -> None

/// Determine how many junctions will require RYSE linkers.
/// Inline dna segments won't for example unless they have rabitstart/end pragmas
let rec countRyseLinkersNeeded printVerbose total (l: DNASlice list) =
    match l with // REGULAR | MARKER | LINKER | INLINEST
    | [] ->
        printVerbose (sprintf "  countRyseLinkersNeeded:  done total=%d" total)
        total
    | a :: b :: tl when a.sliceType = FUSIONST ->
        printVerbose
            (sprintf
                "  countRyseLinkersNeeded:  +0 fusion slice, part=%s , fused to %s 0 linkers needed"
                 a.description
                 b.description)

        countRyseLinkersNeeded printVerbose total tl
    | a :: _ :: tl when a.sliceType = INLINEST
                        && (a.pragmas
                            |> PragmaCollection.contains BuiltIn.rabitStartPragmaDef
                            || a.pragmas
                               |> PragmaCollection.contains BuiltIn.rabitEndPragmaDef) ->
        printVerbose
            (sprintf
                "  countRyseLinkersNeeded:  +0 inline slice at start/end of rabit 0 more linkers needed for %s"
                 a.description)

        countRyseLinkersNeeded printVerbose (total + 1) tl // Add one more rabit if we need to insert a linker next to this inline slice
    | a :: b :: tl when a.sliceType = INLINEST && b.sliceType = REGULAR ->
        printVerbose
            (sprintf
                "  countRyseLinkersNeeded:  +0 inline slice before a regular slice, 0 more linkers needed for %s"
                 a.description)

        countRyseLinkersNeeded printVerbose total tl
    | a :: tl ->
        printVerbose (sprintf "  countRyseLinkersNeeded:  +1 basic case, part=%s" a.description)
        countRyseLinkersNeeded printVerbose (total + 1) tl


/// Assign ryse linkers to the design.
let mapRyseLinkers (opts: ParsedOptions)
                   (hutchAncillary: Map<int, HutchRabit>)
                   (ryseLinkers: Map<string, RYSELinker>)
                   (aIn: DnaAssembly)
                   =

    let printVerbose msg = if opts.Verbose then printfn "%s" msg

    printVerbose "ENTERING: mapRyseLinkers"
    /// If they are building just a stitch, we need to know not to look for the marker
    let megaMono =
        match PragmaCollection.assemblyMode aIn.pragmas with
        | Megastitch -> false
        | Stitch -> true

    // First establish which linker set we are using
    let (allLinkers1, allLinkers2) = getLinkerSetsForDesign aIn

    // Replace DNA parts with expanded version including linkers
    let dnaName =
        aIn.dnaParts
        |> List.map (fun d -> d.description)
        |> fun x -> String.Join(";", x)

    // An error description for user in the event problems happen
    let errorDesc =
        "linkers=["
        + String.Join(",", allLinkers1)
        + "]|["
        + String.Join(",", allLinkers2)
        + "]/"
        + dnaName

    /// Assign RYSE linkers to junctions that need them
    let rec assign startLinkers (phase: bool) (inputList: DNASlice list) (linkers: string list) res =
        let prepLinker (n: string) =
            let linker =
                match ryseLinkers.TryFind n with
                | Some x -> x
                | None -> failwithf "ERROR: unexpected error not found looking up linker '%s'" n
            // DNA for the linker
            let dna =
                linker.dna
                |> fun x -> if phase then x else x.RevComp()
            // Build the linker entry
            { id = None
              extId = None
              sliceName = ""
              uri = Some(linkerUri linker.name)
              dna = dna
              sourceChr = "linker"
              sourceFr = 0<ZeroOffset>
              sourceTo = 0<ZeroOffset>
              template = None
              amplified = false
              sourceFrApprox = false
              sourceToApprox = false
              destFr = -999<ZeroOffset>
              destTo = -999<ZeroOffset>
              sourceFwd = phase
              description = sprintf "Linker_%s" n
              sliceType = LINKER
              destFwd = phase
              dnaSource = ""
              pragmas = PragmaCollection.empty
              breed = B_LINKER
              materializedFrom = None
              annotations = [] }

        let noLinkersLeftMsg =
            sprintf "mapRyseLinkers: out of linkers.  Started with %A" startLinkers

        // Marker encountered.
        // lastParts are the parts that goes before the marker in reverse order
        // markerHd is the marker part
        // partsTl are the remaining parts in the assembly
        // let markerTransition nextLinker markerHd partsTl =
        let markerTransition (lastParts: DNASlice list) markerHd partsTl =
            // Reconstruct output with linker and moved piece/
            // If we hit the marker, flip orientation, restart linker list but backwards
            printVerbose
                "countRyseLinkersNeeded in phase II start with 1 for final leading 0 linker (note 9 linker not included in count)"

            let linkersReq =
                countRyseLinkersNeeded printVerbose 1 partsTl

            // check this first
            if linkersReq > allLinkers2.Length then
                failwithf "mapRyseLinkers - need %d linkers to finish, only %d available %A\n" linkersReq
                    allLinkers2.Length errorDesc

            printVerbose
                (sprintf "\n\n#############################################\npart 2 of megastitch - %d linkers required, using %A"
                     linkersReq (Seq.take linkersReq allLinkers2 |> List.ofSeq))

            // Flipping part around, but only for marker case.
            // Should this also happen for the regular parts?  Must be dealt with elsewhere ;(
            let hd' =
                { markerHd with
                      destFwd = if phase then markerHd.destFwd else not markerHd.destFwd }

            // phase set to false to denote second phase,
            // grab the second set of linkers allLinkers2
            assign
                allLinkers2
                false
                partsTl
                (Seq.take linkersReq allLinkers2
                 |> List.ofSeq
                 |> List.rev)
                (hd' :: lastParts @ res)

        // We *PRE* assign linkers to pieces so as we recognize a pattern, we
        // are emitting the linker that comes before (upstream in dna construct
        // orientation) the rabit part.  At the end we tack on the final '0'
        // linker for the second megastitch.  Note the pre assignment happens
        // the same way for the B stitch which is still constructed left to
        // right (relative to final megastitch construct), so we are reporting the
        // linker to the left of the B stitch elements (downstream of Rabit in
        // B stitch orientation)
        printVerbose
            (sprintf "\n\n==============================================\nin mapRyseLinkers(TOP):\nassign: sliceList=%A \nlinkers=%A\nresult=%A"
                 [| for i in inputList -> i.description |] linkers [| for r in res -> r.description |])


        let reportFinalAndReturn (res: DNASlice list) =
            // print out parts we are returning with linkers inserted
            // (reverse temporarily here so we show them the final order, but return unreversed)
            printVerbose
                (sprintf "\n\n==============================================\nin mapRyseLinkers:\nFinal output: sliceList=%A \n"
                     (res
                      |> List.rev
                      |> List.map (fun (x: DNASlice) -> x.description)))

            res

        // recursive match expression
        match inputList with
        // MRL-CASE 0
        | [] ->
            printVerbose "MRL-CASE 0"

            if megaMono && linkers.Length > 0 then
                let last = List.rev linkers |> List.head
                printVerbose (sprintf "in mapRyseLinkers:assign: end of megaMono, picking %A for last primer" last)
                (prepLinker last) :: res |> reportFinalAndReturn
            else
                if phase then // Must be in phase two by the time we get here
                    let alreadyAssigned =
                        res
                        |> List.rev
                        |> List.map (fun x -> x.description)
                        |> fun x -> String.Join(" ; ", x)

                    failwithf "in mapRyseLinkers:assign, ran ut of linkers while still in phase one :(  .  Are you missing a ### marker for your megastitch?\n\nAssigned:%s"
                        alreadyAssigned

                match linkers with
                | [ linkerName ] ->
                    printVerbose (sprintf "in mapRyseLinkers:assign, finishing on %A linker" linkerName)

                    (prepLinker linkerName) :: res
                    |> reportFinalAndReturn
                | x ->
                    failwithf "mapRyseLinkers: unexpected linker complement  '%s' left at end \nphase=%s (%s)\n"
                        (x.ToString()) (if phase then "phase1" else "phase2") errorDesc

        // MRL-CASE 1
        | InlineSlice a :: b :: c when (b.sliceType = REGULAR
                                        || b.sliceType = SliceType.INLINEST
                                        || b.sliceType = SliceType.MARKER)
                                       && a.pragmas
                                          |> PragmaCollection.contains BuiltIn.rabitStartPragmaDef ->
            printVerbose "MRL-CASE 1"
            // a in an inline type with a pragma telling us to initiate the
            // start of a rabit here, so it needs to be preceded by a linker in
            // the final part list
            //
            //     / inline {#rabitstart} /  ; regularRabit
            //     LINKER :: inline :: regularRabit ..
            printVerbose "in mapRyseLinkers: a is INLINEST with rabitstart"

            match linkers with
            | [] -> failwith noLinkersLeftMsg
            | linkerName :: lt ->
                let linker = prepLinker linkerName

                if b.sliceType = MARKER then
                    // a takes the place of a linker in this scenario
                    markerTransition [ a; linker ] b c
                else
                    printVerbose (sprintf "inlineST starting following rabit, assign linker %s" linkerName)
                    // In general if we just put a linker in front of an inline sequence, the following part
                    // (labeled b here) should come along for the ride with no additional linker in front of it
                    assign startLinkers phase c lt (b :: a :: linker :: res)

        // MRL-CASE 2
        | InlineSlice (a) :: b :: c when (b.sliceType = REGULAR
                                          || b.sliceType = MARKER
                                          || b.sliceType = INLINEST)
                                         && a.pragmas
                                            |> PragmaCollection.contains BuiltIn.rabitEndPragmaDef ->
            printVerbose "MRL-CASE 2"
            // a in an inline type with a pragma telling us to end a rabit here,
            // so it needs to be followed by a rabit in the final part list
            //
            //     / inline {#rabitend} /  ; regularRabit
            //     inline :: LINKER :: regularRabit ..
            match linkers with
            | [] -> failwith noLinkersLeftMsg
            | linkerName :: _ ->
                //let linker = prepLinker linkerName
                //
                // NB: we push B back into the work list, since we might
                // want to process is specially if it's a marker for example,
                // we can't simply push it to the output part list
                printVerbose
                    (sprintf "inlineST ending preceding rabit, assign linker %s, remaining %A" linkerName (b :: c))
                // fixed this - wasn't pushing b out
                assign startLinkers phase (b :: c) linkers (a :: res)

        // MRL-CASE 3
        | [ RegularSlice (a); InlineSlice (b) ] ->
            printVerbose "MRL-CASE 3"
            // a is regular and b inline - special terminal inline case
            //
            printVerbose "terminal regular::inlineST case, take one linker"

            match linkers with
            | [] -> failwith noLinkersLeftMsg
            | linkerName :: ltl ->
                printVerbose (sprintf "assign linker %s, no remaining parts" linkerName)
                let linker = prepLinker linkerName

                assign startLinkers phase [] ltl (b :: a :: linker :: res)

        //assign startLinkers phase c linkers (b::a::res)

        // MRL-CASE 4
        | InlineSlice (a) :: RegularSlice (b) :: c ->
            // a is an inline and b regular, so take b and a and move them to
            // the output
            printVerbose "MRL-CASE 4"
            printVerbose "inlineST no linker needed"
            assign startLinkers phase c linkers (b :: a :: res)

        // MRL-CASE 5
        | FusionSlice _ :: b :: InlineSlice (c) :: d when not
                                                              (c.pragmas
                                                               |> PragmaCollection.contains
                                                                   BuiltIn.rabitStartPragmaDef) ->
            printVerbose "MRL-CASE 5"
            printVerbose "fusionST followed by X and then inline that is not a rabit start"
            // Note: a not emitted - we are not passing FUSIONST through in this case
            assign startLinkers phase d linkers (c :: b :: res)

        // MRL-CASE 6
        | FusionSlice _ :: InlineSlice (b) :: c :: d when not
                                                              (b.pragmas
                                                               |> PragmaCollection.contains
                                                                   BuiltIn.rabitStartPragmaDef)
                                                          && not
                                                              (b.pragmas
                                                               |> PragmaCollection.contains
                                                                   BuiltIn.rabitEndPragmaDef) ->
            printVerbose "MRL-CASE 6"
            printVerbose "fusionST followed by inline that is not a rabit start"
            // No need for a linker before or after a fusion place holder, since
            // it doesn't really exist, but is a hint to join the two adjacent/
            // pieces.
            printVerbose "Fusion ST before inlinest no linker needed"

            // Note: a not emitted - we are not passing FUSIONST through in this case
            assign startLinkers phase d linkers (c :: b :: res) // Was emitting A previously daz

        // MRL-CASE 7
        | FusionSlice _ :: b :: c ->
            printVerbose "MRL-CASE 7"
            // No need for a linker before or after a fusion place holder, since
            // it doesn't really exist, but is a hint to join the two adjacent/
            // pieces.
            printVerbose "Fusion ST no linker needed"

            // Note: a not emitted - we are not passing FUSIONST through in this case
            assign startLinkers phase c linkers (b :: res) // Was emitting A previously daz

        // MRL-CASE 8
        | InlineSlice (a) :: InlineSlice (b) :: c ->
            printVerbose "MRL-CASE 8"
            // Double inline slice type.  Room for more logic here to potentially merge short slices but for now
            // just avoid dropping linkers into the middle of it all
            printVerbose "Double inline no linker needed"
            // a and b move to output list, c stays as the next input
            assign startLinkers phase c linkers (b :: a :: res)

        // MRL-CASE 9
        | InlineSlice (a) :: FusionSlice (b) :: c when (not
                                                            (a.pragmas
                                                             |> PragmaCollection.contains
                                                                 BuiltIn.rabitStartPragmaDef))
                                                       && (not
                                                               (a.pragmas
                                                                |> PragmaCollection.contains
                                                                    BuiltIn.rabitEndPragmaDef)) ->
            printVerbose "MRL-CASE 9"
            // inline then fuse slice type and no directive to end or start here.
            // avoid dropping linkers into the middle of it all
            printVerbose "inline then fuse no linker needed"
            // a moves to the output list
            // b and c stay on the stack of input slices to be processed later
            assign startLinkers phase (b :: c) linkers (a :: res)

        // MRL-CASE 10
        | [ InlineSlice (hd) ] when (hd.pragmas
                                     |> PragmaCollection.contains BuiltIn.rabitEndPragmaDef
                                     || hd.pragmas
                                        |> PragmaCollection.contains BuiltIn.inlinePragmaDef) ->
            printVerbose "MRL-CASE 10"
            printVerbose "terminal inline slice that will be made off final linker, just move it to output list"
            assign startLinkers phase [] linkers (hd :: res)

        // MRL-CASE 11
        | hd :: tl when (match res with
                         | InlineSlice (x) :: _ when x.pragmas
                                                     |> PragmaCollection.contains BuiltIn.rabitEndPragmaDef
                                                     |> not -> true
                         | _ -> false) ->
            printVerbose "MRL-CASE 11 - slice with preceding inline slice , don't drop linker"
            assign startLinkers phase tl linkers (hd :: res)

        // MRL-CASE 999
        | hd :: tl -> // General case, chomp one linker
            printVerbose "MRL-CASE 999"
            printVerbose "General case - assign a linker"
            sprintf "hd=%A" hd.description |> printVerbose

            sprintf "tl=%A" [ for x in tl -> x.description ]
            |> printVerbose

            match linkers with
            | [] -> failwith noLinkersLeftMsg
            | linkerName :: lt ->
                let linker = prepLinker linkerName
                printVerbose (sprintf "Assigning linker %s to %s/%s" linkerName hd.description (formatST hd.sliceType))

                // DETECT MARKER, transition to phase II
                if hd.sliceType = MARKER then
                    // enter marker transition with linker to go before the marker part, marker part and remaining parts to place
                    markerTransition [ linker ] hd tl
                else
                    // We are putting linker before the piece hd (output gets flipped at the end).
                    // Make sure linker is appropriate to precede part hd.
                    // Matters in the case where hd is reuse of a ryse part.
                    match hd.extId with
                    | Some (x) -> //when x.[0] = 'R' || x.[0] = 'r' ->
                        let rabitId = int (x)

                        let h =
                            if (hutchAncillary.ContainsKey(rabitId)) then
                                hutchAncillary.[rabitId]
                            else
                                getHutchInfoViaWeb rabitId

                        let hFive = sprintf "%s" h.five
                        let hThree = sprintf "%s" h.three

                        let linkerName = extractLinker linker.description

                        let linkerNameNext =
                            match lt with
                            | hd :: _ -> hd // (List.head lt)
                            | [] -> failwith noLinkersLeftMsg

                        let failWithLinkerErrorMsg whichEnd hEnd name =
                            failwithf "part R%d expects %s linker (%s) and linker (%s) used instead \nERROR:(%s)"
                                rabitId whichEnd hEnd name errorDesc

                        if phase then
                            if linkerName <> hFive
                            then failWithLinkerErrorMsg "5'" hFive linkerName

                            if linkerNameNext <> hThree
                            then failWithLinkerErrorMsg "3'" hThree linkerNameNext
                        else
                            if linkerName <> hThree
                            then failWithLinkerErrorMsg "3'" hThree linkerName

                            if linkerNameNext <> hFive
                            then failWithLinkerErrorMsg "5'" hFive linkerNameNext
                    | _ -> ()

                    assign startLinkers phase tl lt (hd :: linker :: res) // Reconstruct output with linker and moved piece

    let res =
        { aIn with
              dnaParts =
                  assign allLinkers1 true aIn.dnaParts allLinkers1 []
                  |> List.rev
                  |> recalcOffset }


    printVerbose "DONE:  mapRyseLinkers"
    res


// ==================================================================
// RYSE components in SBOL format
// ==================================================================

// --- static URIs ---

/// Return the URIs for linker ComponentDefintion and Sequence
let linkerUris linkCode =
    (unwrap (buildUri [ "Component"; "Linker" ] linkCode), unwrap (buildUri [ "ComponentSequence"; "Linker" ] linkCode))

/// Return the SBOL specification of a RYSE linker.
let sbolLinker (linker: RYSELinker) =
    let cdUri, seqUri = linkerUris linker.name

    { id =
          { identity = cdUri
            name = Some("RYSE linker " + linker.name)
            description = None }
      roles = [ ryseLinkerRoleUri ]
      sequence =
          Some
              ({ id =
                     { identity = seqUri
                       name = None
                       description = None }
                 elements = linker.dna.str })
      subcomponents = []
      gslProg = None }

type PrimerType =
    | Amplification
    | Quickchange

/// Create the SBOL objects for a primer.
let sbolPrimer (name: string) (tail: Dna) (body: Dna) (kind: PrimerType) =

    // make tail and body items
    let tailComp =
        { id =
              { identity = createTempUri ()
                name = Some(name + "_tail")
                description = None }
          roles = [ primerTailRoleUri ]
          sequence = Some(seqFromDna tail)
          subcomponents = []
          gslProg = None }

    let tailSubcomp =
        tailComp.asSubcomponent ([], [ primerTailRoleUri ])

    let bodyComp =
        { id =
              { identity = createTempUri ()
                name = Some(name + "_body")
                description = None }
          roles = [ primerBodyRoleUri ]
          sequence = Some(seqFromDna body)
          subcomponents = []
          gslProg = None }

    let bodySubcomp =
        bodyComp.asSubcomponent ([], [ primerBodyRoleUri ])

    let primerRole =
        match kind with
        | Amplification -> ampPrimerRoleUri
        | Quickchange -> quickchangePrimerRoleUri

    let fullComp =
        { id =
              { identity = createTempUri ()
                name = Some(name)
                description = None }
          roles = [ primerRole ]
          sequence = Some(seqFromDna (DnaOps.append tail body))
          subcomponents = [ tailSubcomp; bodySubcomp ]
          gslProg = None }


    (fullComp, [ fullComp; tailComp; bodyComp ])

/// Return the ComponentDefintion for a Rabit DNA element.
/// Primers are passed as two-tuples with the implicit
/// ordering (fwd/5', rev/3')
let sbolDnaElement (name: string)
                   (desc: string option)
                   (compUri: Uri option)
                   (dna: Dna)
                   (ampPrimers: ComponentDefinition * ComponentDefinition)
                   (quickchangePrimers: (ComponentDefinition * ComponentDefinition) option)
                   =

    let mutable subcomps =
        [ (fst ampPrimers)
            .asSubcomponent([], [ fivePrimePrimerRoleUri ])
          (snd ampPrimers)
              .asSubcomponent([], [ threePrimePrimerRoleUri ]) ]

    match quickchangePrimers with
    | Some (qc5p, qc3p) ->
        subcomps <-
            subcomps
            @ [ qc5p.asSubcomponent
                    ([],
                     [ fivePrimePrimerRoleUri
                       quickchangePrimerRoleUri ])
                qc3p.asSubcomponent
                    ([],
                     [ threePrimePrimerRoleUri
                       quickchangePrimerRoleUri ]) ]
    | None -> ()

    { id =
          { identity =
                (match compUri with
                 | Some (u) -> u
                 | None -> createTempUri ())
            name = Some(name)
            description = desc }
      roles = [ rabitDnaRoleUri ]
      sequence = Some(seqFromDna dna)
      subcomponents = subcomps
      gslProg = None }

/// Return the ComponentDefintion for a Rabit.
/// Linkers are passed as a two-tuple with implicit ordering (5', 3')
let sbolRabit (name: string)
              (desc: string)
              (compUri: Uri option)
              breed
              (orientation: Orientation)
              (dna: Dna)
              (dnaElements: ComponentDefinition list)
              (linker5p, linker3p)
              =

    if dnaElements.IsEmpty
    then failwithf "Tried to make an SBOL Rabit '%s' with no DNA elements!" name
    // Integrate the linkers and dna elements with explicit locations

    /// Create a range subcomponent in a linear sequence
    let rangeSubcomp (comp: ComponentDefinition) lastbp orient roles =
        let startbp = lastbp + 1

        let endbp =
            match comp.sequence with
            | Some (s) -> lastbp + s.elements.Length
            | None -> failwithf "Rabit %s has a DNA element without a sequence." name

        let loc =
            Range
                ({ start = startbp
                   stop = endbp
                   orient = orient })

        comp.asSubcomponent ([ loc ], roles), endbp

    let linker5pSC, lastbp =
        rangeSubcomp linker5p 0 FWD [ fivePrimeLinkerRoleUri ]

    /// assign explicit locations to all dna elements
    let rec createDnaSubcomps comps lastbp =
        match comps with
        | [ comp ] ->
            let sc, endbp =
                rangeSubcomp comp lastbp orientation [ rabitDnaRoleUri ]

            [ sc ], endbp
        | comp :: tail ->
            let sc, endbp =
                rangeSubcomp comp lastbp orientation [ rabitDnaRoleUri ]

            let others, finalbp = createDnaSubcomps tail endbp
            sc :: others, finalbp
        | [] -> [], lastbp // if you handed us an empty list

    let dnaSubcomps, lastbp = createDnaSubcomps dnaElements lastbp

    let linker3pSC, lastbp =
        rangeSubcomp linker3p lastbp FWD [ threePrimeLinkerRoleUri ]

    if lastbp <> dna.Length then
        failwithf
            "Subcomponent lengths added up to %d, but rabit '%s' has dna sequence of length %d."
            lastbp
            name
            dna.Length

    { id =
          { identity =
                (match compUri with
                 | Some (u) -> u
                 | None -> createTempUri ())
            name = Some(name)
            description = Some(desc) }
      roles = [ rabitRoleUri; (rabitBreedRole breed) ]
      sequence = Some(seqFromDna dna)
      subcomponents = linker5pSC :: linker3pSC :: dnaSubcomps
      gslProg = None }

/// Return the ComponentDefintion for a Stitch.
let sbolStitch (name: string) (desc: string) (compUri: Uri option) (rabits: ComponentDefinition list) =
    if rabits.IsEmpty
    then failwithf "Tried to make an SBOL Stitch '%s' with no rabits!" name

    let rec rabitSubcomponents (rlist: ComponentDefinition list) rabitScs =
        match rlist with
        | [ r ] ->
            let rsc =
                r.asSubcomponent ([], [ stitchRabitRoleUri ])

            rsc, rsc :: rabitScs
        | r :: nr :: tl ->
            let nrsc, rabitScs = rabitSubcomponents (nr :: tl) rabitScs

            let rsc =
                r.asSubcomponent ([ Precede(nrsc) ], [ stitchRabitRoleUri ])

            rsc, rsc :: rabitScs
        | [] -> failwith "Unreachable match condition in rabit subcomponent construction."

    let _, rabitScs = rabitSubcomponents rabits []

    // actually make the ComponentDefinition
    { id =
          { identity =
                (match compUri with
                 | Some (u) -> u
                 | None -> createTempUri ())
            name = Some(name)
            description = Some(desc) }
      roles = [ stitchRoleUri ]
      sequence = None
      subcomponents = rabitScs
      gslProg = None }

/// Return the ComponentDefintion for a Megastitch.
let sbolMegastitch (name: string)
                   (desc: string)
                   (compUri: Uri option)
                   (stitchA: ComponentDefinition)
                   (stitchB: ComponentDefinition option)
                   =

    let subcomps =
        match stitchB with
        | Some (s) ->
            let sBsc = s.asSubcomponent ([], [ stitchRoleUri ])

            [ stitchA.asSubcomponent ([ Precede(sBsc) ], [ stitchRoleUri ])
              sBsc ]
        | None -> [ stitchA.asSubcomponent ([], [ stitchRoleUri ]) ]

    // actually make the ComponentDefinition
    { id =
          { identity =
                (match compUri with
                 | Some (u) -> u
                 | None -> createTempUri ())
            name = Some(name)
            description = Some(desc) }
      roles = [ megastitchRoleUri ]
      sequence = None
      subcomponents = subcomps
      gslProg = None }
