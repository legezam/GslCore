module GslCore.Core.AlleleSwaps

///
/// Support for introducing mutations into genes
///
open Amyris.Bio
open Amyris.Dna
open System
open GslCore.Constants
open GslCore.DesignParams
open GslCore.Ast.Types
open GslCore.Pragma
open GslCore.Ast.Legacy.Types
open GslCore.Reference
open IO.CodonUsage
open biolib
open primercore
open GslCore.Core.Ryse // for getrabit

open GslCore.Core.PluginTypes

(*

if < 1000 to 5' and > 2000 to 3' then   5' design
otherwise 3'

Warn if > 1000 and > 2000 then warn

if IG region < 600 then just duplicate

If upstream gene also same direction, then fine
if divergent and mutation within 500bp of other gene, then repeat mutation

consider double HB

*)

/// Build a reverse lookup table of possible codons encoding a particular amino
/// acid, avoiding a provided list of non starters
let makeRevLookup (avoid: Set<Dna>) =
    DnaConstants.codons
    |> Array.fold (fun (aaMap: Map<char, Set<Dna>>) codon ->
        if avoid.Contains(codon) then
            aaMap
        else
            let aa = DnaOps.codon2aa codon

            if aaMap.ContainsKey(aa)
            then aaMap.Remove(aa).Add(aa, aaMap.[aa].Add(codon))
            else aaMap.Add(aa, Set.ofList [ codon ])) Map.empty

// Rare yeast codons we'd like to avoid at the 5' end
// Yeast specific, we'll need to change this potentially
let rareCodons =
    [ "CTC"; "CGC"; "CGA"; "CGG" ]
    |> List.map Dna
    |> Set.ofSeq

let polyG = [ "GGG" ] |> List.map Dna |> Set.ofSeq

/// list of possible codons encoding each amino acid
let aa2Codons = makeRevLookup polyG
let aa2CodonsNonCare = makeRevLookup (rareCodons + polyG)

/// Count #differences between two codons
let diff (c1: Dna) (c2: Dna) =
    Array.map2 (fun a b -> if a = b then 0 else 1) c1.arr c2.arr
    |> Array.sum

/// How far left is desired change - want a codon change that minimizes base
/// pair changes on right hand side
let diffLeft (c1: Dna) (c2: Dna) =
    Array.mapi2 (fun i a b -> if a <> b then (i + 1) else 0) c1.arr c2.arr
    |> Array.sum

/// How far right is desired change - want a codon change that minimizes base
/// pair changes on left hand side
let diffRight (c1: Dna) (c2: Dna) =
    Array.mapi2 (fun i a b -> if a <> b then (4 - i) else 0) c1.arr c2.arr
    |> Array.sum

/// Count GC bases in sequence
let gcCount (c: Dna) =
    c.arr
    |> Array.sumBy (fun c -> if c = 'G' || c = 'C' || c = 'g' || c = 'c' then 1 else 0)

/// Pick a codon for an amino acid that is maximally different to a given codon
/// given a comparison function cmp that gives a positive score for degree of difference
let selectMutCodonBase cmp (cl: CodonLookup) (minFreq: float) (origCodon: Dna) (targetAA: char) =

    aa2Codons.[targetAA]
    |> Set.fold (fun (bestDiff, bestGC, bestFreq, bestCodon) thisCodon ->
        let thisFreq =
            match cl.Codon(thisCodon.str) with
            | None -> 0.0
            | Some (f) -> f.relFreq1

        let d = cmp thisCodon origCodon

        let better =
            d > bestDiff
            || (d = bestDiff && (gcCount thisCodon > bestGC))
            || (thisFreq >= minFreq && bestFreq < minFreq)

        if better
        then (d, gcCount thisCodon, thisFreq, thisCodon)
        else (bestDiff, bestGC, bestFreq, bestCodon)) (-1, -1, 0.0, Dna(""))
    |> fun (_, _, _, codon) -> codon

/// Find codon that is maximally different given a function to calculate different
let selectMutCodon = selectMutCodonBase diff
/// Variant of selectMutCodon, that forces the base pair difference as far left as possible
let selectMutCodonLeft = selectMutCodonBase diffLeft
/// Variant of selectMutCodon that forces the base pair differences as far right as possible
let selectMutCodonRight = selectMutCodonBase diffRight

/// expand a simple mutation inline with a part
let expandSimpleMut (asAACheck: bool) (_: GenomeDefinition) (g: LegacyPartId) (m: Mutation): GslSourceCode =

    // Get part sequence
    if not (g.Id.StartsWith("R"))
    then failwithf "ERROR: part %s should start with 'R'.  Non rabit part mutation not supported." g.Id

    let hr = Ryse.getRabit (int (g.Id.[1..]))

    let rabit = hr.RabitSpecs.[0]

    let dna =
        rabit.DnaElementSpecs.[0].DnaSequence.ToUpper()
    // Now split by type of mutation and check the original base/amino acid is legit
    match m.Type with
    | NT ->
        if m.Location <= 0 || m.Location > dna.Length
        then failwithf "ERROR: mutation position %d is outside range of rabit %s" m.Location g.Id

        if dna.[m.Location - 1] <> m.From then
            failwithf
                "ERROR: existing base at position %d in rabit %s is %c not %c"
                m.Location
                g.Id
                (dna.[m.Location - 1])
                m.From

        // Design for mutation

        let lhs = m.Location - 1 // before the mutated base
        let rhs = m.Location + 1 // after the mutated base
        let id = g.Id

        sprintf "@%s[1:%d] {#dnasrc %s}; /%c/ {#inline }; @%s[%d:-1E] {#dnasrc %s} " id lhs id m.To id rhs id
        |> GslSourceCode
    | AA ->
        if m.Location <= 0 || m.Location > dna.Length / 3
        then failwithf "ERROR: mutation position %d outside range of rabit %s amino acids" m.Location g.Id

        let currentCodon =
            (dna.[(m.Location - 1) * 3..(m.Location - 1) * 3 + 2])
                .ToCharArray()

        // Ensure we are in the right place in the gene
        if (codon2aa currentCodon <> m.From) && asAACheck then
            failwithf
                "ERROR: for mutation %c%d%c , gene %s has amino acid %c (%s) in that position not %c"
                m.From
                m.Location
                m.To
                g.Id
                (codon2aa currentCodon)
                (utils.arr2seq currentCodon)
                m.From
        else
            let id = g.Id
            let lhs = m.Location - 1
            let rhs = m.Location + 1

            sprintf "@%s[1:%da] {#dnasrc %s}; /$%c/ {#inline }; @%s[%da:-1E] {#dnasrc %s} " id lhs id m.To id rhs id
            |> GslSourceCode


/// 5' promoter mutation design - construct mutation with overlapping primers,
/// repeat promoter region around marker to minimize distruption and allow loop out
/// and use a heterology block downstream to prevent crossovers that eliminate the mutation
/// a         b            a         b          b+2                 c
/// promoterRep ; marker ; promoterRep(mutation)promoter ...ATG..... HB ....
let private classicPromoterNT gene name (f: sgd.Feature) (rg: GenomeDefinition) (m: Mutation) =
    // TODO - this design isn't compatible with the thumper workflow, as it involves a 4 piece rabit

    // m.pos is the zero based version of the offset from the 'A' in the start codon
    // note counterintuitively, we don't have to remove 1 from the '1' based coordinate system,
    // that only happens for the positive coordinate system
    let errorDesc = name

    // b is the base before the mutation, so one base upstream of the promoter mutation
    // b is the basepair before the mutation relative to gene orientation
    let b = (m.Location - 1) * 1<ZeroOffset>

    let genomicCoord =
        (if f.fwd
         then f.l - m.Location
         else f.r - m.Location (* remember m.pos is negative *) )
        * 1<ZeroOffset>

    let existing =
        rg
        |> GenomeDefinition.getDna (errorDesc, sprintf "%d" f.chr, genomicCoord, genomicCoord)
        |> DnaOps.revCompIf (not f.fwd)

    if existing.[0] <> m.From then
        let diag =
            if f.fwd then
                rg
                |> GenomeDefinition.getDna (errorDesc, sprintf "%d" f.chr, genomicCoord, (f.l + 2) * 1<ZeroOffset>)
            else
                (rg
                 |> GenomeDefinition.getDna (errorDesc, sprintf "%d" f.chr, (f.r - 2) * 1<ZeroOffset>, genomicCoord))
                    .RevComp()

        failwithf "ERROR: g%s promoter mutation at %d should be base %c a (in gene orientation) and is %c instead\n leading gene oriented seq=%O"
            name m.Location m.From (existing.[0]) diag

    // Ensure we capture promoter and stay far enough away from b
    let a = (b - 800<ZeroOffset>)

    let c' =
        max (b + 200<ZeroOffset>) (29<ZeroOffset>)

    let c = (c' / 3) * 3 + 2<ZeroOffset>

    sprintf
        "%s[~%A:%A] {#name %s.5us};### ;%s[~%A:%A] {#name %s.hb} ; /%c/ {#inline };%s[%A:%A];~ ;%s[%A:~%A]"
        gene
        (ZeroOffset.toOne a)
        (ZeroOffset.toOne b)
        name
        gene
        (ZeroOffset.toOne a)
        (ZeroOffset.toOne b)
        name
        m.To  // replacement base
        gene
        (ZeroOffset.toOne (b + 2<ZeroOffset>))
        (ZeroOffset.toOne c)
        gene
        (ZeroOffset.toOne (c + 1<ZeroOffset>))
        (ZeroOffset.toOne (c + 1000<ZeroOffset>))
    |> GslSourceCode

let private classicCodingNT _ (* verbose*) g (_: sgd.Feature) (_: GenomeDefinition) (m: Mutation) =
    // Not going to implement this completely just yet - i.e no heterology block.
    // This has to be used as part of a larger design that ensures swapping
    sprintf "%s[1S:%A];/%c/ {#inline };%s[%A:-1E] " g (m.Location - 1) m.To g (m.Location + 1)
    |> GslSourceCode


/// Classic reference implementation for an allele swap always
/// available but overwritten by other implementations
let jobScorerClassicAAMut _ = Some 0.0<PluginScore>

/// Decide which end of the gene the mutation is closer to,
/// and pick design
/// Gene is the actual gene name,  name is the #name entry or full gYNG2$C227Y entry
let classicAAMut (dp: AlleleSwapDesignParams) =
    let minFreq = 0.05
    let x1 = ZeroOffset.toInt dp.MutOff
    let x2 = (ZeroOffset.toInt dp.MutOff) + 2

    if x2
       >= dp.OrfDna.Length + 3 (* include stop codon *)  then
        failwithf
            "ERROR: attempting to mutate amino acid position %d which is outside ORF length %d bases"
            dp.Mutation.Location
            dp.OrfDna.Length

    let currentCodon = dp.OrfDna.[x1..x2]
    // Removing: this seems to be checked by the parent function
//    // Ensure we are in the right place in the gene
//    if not (codon2aa currentCodon = m.f) && asAACheck then
//        printfn "ORF: %s" (arr2seq orf)
//        failwithf
//            "ERROR: for mutation %c%d%c , gene g%s has amino acid %c (%s) in that position not %c"
//            m.f m.pos m.t name (codon2aa currentCodon) (arr2seq currentCodon) m.f

    // If mutation is relatively far from 3' end and close enough to 5' end, use 5' design
    // but we prefer 3' designs since they are less disruptive
    // Can't get too close to 5' end and still insert hB on left, so will need a 5' design
    // 20120204:  Chris suggested changing 50 -> 30bp as the proximity limit for 5' design
    if (dp.EndPref = NTERM
        || (dp.MutOff < 30<ZeroOffset>
            || (dp.MutOff < 1000<ZeroOffset>
                && dp.Length - dp.MutOff > 2500<ZeroOffset>))) then
        let mutSeq =
            selectMutCodonRight dp.CodonLookup minFreq currentCodon dp.Mutation.To

        assert ((DnaOps.translate mutSeq).[0] = dp.Mutation.To)
        // 5' end design, need to rewrite promoter and put in marker
        //                                   b
        // a......-1; marker ; a...ATG.......Mutation ; HB ...  HB + 700
        let a = -1000<ZeroOffset> // TODO - could adjust this based on intergenic distance
        let a2 = -600<ZeroOffset> // Promoter region

        // 3' end design, classic
        sprintf
            "%s[~%A:~-100] {#name %s.5us} ;### ;%s[~%A:%A] {#name %s.hb} ;/%O/ {#inline };~ ;%s[%A:~%A]"
            dp.Gene
            (ZeroOffset.toOne a)
            dp.Name
            dp.Gene
            (ZeroOffset.toOne a2)
            (ZeroOffset.toOne (dp.MutOff - 1<ZeroOffset>))
            dp.Name
            mutSeq
            dp.Gene
            (dp.MutOff + 3<ZeroOffset> |> ZeroOffset.toOne)
            (dp.MutOff + 703<ZeroOffset> |> ZeroOffset.toOne)
    else
        let mutSeq =
            selectMutCodonLeft dp.CodonLookup minFreq currentCodon dp.Mutation.To

        assert ((DnaOps.translate mutSeq).[0] = dp.Mutation.To)

        //  a            b                       c
        //  US700.......Mutation; HB ; dnaToDS200 ; marker ; 800bp downstream including 200bp repeat
        let a' = dp.MutOff - 700<ZeroOffset> // Pick 700 so we can still sequence through

        let a =
            if (ZeroOffset.toOne a') = 0<OneOffset> then 1<OneOffset> else ZeroOffset.toOne a'

        let c = 200<OneOffset>

        sprintf
            "%s[~%A:%A] {#name %s.hb} ;~ ;/%O/ {#inline };%s[%A:~%AE] ;### ;%s[1E:~%AE] {#name %s.3ds} "
            dp.Gene
            a
            ((ZeroOffset.toOne (dp.MutOff - 1<ZeroOffset>)))
            dp.Name
            mutSeq
            dp.Gene
            (dp.MutOff + 3<ZeroOffset> |> ZeroOffset.toOne)
            c
            dp.Gene
            (c + 600<OneOffset>)
            dp.Name
    |> GslSourceCode



//----- General entry point for allele swaps

/// expand an allele swap.  takes verbose/GenomeDef/gene and mutation
/// Common entry point for different types of designs.  Does sanity checking and basic lookups
/// then passes off expansion to specific subdesign routines
let expandAS (providers: AlleleSwapProvider list)
             (asAACheck: bool)
             (name: string)
             (verbose: bool)
             (rg: GenomeDefinition)
             (codonLookup: CodonLookup)
             (g: string)
             (m: Mutation)
             (endPref: EndPref)
             (capa: Capabilities)
             (pragmas: PragmaCollection)
             (longStyle: bool)
             =

    if verbose
    then printf "Processing mutation gene:%s %A\n" g m

    // remove prefix of gene name and retrieve feature description
    let f =
        rg |> GenomeDefinition.getFeature (g.[1..])

    let errorDesc = name

    match m.Type with
    | NT ->
        match m.Location with
        | x when x = 0 -> failwithf "ERROR: illegal mutation offset zero for %s:%A\n" g m
        | x when x < 0 -> classicPromoterNT g name f rg m // Point mutation in promoter
        | x when x > 0 && x < f.r - f.l + 1 -> classicCodingNT verbose g f rg m // Point mutation in coding part of gene
        | _ -> failwithf "ERROR: unimplemented, 3' terminator mutations for %s:%A" g m
    | AA ->
        // General checks we should always make for a coding amino acid change

        // Gen length of ORF
        let len = (f.r - f.l + 1) * 1<ZeroOffset>
        let b = (m.Location - 1) * 3<ZeroOffset> // zero based coordinates, location of mutant codon

        // Extend ORF to include stop codon so we can mutate that if needed
        let l', r' =
            if f.fwd then f.l, f.r + 3 else f.l - 3, f.r

        let orf =
            rg
            |> GenomeDefinition.getDna (errorDesc, sprintf "%d" f.chr, l' * 1<ZeroOffset>, r' * 1<ZeroOffset>)
            |> DnaOps.revCompIf (not f.fwd)

        /// Orf sequence plus "orfPlusMargin"
        let orfPlus =
            rg
            |> GenomeDefinition.getDna
                (errorDesc,
                 sprintf "%d" f.chr,
                 ((l' - Default.OrfPlusMargin) |> max 0)
                 * 1<ZeroOffset>,
                 (r' + Default.OrfPlusMargin) * 1<ZeroOffset>)
            |> DnaOps.revCompIf (not f.fwd)

        let x1 = ZeroOffset.toInt b
        let x2 = (ZeroOffset.toInt b) + 2

        if x2 >= orf.Length + 3 then // Include stop codon
            failwithf
                "ERROR: attempting to mutate amino acid position %d which is outside ORF length %d bases"
                m.Location
                orf.Length

        let currentCodon = orf.[x1..x2]

        // Ensure we are in the right place in the gene
        if (codon2aa currentCodon.arr <> m.From) && asAACheck then
            printfn "ORF: %O" orf

            failwithf
                "ERROR: for mutation %c%d%c , gene %s has amino acid %c (%O) in that position not %c"
                m.From
                m.Location
                m.To
                g
                (codon2aa currentCodon.arr)
                currentCodon
                m.From

        let designParams =
            { IsVerbose = verbose
              IsLongStyle = longStyle
              CodonLookup = codonLookup
              EndPref = endPref
              Gene = g
              Name = name
              ReferenceGenome = rg
              Feature = f
              Mutation = m
              Length = len
              MutOff = b
              OrfDna = orf
              OrfPlusDna = orfPlus
              Pragmas = pragmas }
        // Choose provider
        let fn =
            providers
            |> List.choose (fun provider ->
                match provider.JobScorer capa with
                | None -> None
                | Some (score) -> Some(score, provider.Provider))
            |> List.sortWith (fun (a, _) (b, _) -> compare b a)
            |> // Note sort largest first
            List.head
            |> snd

        fn designParams

// Heterology block implementation
// ============================================================================

(*
/// Create a heterology block
/// Remove part of the right hand slice and replace with rewritten sequence
///
let private generateRightHB' (prefix:char array) (slice:char array) =
    // Dumbest implementation imaginable, cut away 10 amino acids
    let current = slice.[0..29] |> translate
    // Pick alternative codons
    let alt = current |> Array.mapi (fun i codon -> selectMutCodon codonLookup minFreq (slice.[i*3..i*3+2]) codon) |> Array.concat
    alt // return the alternative starting sequence
*)

type private HBMove =
    | HBLEFT
    | HBRIGHT
    | HBBOTH

///
/// Generate a heterology block by eating into the downstream sequencing
/// and replacing it with a sequence that translates into the equivalent
/// amino acid sequence.  Be mindful of primer design and ensure that
/// the heterology block is long enough while having a good Tm of overlap
/// and leaving enough bases in a 60mer to overlap the adjacent regions
/// Takes up  prefix and down  which are the adjacent sequence and the prefix sequence
/// (e.g. a mutation) we'd like just before the heterology block
///
/// Takes an optional targetAALen if a particular number of changed amino acids are required
/// This overrides any Tm optimization considerations.
/// Returns alternative sequence for down not including prefix sequence, but assuming Tm calculation will be for
/// prefix + down
let private generateHBCore (cl: CodonLookup)
                           (minFreq: float)
                           (targetAALen: int option)
                           (dp: DesignParams)
                           (fwd: bool)
                           (up: Dna)
                           (prefix: Dna)
                           (down: Dna)
                           =
    // Parameters
    // Want ~10 amino acids changed
    //
    // ds500Rev 65
    // Overlap  65
    // us500Fwd 65
    // cPCR     56
    // leftHB overhang   56
    // rightHB overhang   56
    let localVerbose = false
    let meltHB = 70.0<C>
    let meltHBNeighbour = 60.0<C>
    let maxOligoLen = dp.PrimerParams.maxLength

    let upRev = up.RevComp()
    // TODO - if we are near the 5' end of the gene we should use non rare
    // codons.  Can't currently figure out position from params
    // Also need to ensure we don't chew back past 5' start codon

    // Build a really long het block to use as a template
    // Note which strand we are translating on.  If this is a reverse (left) design, watch other strand

    /// Have to ensure this is a multiple of 3 to ensure correct translation
    /// Make a decent length of downstream alternatives, at least 120 bp or max if there is one but not longer
    /// than available dna segment
    let transLen =
        (down.Length / 3 * 3)
        |> min
            (match targetAALen with
             | None -> 120
             | Some (v) -> v * 3)

    let downTrans =
        down.[0..transLen - 1]
        |> DnaOps.revCompIf (not fwd)

    let current = DnaOps.translate downTrans

    // Pick alternative codons
    let alt =
        current
        |> Array.mapi (fun i aa ->
            let orig = downTrans.[i * 3..i * 3 + 2]
            // ensure we passed in something sensible
            assert ((DnaOps.translate orig).[0] = aa)
            selectMutCodon cl minFreq orig aa)
        |> DnaOps.concat
        // If the template was flipped to get translation right, flip it back
        |> DnaOps.revCompIf (not fwd)

    let altRetranslated =
        alt
        |> DnaOps.revCompIf (not fwd)
        |> DnaOps.translate

    if localVerbose
    then printf "templat: %O\n" downTrans.[0..min 60 downTrans.Length]

    if localVerbose then printf "    alt: %O\n" alt
    if localVerbose then printf "Current:%O\n" current

    if localVerbose
    then printf "Alt    :%s\n" (altRetranslated |> utils.arr2seq)

    // ensure the sequence still reads the same amino acids out
    assert (DnaOps.translate downTrans = altRetranslated)

    match targetAALen with
    | Some (v) -> alt.[..v * 3 - 1]
    | None ->
        /// How many bases do we need to go out till 10 amino acids have had changed codons
        let rec countTo10 i j =
            if i = 10 || j + 3 = alt.Length then j
            else if alt.[j..j + 2] = down.[j..j + 2] then countTo10 i (j + 3)
            else countTo10 (i + 1) (j + 3)

        let ten = countTo10 0 0
        // Decide on a cutover point between the hetblock and the adjacent wild type DNA
        // so we meet tm requirements for the HB part (A+B) , and tm requirements for the wt part (C)
        // and tm requirements for the other reverse adjacent sequence
        // and the hetblock is long enough and the total primer doesn't exceed 60bp
        //                          i
        //         pref~~~~~~~~~~~~~~~~~~~wtwtwtwtwtwtwtwtwtwtwt
        // upupupupprefwtwtwtwtwtwtwtwtwtwtwtwtwtwtwtwtwtwtwtwtw
        // DDDDDDDDAAAABBBBBBBBBBBBBCCCCCCCCCCCCCCC
        // Pick ideal point i, first base post het block
        //let localVerbose = true
        let minC = 18
        let tm2PrimerParams = 20.0<1/C> // Cost of deviation in the flanking region
        let tm1PrimerParams = 20.0<1/C> // Cost of deviation in the HB
        let hbLenPrimerParams = 30.0 // Cost of hb not reaching desired # amino acid changes

        let rec transitionOpt left right i =
            // Eval position i
            let AB = DnaOps.append prefix alt.[..i - 1]

            let penHB =
                { dp.PrimerParams with
                      maxLength = maxOligoLen - minC }

            let task =
                { tag = "HBFwd"
                  temp = AB.arr
                  align = ANCHOR.LEFT
                  strand = STRAND.TOP
                  offset = 0
                  targetTemp = meltHB
                  sequencePenalties = None }

            let score, primerLen, move =
                match oligoDesign false penHB task with
                | None -> 999.0, 0, HBBOTH // Failed primer generation, not an option
                | Some (o1) ->
                    // HB primer designed, what's left for the C region?

                    let primer1 =
                        if o1.oligo.Length % 3 = 0
                        then Dna(o1.oligo)
                        else AB.[0..((o1.oligo.Length / 3 + 1) * 3) - 1]

                    let penC =
                        { dp.PrimerParams with
                              maxLength = maxOligoLen - primer1.Length
                              minLength = max 10 (54 - primer1.Length) }

                    let task2 =
                        { tag = "CFwd"
                          temp =
                              down.[primer1.Length..primer1.Length + penC.maxLength]
                                  .arr
                          align = ANCHOR.LEFT
                          strand = STRAND.TOP
                          offset = 0
                          targetTemp = meltHBNeighbour
                          sequencePenalties = None }

                    let move1 =
                        if o1.temp < meltHB then HBRIGHT
                        else if o1.temp > meltHB then HBLEFT
                        else HBRIGHT

                    match oligoDesign false penC task2 with
                    | None -> 999.0, 0, HBLEFT // Failed primer 2 generation, not an option
                    | Some (o2) ->
                        // We have two primers now, one over HB and one matching RHS
                        // Oligo 3, upstream
                        let task3 =
                            { tag = "DRev"
                              temp =
                                  upRev.[0..min (upRev.Length - 1) (penC.maxLength - 1)]
                                      .arr
                              align = ANCHOR.LEFT
                              strand = STRAND.TOP
                              offset = 0
                              targetTemp = meltHBNeighbour
                              sequencePenalties = None }

                        match oligoDesign false penC task3 with
                        | None -> 999.0, 0, HBLEFT // Failed primer 3 generation, not an option
                        | Some (o3) ->
                            // We have three primers now and can calculate some sense of optimiality for this
                            // heterology block.  Take into account the two tms and
                            let score =
                                (o1.temp - meltHB |> abs) * tm1PrimerParams
                                + (o2.temp - meltHBNeighbour |> abs)
                                  * tm2PrimerParams
                                + (o3.temp - meltHBNeighbour |> abs)
                                  * tm2PrimerParams
                                + (if primer1.Length <= ten then
                                    (ten - primer1.Length |> float)
                                    * hbLenPrimerParams
                                    + 15.0
                                   else if primer1.Length < ten + 15 then
                                       (ten + 15 - primer1.Length |> float)
                                   else
                                       0.0)

                            assert (score >= 0.0)
                            // WARNING - can't customize the sodium and primer concentration values here
                            let x =
                                primercore.temp dp.PrimerParams o3.oligo o3.oligo.Length

                            if localVerbose then
                                printf "i=%-3d p1l=%-3d p2l=%-3d p3l=%-3d p1t=%-3A p2t=%-3A p3t=%-3A %f score=%f %s %s\n"
                                    i o1.oligo.Length o2.oligo.Length o3.oligo.Length o1.temp o2.temp o3.temp (float x)
                                    score (utils.arr2seq o2.oligo) (utils.arr2seq o3.oligo)

                            let move2 =
                                if o2.temp < meltHBNeighbour then HBRIGHT
                                else if o2.temp > meltHBNeighbour then HBLEFT
                                else HBRIGHT

                            // Return the score, the length of the HB portion of the primer and the suggested direction to move
                            if move1 = move2 then score, primer1.Length, move1 else score, primer1.Length, HBBOTH

            match score, move with
            | 0.0, _ -> score, primerLen // done
            | _, HBLEFT when i > left ->
                let score', primerLen' = transitionOpt left (i - 3) (i - 3)
                if score' < score then score', primerLen' else score, primerLen
            | _, HBBOTH when i > left && i = right ->
                let score', primerLen' = transitionOpt left (i - 3) (i - 3)
                if score' < score then score', primerLen' else score, primerLen
            | _, HBRIGHT when i < right ->
                let score', primerLen' = transitionOpt (i + 3) right (i + 3)
                if score' < score then score', primerLen' else score, primerLen
            | _, HBBOTH when i < right && i = left ->
                let score', primerLen' = transitionOpt (i + 3) right (i + 3)
                if score' < score then score', primerLen' else score, primerLen
            | _, HBBOTH when i > left && i < right ->
                let scoreR, primerLenL = transitionOpt (i + 3) right (i + 3)
                let scoreL, primerLenR = transitionOpt left (i - 3) (i - 3)

                let score2, primer2 =
                    if scoreL < scoreR then scoreL, primerLenL else scoreR, primerLenR

                if score2 < score then score2, primer2 else score, primerLen
            | _, _ -> score, primerLen

        let left = 6 * 3 // min 3 amino acids changed
        let right = 42 // min 18 bp for overlap primer
        let mid = maxOligoLen / 2
        let finalScore, finalPrimer = transitionOpt left right mid

        if localVerbose
        then printf "finalScore=%f finalPrimerLen=%d\n" finalScore finalPrimer
        alt.[..finalPrimer - 1]

/// Create a heterology block, removing part of the right hand (down) slice
/// sequence and replacing with a rewritten sequence
/// Entry point into this module
let generateRightHB (cl: CodonLookup)
                    (minFreq: float)
                    (targetAALen: int option)
                    (pp: DesignParams)
                    (up: Dna)
                    (prefix: Dna)
                    (down: Dna)
                    =
    generateHBCore cl minFreq targetAALen pp true up prefix down

/// Create a heterology block, removing part of the left hand (up) slice sequence and replacing with a rewritten sequence
/// Entry point into this module
let generateLeftHB (cl: CodonLookup)
                   (minFreq: float)
                   (targetAALen: int option)
                   (pp: DesignParams)
                   (up: Dna)
                   (prefix: Dna)
                   (down: Dna)
                   =
    // generateRight takes upstream prefix and downstream sequence (to be eaten into)
    // reverse everything so that it works as a right design
    //
    //    upupupupup pref downdowndowndown
    generateHBCore cl minFreq targetAALen pp false (down.RevComp()) (prefix.RevComp()) (up.RevComp())
    |> DnaOps.revComp
