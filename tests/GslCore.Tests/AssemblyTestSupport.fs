/// Shared operations for compiling and extracting material from assemblies
module GslCore.AssemblyTestSupport

open GslCore.AstTypes
open GslCore.AstAssertions
open GslCore.AstExpansion
open GslCore.Constants
open Amyris.ErrorHandling
open GslCore.CommonTypes
open GslCore.Pragma
open GslCore.Pragma.Domain
open Amyris.Dna

let rec extractAssemblies (n: AstNode): AstNode list =
    [ match n with
      | Block b ->
          let result = b.x |> List.collect extractAssemblies
          yield! result
      | Splice s ->
          let result =
              s
              |> List.ofArray
              |> List.collect extractAssemblies

          yield! result
      | Part p ->
          match p.x.basePart with
          | Assembly a as x -> yield x
          | _ -> ()
      | Assembly a as x -> yield x
      | _ -> () ]


/// compile one GSL source example and extract assemblies
let compileOne source =
    source
    |> GslSourceCode
    |> compile (phase1 Set.empty PragmaCache.builtin)
    |> returnOrFail
    |> fun x -> extractAssemblies x.wrappedNode

/// Simple slice creator with just the parameters
/// needed for testing procAssembly
let makeSimpleSlice dna sliceName sliceType pragmas isFromApprox isToApprox isAmplified breed =

    { id = None
      extId = None
      dna = dna
      sourceChr = "1"
      sourceFr = 0<ZeroOffset>
      sourceTo = (dna.Length - 1) * 1<ZeroOffset>
      sourceFwd = true
      sourceFrApprox = isFromApprox
      sourceToApprox = isToApprox
      destFr = 0<ZeroOffset>
      destTo = (dna.Length - 1) * 1<ZeroOffset>
      destFwd = true
      /// is this slice created by PCR
      amplified = isAmplified
      template = None // might want to add
      sliceName = sliceName
      uri = None
      description = sliceName
      sliceType = sliceType
      pragmas = pragmas
      dnaSource = "unknown"
      breed = breed
      /// Keep track of the part this slice was materialized from.
      materializedFrom = None
      annotations = [] }


let uFoo =
    makeSimpleSlice
        (Dna
            "TACTGACTGAGTCTGACTGACGTTAGCTGACTGACTGCATGACGTACGTACTGAGTCAGTCGTACTGACTGACTGCATGACTGACTGCATGCATGATGCGTATCGAGCGGCGCTGCTGTGGTCGTATATCTGGTCGTATGTGCGTACGTGTAGTCATGCGTACTG")
        "uFoo"
        SliceType.REGULAR
        PragmaCollection.empty
        true
        false
        true
        Breed.B_UPSTREAM

let dFoo =
    makeSimpleSlice
        (Dna
            "TTTGGTATGCTGTTATCGTGTTGGGCGGTCTATTGAGTTTTGCGTGTCGTAGTCGTGCGGCGCGTATTGTGCGTGTCGGCGCGATGCGTGTGTTGAGTCGTGTGGGATTGGTGTGTGTCGTCGCGACTGATCATGTATCAGTCGAGCGATGGTGTGTCAGTGTTGTGAGTCG")
        "dFoo"
        SliceType.REGULAR
        PragmaCollection.empty
        true  // from approx
        false  // to approx
        true  // amplified
        Breed.B_DOWNSTREAM

/// open reading frame slice
let oBar =
    makeSimpleSlice
        (Dna
            "ATGTCTCAGAACGTTTACATTGTATCGACTGCCAGAACCCCAATTGGTTCATTCCAGGGTTCTCTATCCTCCAAGACAGCAGTGGAATTGGGTGCTGTTGCTTTAAAAGGCGCCTTGGCTAAGGTTCCAGAATTGGATGCATCCAAGGAT")
        "oBar"
        SliceType.REGULAR
        PragmaCollection.empty
        false  // from approx
        false  // to approx
        true  // amplified
        Breed.B_FUSABLEORF

/// open reading frame slice
let oBar2 =
    makeSimpleSlice
        (Dna
            "ATTGTGATGCTGTACGTGGTTGCGTTGCTGTGTGCGTGCGCGCGTATATTATAGTCGCGGCTAGTTACGTGCGGCGTACTGGTCGTGTCGATGGTAGTCGTCGGCGCGAGTGTCGTATGCGTACGTACTGACGGCGCGCGCAGTTGATAG")
        "oBar"
        SliceType.REGULAR
        PragmaCollection.empty
        false  // from approx
        false  // to approx
        true  // amplified
        Breed.B_FUSABLEORF

/// promoter
let pBaz =
    makeSimpleSlice
        (Dna
            "TTGACTGATGCTGACTGACTGATGCTGACTGACTGGGGGCTAGTGCTACTATCTATCCATCACACACACATCAGTCGTACTTATATTATATGATCTTACGATCTATATTATTACGGATCTGATATATTTACGTTGATTATGCGATCTGAT")
        "pBaz"
        SliceType.REGULAR
        PragmaCollection.empty
        true  // from approx
        false  // to approx
        true  // amplified
        Breed.B_PROMOTER

/// terminator
let tShaz =
    makeSimpleSlice
        (Dna
            "ATATTATATACTGTCGCGACTTATATATATATCTGACGTCTGTGCTGATGATTATATATTACTGACTGCGTCATGATCTATTATATATATATTATATCTGGTCGTCGTCTGAGTCATGCGTACTGACGTACTATATATATATATATATAG")
        "pBaz"
        SliceType.REGULAR
        PragmaCollection.empty
        false  // from approx
        true  // to approx
        true  // amplified
        Breed.B_TERMINATOR

let marker =
    makeSimpleSlice
        (Dna
            "TGTACTGACGTAGTCGTACACGTAGTCGTATCGATGTGCGACGTACTGAGCGTAGTCTGATGCGTATGCTCGTAGTAGTCGTACGTACGTGTCGTCGTGTGTGTAGTCGTGTACGAGCGTACGATCGATCAGTCTGACGTAGTGTAGTCGTAGTGTCGTAGTACGTA")
        "###"
        SliceType.MARKER
        PragmaCollection.empty
        false  // from approx
        false  // to approx
        true  // amplified
        Breed.B_MARKER
/// really short inline (14) which will be implemented with primers
let shortInline =
    makeSimpleSlice
        (Dna "CACATGTGGAGATT")
        "shortInline1"
        SliceType.INLINEST
        PragmaCollection.empty
        false  // from approx
        false  // to approx
        true  // amplified
        Breed.B_MARKER

let rabitStart =
    { Definition = BuiltIn.rabitStartPragmaDef
      Arguments = [] }

let rabitEnd =
    { Definition = BuiltIn.rabitEndPragmaDef
      Arguments = [] }

let amp =
    { Definition = BuiltIn.ampPragmaDef
      Arguments = [] }

let fusePragma =
    { Definition = BuiltIn.fusePragmaDef
      Arguments = [] }

let inlinePragma =
    { Definition = BuiltIn.inlinePragmaDef
      Arguments = [] }

/// 102 bp inline
let longInline =
    makeSimpleSlice
        (Dna "ATGTCTCAGAACGTTTACATTGTATCGACTGCCAGAACCCCAATTGGTTCATTCCAGGGTTCTCTATCCTCCAAGACAGCAGTGGAATTGGGTGCTGTTATG")
        "longinline"
        SliceType.INLINEST
        PragmaCollection.empty
        false  // from approx
        false  // to approx
        true  // amplified
        Breed.B_INLINE

/// 75 bp inline
let mediumInline =
    makeSimpleSlice
        (Dna "TTTGACGTGTAGTCGTGCGCGGTCGCGCGCGTCTATTTTTGTCGTCGTACGTACGTACGGCTAGCGTACGTACGT")
        "mediuminline"
        SliceType.INLINEST
        PragmaCollection.empty
        false  // from approx
        false  // to approx
        true  // amplified
        Breed.B_INLINE

/// small 48 bp inline
let smallInline =
    makeSimpleSlice
        (Dna "TAGCTATATAGGTAGCTAGACTATCTTTATCTTACTACTTCTCTTTAT")
        "smallinline"
        SliceType.INLINEST
        PragmaCollection.empty
        false  // from approx
        false  // to approx
        true  // amplified
        Breed.B_INLINE

let uFooFuse =
    { uFoo with
          pragmas =
              uFoo.pragmas
              |> PragmaCollection.addPragma fusePragma
          sliceName = uFoo.sliceName + "#fuse" }

let smallInlineAmp =
    { smallInline with
          pragmas =
              smallInline.pragmas
              |> PragmaCollection.addPragma amp
          sliceName = smallInline.sliceName + "#amp" }

let smallInlineFuse =
    { smallInline with
          pragmas =
              smallInline.pragmas
              |> PragmaCollection.addPragma fusePragma
          sliceName = smallInline.sliceName + "#fuse" }

let mediumInlineAmp =
    { mediumInline with
          pragmas =
              mediumInline.pragmas
              |> PragmaCollection.addPragma amp
          sliceName = mediumInline.sliceName + "#amp" }

let mediumInlineFuse =
    { mediumInline with
          pragmas =
              mediumInline.pragmas
              |> PragmaCollection.addPragma fusePragma
          sliceName = mediumInline.sliceName + "#fuse" }

let longInlineAmp =
    { longInline with
          pragmas =
              longInline.pragmas
              |> PragmaCollection.addPragma amp
          sliceName = longInline.sliceName + "#amp" }

let longInlineAmpFuse =
    { longInlineAmp with
          pragmas =
              longInlineAmp.pragmas
              |> PragmaCollection.addPragma fusePragma
          sliceName = longInlineAmp.sliceName + "#fuse" }

let longInlineInline =
    { longInline with
          pragmas =
              longInline.pragmas
              |> PragmaCollection.addPragma inlinePragma }

let shortInlineWithRabitStart =
    { shortInline with
          pragmas =
              shortInline.pragmas
              |> PragmaCollection.addPragma rabitStart
          sliceName = shortInline.sliceName + "#rabitstart" }

let shortInlineWithRabitEnd =
    { shortInline with
          pragmas =
              shortInline.pragmas
              |> PragmaCollection.addPragma rabitEnd
          sliceName = shortInline.sliceName + "#rabitend" }

let linkerAlice =
    makeSimpleSlice
        (Dna "GATCGATTAGATCGATAGGCTACG")
        "linkerAlice"
        SliceType.LINKER
        PragmaCollection.empty
        false  // from approx
        false  // to approx
        true  // amplified
        Breed.B_LINKER
(*
/// 102 bp inline
let longInline =
    makeSimpleSlice
        (Dna "ATGTCTCAGAACGTTTACATTGTATCGACTGCCAGAACCCCAATTGGTTCATTCCAGGGTTCTCTATCCTCCAAGACAGCAGTGGAATTGGGTGCTGTTATG")
        "oBar"
        SliceType.INLINEST
        EmptyPragmas
        false // from approx
        false // to approx
        true // amplified
        Breed.B_INLINE


let shortInlineWithRabitStart =
    { shortInline with
        pragmas = shortInline.pragmas.Add(rabitStart)
        description = shortInline.description+"#rabitstart"
    }
let shortInlineWithRabitEnd =
    { shortInline with
        pragmas = shortInline.pragmas.Add(rabitEnd);
        description = shortInline.description+"#rabitend"
    }
let linkerAlice =
    makeSimpleSlice
        (Dna "GATCGATTAGATCGATAGGCTACG")
        "linkerAlice"
        SliceType.LINKER
        EmptyPragmas
        false // from approx
        false // to approx
        true // amplified
        Breed.B_LINKER

        *)
let linkerBob =
    makeSimpleSlice
        (Dna "TTTGGTTTGTAGCGGGGCTTTAGA")
        "linkerBob"
        SliceType.LINKER
        PragmaCollection.empty
        false  // from approx
        false  // to approx
        true  // amplified
        Breed.B_LINKER

let linkerCharlie =
    makeSimpleSlice
        (Dna "ATGATGGGATCGGGATCGGGGGCAGACTTTG")
        "linkerCharlie"
        SliceType.LINKER
        PragmaCollection.empty
        false  // from approx
        false  // to approx
        true  // amplified
        Breed.B_LINKER

let linkerDoug =
    makeSimpleSlice
        (Dna "GATCGATTAGCTTAGATCGTGATCGGTCG")
        "linkerDoug"
        SliceType.LINKER
        PragmaCollection.empty
        false  // from approx
        false  // to approx
        true  // amplified
        Breed.B_LINKER

let linkerEmma =
    makeSimpleSlice
        (Dna "ATCGATTAGATTAGCTACTGTGGTCCAAA")
        "linkerEmma"
        SliceType.LINKER
        PragmaCollection.empty
        false  // from approx
        false  // to approx
        true  // amplified
        Breed.B_LINKER

let placeholder =
    makeSimpleSlice
        (Dna "")
        "placeholder"
        SliceType.LINKER
        PragmaCollection.empty
        false  // from approx
        false  // to approx
        true  // amplified
        Breed.B_VIRTUAL

let fuse =
    makeSimpleSlice
        (Dna "")
        "fusion"
        SliceType.FUSIONST
        PragmaCollection.empty
        false  // from approx
        false  // to approx
        false  // amplified
        Breed.B_X
