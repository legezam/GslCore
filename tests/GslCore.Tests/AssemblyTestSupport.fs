/// Shared operations for compiling and extracting material from assemblies
module GslCore.AssemblyTestSupport

open GslCore.Ast
open GslCore.Ast.Types
open GslCore.AstAssertions
open GslCore.Constants
open GslCore.Core.Types
open GslCore.GslResult
open GslCore.Pragma
open Amyris.Dna
open GslCore.Ast.Phase1


let defaultPhase1Parameters =
    { Phase1Parameters.PragmaBuilder = PragmaFactory.builtin
      LegalCapabilities = Set.empty }

let rec extractAssemblies (n: AstNode): AstNode list =
    [ match n with
      | AstNode.Block b ->
          let result =
              b.Value |> List.collect extractAssemblies

          yield! result
      | AstNode.Splice s ->
          let result =
              s
              |> List.ofArray
              |> List.collect extractAssemblies

          yield! result
      | AstNode.Part p ->
          match p.Value.BasePart with
          | AstNode.Assembly _ as x -> yield x
          | _ -> ()
      | AstNode.Assembly _ as x -> yield x
      | _ -> () ]


/// compile one GSL source example and extract assemblies
let compileOne source =
    source
    |> GslSourceCode
    |> compile (Phase1.phase1 defaultPhase1Parameters)
    |> GslResult.assertOk
    |> fun x -> extractAssemblies x.wrappedNode

/// Simple slice creator with just the parameters
/// needed for testing procAssembly
let makeSimpleSlice dna sliceName sliceType pragmas isFromApprox isToApprox isAmplified breed =

    { Id = None
      ExternalId = None
      Dna = dna
      SourceChromosome = "1"
      SourceFrom = 0<ZeroOffset>
      SourceTo = (dna.Length - 1) * 1<ZeroOffset>
      SourceForward = true
      SourceFromApprox = isFromApprox
      SourceToApprox = isToApprox
      DestinationFrom = 0<ZeroOffset>
      DestinationTo = (dna.Length - 1) * 1<ZeroOffset>
      DestinationForward = true
      /// is this slice created by PCR
      IsAmplified = isAmplified
      Template = None // might want to add
      SliceName = sliceName
      Uri = None
      Description = sliceName
      Type = sliceType
      Pragmas = pragmas
      DnaSource = "unknown"
      Breed = breed
      /// Keep track of the part this slice was materialized from.
      MaterializedFrom = None
      Annotations = [] }


let uFoo =
    makeSimpleSlice
        (Dna
            "TACTGACTGAGTCTGACTGACGTTAGCTGACTGACTGCATGACGTACGTACTGAGTCAGTCGTACTGACTGACTGCATGACTGACTGCATGCATGATGCGTATCGAGCGGCGCTGCTGTGGTCGTATATCTGGTCGTATGTGCGTACGTGTAGTCATGCGTACTG")
        "uFoo"
        SliceType.Regular
        PragmaCollection.empty
        true
        false
        true
        Breed.Upstream

let dFoo =
    makeSimpleSlice
        (Dna
            "TTTGGTATGCTGTTATCGTGTTGGGCGGTCTATTGAGTTTTGCGTGTCGTAGTCGTGCGGCGCGTATTGTGCGTGTCGGCGCGATGCGTGTGTTGAGTCGTGTGGGATTGGTGTGTGTCGTCGCGACTGATCATGTATCAGTCGAGCGATGGTGTGTCAGTGTTGTGAGTCG")
        "dFoo"
        SliceType.Regular
        PragmaCollection.empty
        true  // from approx
        false  // to approx
        true  // amplified
        Breed.Downstream

/// open reading frame slice
let oBar =
    makeSimpleSlice
        (Dna
            "ATGTCTCAGAACGTTTACATTGTATCGACTGCCAGAACCCCAATTGGTTCATTCCAGGGTTCTCTATCCTCCAAGACAGCAGTGGAATTGGGTGCTGTTGCTTTAAAAGGCGCCTTGGCTAAGGTTCCAGAATTGGATGCATCCAAGGAT")
        "oBar"
        SliceType.Regular
        PragmaCollection.empty
        false  // from approx
        false  // to approx
        true  // amplified
        Breed.FusableOrf

/// open reading frame slice
let oBar2 =
    makeSimpleSlice
        (Dna
            "ATTGTGATGCTGTACGTGGTTGCGTTGCTGTGTGCGTGCGCGCGTATATTATAGTCGCGGCTAGTTACGTGCGGCGTACTGGTCGTGTCGATGGTAGTCGTCGGCGCGAGTGTCGTATGCGTACGTACTGACGGCGCGCGCAGTTGATAG")
        "oBar"
        SliceType.Regular
        PragmaCollection.empty
        false  // from approx
        false  // to approx
        true  // amplified
        Breed.FusableOrf

/// promoter
let pBaz =
    makeSimpleSlice
        (Dna
            "TTGACTGATGCTGACTGACTGATGCTGACTGACTGGGGGCTAGTGCTACTATCTATCCATCACACACACATCAGTCGTACTTATATTATATGATCTTACGATCTATATTATTACGGATCTGATATATTTACGTTGATTATGCGATCTGAT")
        "pBaz"
        SliceType.Regular
        PragmaCollection.empty
        true  // from approx
        false  // to approx
        true  // amplified
        Breed.Promoter

/// terminator
let tShaz =
    makeSimpleSlice
        (Dna
            "ATATTATATACTGTCGCGACTTATATATATATCTGACGTCTGTGCTGATGATTATATATTACTGACTGCGTCATGATCTATTATATATATATTATATCTGGTCGTCGTCTGAGTCATGCGTACTGACGTACTATATATATATATATATAG")
        "pBaz"
        SliceType.Regular
        PragmaCollection.empty
        false  // from approx
        true  // to approx
        true  // amplified
        Breed.Terminator

let marker =
    makeSimpleSlice
        (Dna
            "TGTACTGACGTAGTCGTACACGTAGTCGTATCGATGTGCGACGTACTGAGCGTAGTCTGATGCGTATGCTCGTAGTAGTCGTACGTACGTGTCGTCGTGTGTGTAGTCGTGTACGAGCGTACGATCGATCAGTCTGACGTAGTGTAGTCGTAGTGTCGTAGTACGTA")
        "###"
        SliceType.Marker
        PragmaCollection.empty
        false  // from approx
        false  // to approx
        true  // amplified
        Breed.Marker
/// really short inline (14) which will be implemented with primers
let shortInline =
    makeSimpleSlice
        (Dna "CACATGTGGAGATT")
        "shortInline1"
        SliceType.Inline
        PragmaCollection.empty
        false  // from approx
        false  // to approx
        true  // amplified
        Breed.Marker

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
        SliceType.Inline
        PragmaCollection.empty
        false  // from approx
        false  // to approx
        true  // amplified
        Breed.Inline

/// 75 bp inline
let mediumInline =
    makeSimpleSlice
        (Dna "TTTGACGTGTAGTCGTGCGCGGTCGCGCGCGTCTATTTTTGTCGTCGTACGTACGTACGGCTAGCGTACGTACGT")
        "mediuminline"
        SliceType.Inline
        PragmaCollection.empty
        false  // from approx
        false  // to approx
        true  // amplified
        Breed.Inline

/// small 48 bp inline
let smallInline =
    makeSimpleSlice
        (Dna "TAGCTATATAGGTAGCTAGACTATCTTTATCTTACTACTTCTCTTTAT")
        "smallinline"
        SliceType.Inline
        PragmaCollection.empty
        false  // from approx
        false  // to approx
        true  // amplified
        Breed.Inline

let uFooFuse =
    uFoo.Pragmas
    |> PragmaCollection.add fusePragma
    |> GslResult.assertOk
    |> fun pragmas ->
        { uFoo with
              Pragmas = pragmas
              SliceName = uFoo.SliceName + "#fuse" }

let smallInlineAmp =
    smallInline.Pragmas
    |> PragmaCollection.add amp
    |> GslResult.assertOk
    |> fun pragmas ->
        { smallInline with
              Pragmas = pragmas
              SliceName = smallInline.SliceName + "#amp" }

let smallInlineFuse =
    smallInline.Pragmas
    |> PragmaCollection.add fusePragma
    |> GslResult.assertOk
    |> fun pragmas ->
        { smallInline with
              Pragmas = pragmas
              SliceName = smallInline.SliceName + "#fuse" }

let mediumInlineAmp =
    mediumInline.Pragmas
    |> PragmaCollection.add amp
    |> GslResult.assertOk
    |> fun pragmas ->
        { mediumInline with
              Pragmas = pragmas
              SliceName = mediumInline.SliceName + "#amp" }

let mediumInlineFuse =
    mediumInline.Pragmas
    |> PragmaCollection.add fusePragma
    |> GslResult.assertOk
    |> fun pragmas ->
        { mediumInline with
              Pragmas = pragmas
              SliceName = mediumInline.SliceName + "#fuse" }

let longInlineAmp =
    longInline.Pragmas
    |> PragmaCollection.add amp
    |> GslResult.assertOk
    |> fun pragmas ->
        { longInline with
              Pragmas = pragmas
              SliceName = longInline.SliceName + "#amp" }

let longInlineAmpFuse =
    longInlineAmp.Pragmas
    |> PragmaCollection.add fusePragma
    |> GslResult.assertOk
    |> fun pragmas ->
        { longInlineAmp with
              Pragmas = pragmas
              SliceName = longInlineAmp.SliceName + "#fuse" }

let longInlineInline =
    longInline.Pragmas
    |> PragmaCollection.add inlinePragma
    |> GslResult.assertOk
    |> fun pragmas -> { longInline with Pragmas = pragmas }

let shortInlineWithRabitStart =
    shortInline.Pragmas
    |> PragmaCollection.add rabitStart
    |> GslResult.assertOk
    |> fun pragmas ->
        { shortInline with
              Pragmas = pragmas

              SliceName = shortInline.SliceName + "#rabitstart" }

let shortInlineWithRabitEnd =
    shortInline.Pragmas
    |> PragmaCollection.add rabitEnd
    |> GslResult.assertOk
    |> fun pragmas ->
        { shortInline with
              Pragmas = pragmas
              SliceName = shortInline.SliceName + "#rabitend" }

let linkerAlice =
    makeSimpleSlice
        (Dna "GATCGATTAGATCGATAGGCTACG")
        "linkerAlice"
        SliceType.Linker
        PragmaCollection.empty
        false  // from approx
        false  // to approx
        true  // amplified
        Breed.Linker
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
        SliceType.Linker
        PragmaCollection.empty
        false  // from approx
        false  // to approx
        true  // amplified
        Breed.Linker

let linkerCharlie =
    makeSimpleSlice
        (Dna "ATGATGGGATCGGGATCGGGGGCAGACTTTG")
        "linkerCharlie"
        SliceType.Linker
        PragmaCollection.empty
        false  // from approx
        false  // to approx
        true  // amplified
        Breed.Linker

let linkerDoug =
    makeSimpleSlice
        (Dna "GATCGATTAGCTTAGATCGTGATCGGTCG")
        "linkerDoug"
        SliceType.Linker
        PragmaCollection.empty
        false  // from approx
        false  // to approx
        true  // amplified
        Breed.Linker

let linkerEmma =
    makeSimpleSlice
        (Dna "ATCGATTAGATTAGCTACTGTGGTCCAAA")
        "linkerEmma"
        SliceType.Linker
        PragmaCollection.empty
        false  // from approx
        false  // to approx
        true  // amplified
        Breed.Linker

let placeholder =
    makeSimpleSlice
        (Dna "")
        "placeholder"
        SliceType.Linker
        PragmaCollection.empty
        false  // from approx
        false  // to approx
        true  // amplified
        Breed.Virtual

let fuse =
    makeSimpleSlice
        (Dna "")
        "fusion"
        SliceType.Fusion
        PragmaCollection.empty
        false  // from approx
        false  // to approx
        false  // amplified
        Breed.X
