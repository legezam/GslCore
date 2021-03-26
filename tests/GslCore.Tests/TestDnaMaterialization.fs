namespace GslCore.Tests

open System.IO
open GslCore
open GslCore.AstAssertions
open GslCore.Core
open GslCore.Core.DnaCreation
open GslCore.Pragma
open GslCore.Primer
open GslCore.Reference
open NUnit.Framework
open Amyris.Bio.biolib
open GslCore.Legacy.Types
open GslCore.Core.Types
open GslCore.GslResult
open GslCore.Constants
open GslCore.DesignParams
open GslCore.AssemblyTestSupport

/// One part design request and test expectation w.r.t reference
type private PartTest =
    { ppp: PartPlusPragma
      gp: GenePartWithLinker
      revPart: bool
      revReference: bool }

/// Tests for DNA part retrieval from genome
[<TestFixture>]
[<Category("Integration")>]
type DnaMaterialization() =
    /// location of test gslc_lib fixtures
    let testLibDir1 = @"../../../../TestGslcLib"
    let testLibDir2 = @"../../../../../TestGslcLib"

    let testLibDir =
        if Directory.Exists testLibDir1 then testLibDir1 else testLibDir2

    let refGenomePragma =
        PragmaFactory.createPragmaFromNameValue "refgenome" [ "TestGenome2" ] PragmaFactory.builtin
        |> GslResult.valueOr (fun _ -> failwith "Failure to build refgenome TestGenome2")


    let pc =
        PragmaCollection.create [ refGenomePragma ]


    /// We don't need much from an assembly so ok to leave it mostly empty
    let emptyAssembly =
        { Parts = []
          Name = None
          Uri = None
          LinkerHint = ""
          Pragmas = pc
          DesignParams = DesignParams.identity
          Capabilities = Set.empty
          DocStrings = []
          SourcePosition = [] }

    /// this test gene is on the fwd / W strand
    let tADH1 =
        { Gene = "tADH1"
          Modifiers = []
          Where = [] }
    /// this test gene is on the rev / C strand
    let tABC1 =
        { Gene = "tABC1"
          Modifiers = []
          Where = [] }

    // general retrieval parameters that are gene agnostic
    let gd =
        GenomeDefinition.createEager testLibDir "TestGenome2"

    let verbose = false

    let rgs: GenomeDefinitions =
        [ ("TestGenome2", gd) ]
        |> Map.ofList
        |> GenomeDefinitions.create

    let library: SequenceLibrary = Map.empty
    let a: Assembly = emptyAssembly
    let dnaSource = "TestGenome2"


    // get the raw DNA we are going to compare retrieval against. First chromosome
    let testGenome2Chrom1 =
        Path.Combine(testLibDir, "TestGenome2", "TestGenome2.fsa")
        |> fastaStream
        |> Seq.head
        |> snd

    let materializeOne gp ppp =
        DnaCreation.expandGenePart verbose rgs library a dnaSource ppp gp

    /// get dna between sourceFr and sourceTo
    let liftDnaFromChrom (dna: DNASlice) =
        assert (dna.SourceFrom <= dna.SourceTo)
        let left = dna.SourceFrom
        let right = dna.SourceTo
        testGenome2Chrom1.[left / 1<ZeroOffset>..right / 1<ZeroOffset>]

    /// retrieve one part and check the coordinates match the DNA returned independently
    let checkOne (test: PartTest) =
        let dna =
            materializeOne test.gp test.ppp
            |> GslResult.assertOk

        let fromDna = liftDnaFromChrom dna

        Assert.AreEqual
            (fromDna,
             (if test.revReference then dna.Dna.RevComp() else dna.Dna)
                 .str)

    let checkOneWithProcAssembly testName (template: DNASlice list) (test: PartTest) =
        let dna =
            materializeOne test.gp test.ppp
            |> GslResult.assertOk

        // drop test part into template where marker is located
        let parts =
            template
            |> List.map (fun part ->
                if part = placeholder then dna // swap in test part
                else part)

        let placeholderIndex =
            template
            |> List.findIndex (fun part -> part = placeholder)

        let _primers, slices =
            PrimerCreation.designPrimersForAssembly false DesignParams.identity testName [] [] [] parts

        let dna = slices |> List.item placeholderIndex
        let fromDna = liftDnaFromChrom dna

        Assert.AreEqual
            (fromDna,
             (if test.revReference then dna.Dna.RevComp() else dna.Dna)
                 .str)

    let makeTest gp revPart revReference =
        let gpWithLinker = { Part = gp; Linker = None }

        { gp = gpWithLinker
          ppp =
              { Part = Part.GenePart gpWithLinker
                Pragma = PragmaCollection.empty
                IsForward = not revPart }
          revPart = revPart
          revReference = revReference }

    let tABC1Prefix =
        "AAGAAGTTGCATGCGCCTATTATTACTTCAATAGATGGCAAATGGAAAAAG"

    let tADH1Prefix =
        "GCGAATTTCTTATGATTTATGATTTTTATTATTAAATAAGTTATAAAAAA"

    // makeTest part revPart revReference
    let test1 = makeTest tADH1 false false
    let test2 = makeTest tABC1 false true
    let test3 = makeTest tADH1 true true
    let test4 = makeTest tABC1 true false

    let testContext1 = [ placeholder; linkerAlice; dFoo ]
    let testContext2 = [ linkerAlice; placeholder; dFoo ]

    [<Test>]
    [<Category("Integration")>]
    /// Check basic sequence
    member __.ChecktADH1StartsWith() =
        let dna =
            materializeOne test1.gp test1.ppp
            |> GslResult.assertOk

        Assert.AreEqual(tADH1Prefix, dna.Dna.[0..tADH1Prefix.Length - 1].str)

    [<Test>]
    [<Category("Integration")>]
    /// Check basic sequence
    member __.CheckRevtADH1EndsWith() =
        let dna =
            materializeOne test3.gp test3.ppp
            |> GslResult.assertOk

        Assert.IsTrue(dna.Dna.RevComp().str.StartsWith(tADH1Prefix))

    [<Test>]
    [<Category("Integration")>]
    /// Check basic sequence
    member __.ChecktABC1StartsWith() =
        let dna =
            materializeOne test2.gp test2.ppp
            |> GslResult.assertOk

        Assert.AreEqual(tABC1Prefix, dna.Dna.[0..tABC1Prefix.Length - 1].str)

    [<Test>]
    [<Category("Integration")>]
    /// Check basic sequence
    member __.CheckRevtABC1EndsWith() =
        let dna =
            materializeOne test4.gp test4.ppp
            |> GslResult.assertOk

        Assert.IsTrue(dna.Dna.RevComp().str.StartsWith(tABC1Prefix))

    [<Test>]
    [<Category("Integration")>]
    /// fwd gene in genome used in fwd orientation
    member __.CheckFromCoordsFwdGeneFwd() = checkOne test1

    [<Test>]
    [<Category("Integration")>]
    /// rev gene in genome used in fwd orientation
    member __.CheckFromCoordsRevGeneFwd() = checkOne test2

    [<Test>]
    [<Category("Integration")>]
    /// fwd gene in genome used in rev orientation
    member __.CheckFromCoordsFwdGeneRev() = checkOne test3

    [<Test>]
    [<Category("Integration")>]
    /// rev gene in genome used in rev orientation
    member __.CheckFromCoordsRevGeneRev() = checkOne test4

    [<Test>]
    [<Category("Integration")>]
    member __.CheckFromCoordsProcAssembly1() =
        checkOneWithProcAssembly "checkFromCoordsProcAssembly1" testContext1 test1

    [<Test>]
    [<Category("Integration")>]
    member __.CheckFromCoordsProcAssembly2() =
        checkOneWithProcAssembly "checkFromCoordsProcAssembly2" testContext1 test2

    [<Test>]
    [<Category("Integration")>]
    member __.CheckFromCoordsProcAssembly3() =
        checkOneWithProcAssembly "checkFromCoordsProcAssembly3" testContext1 test3

    [<Test>]
    [<Category("Integration")>]
    member __.CheckFromCoordsProcAssembly4() =
        checkOneWithProcAssembly "checkFromCoordsProcAssembly4" testContext1 test4

    [<Test>]
    [<Category("Integration")>]
    member __.CheckFromCoordsProcAssembly1B() =
        checkOneWithProcAssembly "checkFromCoordsProcAssembly1B" testContext2 test1

    [<Test>]
    [<Category("Integration")>]
    member __.CheckFromCoordsProcAssembly2B() =
        checkOneWithProcAssembly "checkFromCoordsProcAssembly2B" testContext2 test2

    [<Test>]
    [<Category("Integration")>]
    member __.CheckFromCoordsProcAssembly3B() =
        checkOneWithProcAssembly "checkFromCoordsProcAssembly3B" testContext2 test3

    [<Test>]
    [<Category("Integration")>]
    member __.CheckFromCoordsProcAssembly4B() =
        checkOneWithProcAssembly "checkFromCoordsProcAssembly4B" testContext2 test4
