module GslCore.TestSeamlessPrimers

open System
open GslCore.Plugin
open NUnit.Framework

open GslCore.Constants
open GslCore.Core.Types
open Amyris.Dna
open GslCore.Pragma

let map = Map.ofSeq

let emptyPragmas =
    PragmaCollection.empty

let uHO: DNASlice =
    { Id = Some(2)
      IsAmplified = true
      Annotations = []
      Breed = Breed.Upstream
      Description = "uHO"
      DestinationFrom = 0<ZeroOffset>
      DestinationForward = true
      DestinationTo = 549<ZeroOffset>
      Dna =
          Dna
              ("ACCTTTTTTGTGCGTGTATTGAAATATTATGACATATTACAGAAAGGGTTCGCAAGTCCTGTTTCTATGCCTTTCTCTTAGTAATTCACGAAATAAACCTATGGTTTACGAAATGATCCACGAAAATCATGTTATTATTTACATCAACATATCGCGAAAATTCATGTCATGTCCACATTAACATCATTGCAGAGCAACAATTCATTTTCATAGAGAAATTTGCTACTATCACCCACTAGTACTACCATTGGTACCTACTACTTTGAATTGTACTACCGCTGGGCGTTATTAGGTGTGAAACCACGAAAAGTTCACCATAACTTCGAATAAAGTCGCGGAAAAAAGTAAACAGCTATTGCTACTCAAATGAGGTTTGCAGAAGCTTGTTGAAGCATGATGAAGCGTTCTAAACGCACTATTCATCATTAAATATTTAAAGCTCATAAAATTGTATTCAATTCCTATTCTAAATGGCTTTTATTTCTATTACAACTATTAGCTCTAAATCCATATCCTCATAAGCAGCAATCAATTCTATCTATACTTTAAA")
      DnaSource = "S288C"
      ExternalId = None
      MaterializedFrom = None // Some({LegacyParseTypes.PPP})
      Pragmas = emptyPragmas
      SliceName = ""
      Type = SliceType.Regular
      SourceChromosome = "4"
      SourceFrom = 48031<ZeroOffset>
      SourceFromApprox = true
      SourceForward = false
      SourceTo = 48580<ZeroOffset>
      SourceToApprox = false
      Template =
          Some
              (Dna
                  ("ACCTTTTTTGTGCGTGTATTGAAATATTATGACATATTACAGAAAGGGTTCGCAAGTCCTGTTTCTATGCCTTTCTCTTAGTAATTCACGAAATAAACCTATGGTTTACGAAATGATCCACGAAAATCATGTTATTATTTACATCAACATATCGCGAAAATTCATGTCATGTCCACATTAACATCATTGCAGAGCAACAATTCATTTTCATAGAGAAATTTGCTACTATCACCCACTAGTACTACCATTGGTACCTACTACTTTGAATTGTACTACCGCTGGGCGTTATTAGGTGTGAAACCACGAAAAGTTCACCATAACTTCGAATAAAGTCGCGGAAAAAAGTAAACAGCTATTGCTACTCAAATGAGGTTTGCAGAAGCTTGTTGAAGCATGATGAAGCGTTCTAAACGCACTATTCATCATTAAATATTTAAAGCTCATAAAATTGTATTCAATTCCTATTCTAAATGGCTTTTATTTCTATTACAACTATTAGCTCTAAATCCATATCCTCATAAGCAGCAATCAATTCTATCTATACTTTAAA"))
      Uri = None }

let inlineSlice =
    { IsAmplified = false
      Annotations = []
      Breed = Breed.Inline
      Description = "ATGTGAC"
      DestinationFrom = 550<ZeroOffset>
      DestinationForward = true
      DestinationTo = 556<ZeroOffset>
      Dna = Dna("ATGTGAC")
      DnaSource = "S288C"
      ExternalId = None
      Id = Some(0)
      MaterializedFrom = None
      Pragmas = emptyPragmas
      SliceName = ""
      Type = SliceType.Inline
      SourceChromosome = "inline"
      SourceFrom = 0<ZeroOffset>
      SourceFromApprox = false
      SourceForward = true
      SourceTo = 6<ZeroOffset>
      SourceToApprox = false
      Template = Some(Dna("ATGTGAC"))
      Uri = None }

let dHO =
    { IsAmplified = true
      Annotations = []
      Breed = Breed.Downstream
      Description = "dHO"
      DestinationFrom = 557<ZeroOffset>
      DestinationForward = true
      DestinationTo = 1106<ZeroOffset>
      Dna =
          Dna
              ("AATGTGTATATTAGTTTAAAAAGTTGTATGTAATAAAAGTAAAATTTAATATTTTGGATGAAAAAAACCATTTTTAGACTTTTTCTTAACTAGAATGCTGGAGTAGAAATACGCCATCTCAAGATACAAAAAGCGTTACCGGCACTGATTTGTTTCAACCAGTATATAGATTATTATTGGGTCTTGATCAACTTTCCTCAGACATATCAGTAACAGTTATCAAGCTAAATATTTACGCGAAAGAAAAACAAATATTTTAATTGTGATACTTGTGAATTTTATTTTATTAAGGATACAAAGTTAAGAGAAAACAAAATTTATATACAATATAAGTAATATTCATATATATGTGATGAATGCAGTCTTAACGAGAAGACATGGCCTTGGTGACAACTCTCTTCAAACCAACTTCAGCCTTTCTCAATTCATCAGCAGATGGGTCTTCGATTTGCAAAGCAGCCAAAGCATCGGACAAAGCAGCTTCAATCTTGGACTTGGAACCTCTCTTCAATTTAGAAGACAAGACTGGGTCAGTGACAGTTTGTTCGAT")
      DnaSource = "S288C"
      ExternalId = None
      Id = Some(1)
      MaterializedFrom = None
      Pragmas = emptyPragmas
      SliceName = ""
      Type = SliceType.Regular
      SourceChromosome = "4"
      SourceFrom = 45720<ZeroOffset>
      SourceFromApprox = false
      SourceForward = false
      SourceTo = 46269<ZeroOffset>
      SourceToApprox = true
      Template =
          Some
              (Dna
                  ("AATGTGTATATTAGTTTAAAAAGTTGTATGTAATAAAAGTAAAATTTAATATTTTGGATGAAAAAAACCATTTTTAGACTTTTTCTTAACTAGAATGCTGGAGTAGAAATACGCCATCTCAAGATACAAAAAGCGTTACCGGCACTGATTTGTTTCAACCAGTATATAGATTATTATTGGGTCTTGATCAACTTTCCTCAGACATATCAGTAACAGTTATCAAGCTAAATATTTACGCGAAAGAAAAACAAATATTTTAATTGTGATACTTGTGAATTTTATTTTATTAAGGATACAAAGTTAAGAGAAAACAAAATTTATATACAATATAAGTAATATTCATATATATGTGATGAATGCAGTCTTAACGAGAAGACATGGCCTTGGTGACAACTCTCTTCAAACCAACTTCAGCCTTTCTCAATTCATCAGCAGATGGGTCTTCGATTTGCAAAGCAGCCAAAGCATCGGACAAAGCAGCTTCAATCTTGGACTTGGAACCTCTCTTCAATTTAGAAGACAAGACTGGGTCAGTGACAGTTTGTTCGAT"))
      Uri = None }

[<TestFixture>]
[<Category("Integration")>]
type TestSeamlessPrimers() =
    let checkSliceTypes (exp: SliceType list) (actual: SliceType list) =
        if exp <> actual then
            printfn "ERROR: matching slice types in seamless check"
            printfn "Expected: %s" (String.Join(",", exp |> List.map (SliceType.toString)))
            printfn "Actual  : %s" (String.Join(",", actual |> List.map (SliceType.toString)))

        Assert.AreEqual(exp, actual)

    [<Test>]
    member x.testShortInline() =
        // No need for fuse directive for small inline sequence
        //                                    <----------
        //    part / inline / part  =>   L part inline part L
        //                                    ---------->
        let dnaParts = [ uHO; inlineSlice; dHO ]

        let fused =
            SeamlessPlugin.procInsertFuse false dnaParts

        let pTypes = fused |> List.map (fun p -> p.Type)

        let expected =
            [ SliceType.Linker
              SliceType.Regular
              SliceType.Inline
              SliceType.Regular
              SliceType.Linker ]
        // bug   LINKER,REG,FUSION,INLINE,REG,LINKER
        checkSliceTypes expected pTypes

        ()
