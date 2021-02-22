namespace GslCore.Tests

open GslCore
open GslCore.DesignParams
open GslCore.GslResult
open NUnit.Framework
open Amyris.ErrorHandling
open GslCore.GslcProcess
open GslCore.Ast.LegacyParseTypes
open GslCore.Core.Types
open GslCore.Pragma
open Amyris.Dna
open GslCore.Constants

module AssemblyTestBase =
    let emptyAssembly: Assembly =
        { parts = []
          name = None
          uri = None
          linkerHint = ""
          pragmas = PragmaCollection.empty
          designParams = DesignParams.identity
          capabilities = Set.empty
          docStrings = []
          sourcePosition = [] }

    let testSlice pragmas =
        { Id = None
          ExternalId = None
          Dna = Dna("atcgatcgatcgatcgatcgatcgatcgatcgatcgatcgatcgatcgatcgatcgatcgatcgatcgatcgatcgatcg")
          SourceChromosome = ""
          SourceFrom = 0<ZeroOffset>
          SourceTo = 0<ZeroOffset>
          SourceForward = true
          SourceFromApprox = false
          SourceToApprox = false
          DestinationFrom = 0<ZeroOffset>
          DestinationTo = 0<ZeroOffset>
          DestinationForward = true
          IsAmplified = false
          Template = None
          SliceName = ""
          Uri = None
          Description = ""
          Type = SliceType.Inline
          Pragmas = pragmas
          DnaSource = ""
          Breed = Breed.Inline
          MaterializedFrom = None
          Annotations = [] }

    let testAssembly part assemblyPragmas: DnaAssembly =
        { Id = None
          DnaParts = [ part ]
          Name = ""
          Uri = None
          LinkerHint = ""
          Pragmas = assemblyPragmas
          DesignParams = DesignParams.identity
          DocStrings = []
          MaterializedFrom = emptyAssembly
          Tags = Set.empty
          Topology = Linear }

[<TestFixture>]
type Test() =
    let runTest assembly expectedSource =
        let transformed =
            cleanLongSlices () assembly |> GslResult.valueOr (fun _ -> failwith "illegal")

        let source =
            transformed.DnaParts
            |> List.head
            |> (fun p -> p.DnaSource)

        Assert.AreEqual(expectedSource, source)

    [<Test>]
    member x.TestLongSliceUsesDnaSrc() =
        let sliceNoSource = AssemblyTestBase.testSlice PragmaCollection.empty

        let assemblyNoSource =
            AssemblyTestBase.testAssembly sliceNoSource PragmaCollection.empty

        runTest assemblyNoSource "synthetic"

        let refGenomePragmas =
            PragmaCollection.empty
            |> PragmaCollection.add
                   { Pragma.Definition = BuiltIn.refGenomePragmaDef
                     Arguments = [ "foogenome" ] }
            

        let assemblyHasRefGenome =
            AssemblyTestBase.testAssembly sliceNoSource refGenomePragmas

        runTest assemblyHasRefGenome "foogenome"

        let sliceWithSource =
            PragmaCollection.empty
            |> PragmaCollection.add
                   { Pragma.Definition = BuiltIn.dnaSrcPragmaDef
                     Arguments = [ "foosource" ] }
            |> AssemblyTestBase.testSlice

        let assemblyHasSource =
            AssemblyTestBase.testAssembly sliceWithSource PragmaCollection.empty

        runTest assemblyHasSource "foosource"

        let assemblyHasSourceAndRef =
            AssemblyTestBase.testAssembly sliceWithSource refGenomePragmas

        // #dnasrc should take precedence over refgenome if both are present
        runTest assemblyHasSourceAndRef "foosource"
