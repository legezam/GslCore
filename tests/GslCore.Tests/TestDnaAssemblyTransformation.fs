﻿namespace GslCore.Tests

open NUnit.Framework
open Amyris.ErrorHandling
open GslcProcess
open LegacyParseTypes
open CommonTypes
open PragmaTypes
open Amyris.Dna
open Constants

module AssemblyTestBase =
    let emptyAssembly: Assembly = {
        parts = []
        name = None
        uri = None
        linkerHint = ""
        pragmas = EmptyPragmas
        designParams = DesignParams.initialDesignParams
        capabilities = Set.empty
        docStrings = []
        sourcePosition = []
    }

    let testSlice pragmas = {
        id = None
        extId = None
        dna = Dna("atcgatcgatcgatcgatcgatcgatcgatcgatcgatcgatcgatcgatcgatcgatcgatcgatcgatcgatcgatcg")
        sourceChr = ""
        sourceFr = 0<ZeroOffset>
        sourceTo = 0<ZeroOffset>
        sourceFwd = true
        sourceFrApprox = false
        sourceToApprox = false
        destFr = 0<ZeroOffset>
        destTo = 0<ZeroOffset>
        destFwd = true
        amplified = false
        template = None
        sliceName = ""
        uri = None
        description = ""
        sliceType = INLINEST
        pragmas = pragmas
        dnaSource = ""
        breed = B_INLINE
        materializedFrom = None
        annotations = []
    }

    let testAssembly part assemblyPragmas : DnaAssembly = {
        id = None
        dnaParts = [part]
        name = ""
        uri = None
        linkerHint = ""
        pragmas = assemblyPragmas
        designParams = DesignParams.initialDesignParams
        docStrings = []
        materializedFrom = emptyAssembly
        tags=Set.empty
        topology = Linear
    }    

[<TestFixture>]
type Test() = 
    let runTest assembly expectedSource =
        let transformed =
            cleanLongSlices () assembly
            |> returnOrFail
        let source =
            transformed.dnaParts
            |> List.head
            |> (fun p -> p.dnaSource)
        Assert.AreEqual(expectedSource, source)

    [<SetUp>]
    member x.SetUp() =
        finalizePragmas []

    [<Test>]
    member x.TestLongSliceUsesDnaSrc() =
        let sliceNoSource = AssemblyTestBase.testSlice EmptyPragmas
        let assemblyNoSource = AssemblyTestBase.testAssembly sliceNoSource EmptyPragmas

        runTest assemblyNoSource "synthetic"

        let refGenomePragmas = EmptyPragmas.Add("refgenome", "foogenome") |> returnOrFail

        let assemblyHasRefGenome = AssemblyTestBase.testAssembly sliceNoSource refGenomePragmas

        runTest assemblyHasRefGenome "foogenome"

        let sliceWithSource =
            EmptyPragmas.Add("dnasrc", "foosource")
            |> returnOrFail
            |> AssemblyTestBase.testSlice

        let assemblyHasSource = AssemblyTestBase.testAssembly sliceWithSource EmptyPragmas

        runTest assemblyHasSource "foosource"

        let assemblyHasSourceAndRef = AssemblyTestBase.testAssembly sliceWithSource refGenomePragmas

        // #dnasrc should take precedence over refgenome if both are present
        runTest assemblyHasSourceAndRef "foosource"

    