﻿namespace GslCore.Tests

open System
open GslCore
open GslCore.Ast.LegacyParseTypes
open GslCore.Core
open GslCore.Core.Ryse
open GslCore.GslResult
open NUnit.Framework
open GslCore.Core.Types
open GslCore.AssemblyTestSupport
open GslCore.Pragma
open GslCore.DesignParams

[<TestFixture>]
type TestMapRyseLinkers() =

    /// Enable for detailed (very detailed) output from mapRyseLinkers - useful for debugging test cases
    let verbose = false


    let makePragma name values =
        let p =
            PragmaBuilder.createPragmaFromNameValue name values PragmaBuilder.builtin
            |> GslResult.valueOr (fun _ -> failwith "building pragma")

        { Pragmas = [ p.Name, p ] |> Map.ofList }

    /// perform one test and check output pattern and sequences
    let runOne (name: string) isMegastitch (linkersIn: (DNASlice list * DNASlice list)) slicesIn expected =
        /// Boring default options
        let opts: ParsedOptions =
            { Quiet = false
              RefStrain = "cenpk"
              LibDir = "whatever"
              Iter = true
              OnlyPhase1 = false
              DoParallel = false
              Verbose = verbose
              NoPrimers = false
              LexOnly = false
              RefList = false
              RefDump = None
              ListPlugins = false
              DoHelpPragmas = false
              IsDeterministic = false }

        let leftLinkers, rightLinkers = linkersIn

        let linkers =
            [ for l in leftLinkers @ rightLinkers ->
                l.SliceName,
                { RYSELinker.Name = l.SliceName
                  Dna = l.Dna } ]
            |> Map.ofList


        /// Wrap up in a generic assembly
        let assemblyIn: DnaAssembly =
            { Id = None
              DnaParts = slicesIn
              Name = name
              Uri = None
              LinkerHint =
                  String.Join(",", [ for l in leftLinkers -> l.SliceName ])
                  + "|"
                  + String.Join(",", [ for l in rightLinkers -> l.SliceName ])
              Pragmas = if isMegastitch then PragmaCollection.empty else makePragma "platform" [ "stitch" ]
              DesignParams = DesignParams.identity
              DocStrings = []
              MaterializedFrom =
                  { Assembly.name = None
                    parts = []
                    uri = None
                    linkerHint = ""
                    pragmas = PragmaCollection.empty
                    designParams = DesignParams.identity
                    capabilities = Set.empty
                    docStrings = []
                    sourcePosition = [] }
              Tags = Set.empty
              Topology = Linear }

        let updatedAssembly =
            Ryse.mapRyseLinkers opts Map.empty linkers assemblyIn
        // Check expected and actual slice output
        SharedSliceTesting.checkSequence expected updatedAssembly.DnaParts


    [<Test>]
    member __.SinglePart() =
        runOne
            "SinglePart"
            false  // is stitch
            ([ linkerAlice; linkerBob ], [])  // A and B part linkers
            [ uFoo ]
            [ linkerAlice; uFoo; linkerBob ]

    [<Test>]
    member __.TwoParts() =
        runOne
            "TwoParts"
            false  // is stitch
            ([ linkerAlice
               linkerBob
               linkerCharlie ],
             [])  // A and B part linkers
            [ uFoo; dFoo ]
            [ linkerAlice
              uFoo
              linkerBob
              dFoo
              linkerCharlie ]

    [<Test>]
    member __.TwoPartsPlusShortInline() =
        runOne
            "TwoPartsShortInline"
            false  // is stitch
            ([ linkerAlice; linkerBob ], [])  // A and B part linkers
            [ uFoo; shortInline; dFoo ]
            [ linkerAlice
              uFoo
              shortInline
              dFoo
              linkerBob ]

    [<Test>]
    member __.FuseTwoNormalSlices() =
        // Note - we need to use the explicit fusionSlice here to test
        // rather than just putting a #fuse pragma inside foo since that expansion
        // step (pragma -> virtual slice) would already have happened by this stage normally
        runOne
            "FuseTwoNormalSlices"
            false  // is stitch
            ([ linkerAlice; linkerBob ], [])  // A and B part linkers
            [ uFoo; fuse; dFoo ]
            [ linkerAlice; uFoo; dFoo; linkerBob ]

    [<Test>]
    member __.TestSkipToLastLinker() =
        // With linkers A,B,C available and only A and a last linker needed, we should choose A and C (not A and B)
        // This effectively implements the 'put D9 at the end' rule
        runOne
            "FuseTwoNormalSlices"
            false  // is stitch
            ([ linkerAlice
               linkerBob
               linkerCharlie ],
             [])  // A and B part linkers
            [ uFoo; fuse; dFoo ]
            [ linkerAlice
              uFoo
              dFoo
              linkerCharlie ]

    [<Test>]
    /// BROKEN - this is a test case for https://gitlab.com/demetrixbio/DemGslc/-/issues/48
    member __.TerminalSlice() =

        runOne
            "TwoPartsShortInline"
            false  // is stitch
            ([ linkerAlice; linkerBob ], [])  // A and B part linkers
            [ uFoo
              fuse
              dFoo
              shortInlineWithRabitEnd ]
            [ linkerAlice
              uFoo
              dFoo
              shortInline
              linkerBob ]

    [<Test>]
    member __.InternalFuseThenInline() =

        runOne
            "InternalFuseThenInline"
            false  // is stitch
            ([ linkerAlice
               linkerBob
               linkerCharlie
               linkerDoug ],
             [])  // A and B part linkers
            [ uFoo
              fuse
              dFoo
              shortInlineWithRabitStart
              oBar ]
            [ linkerAlice
              uFoo
              dFoo
              linkerBob
              shortInlineWithRabitStart
              oBar
              linkerDoug ]

    [<Test>]
    member __.``Last AMP with rabitstart + rabitend``() =

        runOne
            "Last AMP with rabitstart + rabitend"
            false  // is stitch
            ([ linkerAlice
               linkerBob
               linkerCharlie
               linkerDoug ],
             [])  // A and B part linkers
            [ uFoo
              fuse
              dFoo
              shortInlineWithRabitStart
              oBar
              shortInlineWithRabitEnd ]
            [ linkerAlice
              uFoo
              dFoo
              linkerBob
              shortInlineWithRabitStart
              oBar
              shortInlineWithRabitEnd
              linkerDoug ]


    [<Test>]
    member __.inlineFusedExample1() =
        // this case is interesting - it broke things at the time (fixed)
        // but the equivalent raw GSL doesn't cause an issue.  I think because the
        // fuse is getting stripped out early on. Still a decent case we should cover
        runOne
            "inlineFusedExample1"
            false
            ([ linkerAlice; linkerBob ], [])
            [ uFoo; fuse; shortInline; dFoo ]
            [ linkerAlice
              uFoo
              shortInline
              dFoo
              linkerBob ]

    [<Test>]
    member __.inlineFusedExample2() =
        // This case is a more elaborate example where the fuse marked XXX is causing a linker to get inserted (fixed)
        runOne
            "inlineFusedExample2"
            false
            ([ linkerAlice
               linkerBob
               linkerCharlie
               linkerDoug ],
             [])
            [ uFoo
              oBar
              fuse
              uFoo
              fuse (* XXX *)
              shortInline
              dFoo
              shortInline
              oBar
              fuse
              uFoo
              shortInlineWithRabitStart
              dFoo ]
            [ linkerAlice
              uFoo
              linkerBob
              oBar
              uFoo
              shortInline
              dFoo
              shortInline
              oBar
              uFoo
              linkerCharlie
              shortInlineWithRabitStart
              dFoo
              linkerDoug ]

    [<Test>]
    member __.inlineFusedExample3() =
        // similar to previous case but omits fuse before shortInline
        runOne
            "inlineFusedExample3"
            false
            ([ linkerAlice
               linkerBob
               linkerCharlie
               linkerDoug ],
             [])
            [ uFoo
              pBaz
              fuse
              uFoo (* no fuse c.f inlineFusedExample2 *)
              shortInline
              dFoo
              shortInline
              oBar2
              fuse
              tShaz
              shortInlineWithRabitStart
              dFoo ]
            [ linkerAlice
              uFoo
              linkerBob
              pBaz
              uFoo
              shortInline
              dFoo
              shortInline
              oBar2
              tShaz
              linkerCharlie
              shortInlineWithRabitStart
              dFoo
              linkerDoug ]

    [<Test>]
    member __.internalInlineMarkhell2Case() =
        runOne
            "internalInlineMarkhell2Case"
            false
            ([ linkerAlice
               linkerBob
               linkerCharlie
               linkerDoug
               linkerEmma ],
             [])
            [ uFoo
              pBaz
              fuse
              oBar
              shortInlineWithRabitStart
              oBar2
              shortInlineWithRabitEnd
              tShaz ]
            [ linkerAlice
              uFoo
              linkerBob
              pBaz
              oBar
              linkerCharlie
              shortInlineWithRabitStart
              oBar2
              shortInlineWithRabitEnd
              linkerDoug
              tShaz
              linkerEmma ]
