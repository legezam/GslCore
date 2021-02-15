﻿namespace GslCore.Tests

open System
open GslCore
open GslCore.LegacyParseTypes
open NUnit.Framework
open GslCore.CommonTypes
open GslCore.AssemblyTestSupport
open GslCore.PragmaTypes
open Amyris.ErrorHandling

[<TestFixture>]
type TestMapRyseLinkers() =

    /// Enable for detailed (very detailed) output from mapRyseLinkers - useful for debugging test cases
    let verbose = false

    do
        // initialize pragmas
        PragmaTypes.finalizePragmas []

    let makePragma name values =
        match buildPragma name values with
        | Ok (p, []) ->
            let map =
                [ p.name, p ] |> Map.ofList |> PragmaCollection

            map
        | _ -> failwith "building pragma"
    /// perform one test and check output pattern and sequences
    let runOne (name: string) isMegastitch (linkersIn: (DNASlice list * DNASlice list)) slicesIn expected =
        /// Boring default options
        let opts: ParsedOptions =
            { quiet = false
              refStrain = "cenpk"
              libDir = "whatever"
              iter = true
              onlyPhase1 = false
              doParallel = false
              verbose = verbose
              noPrimers = false
              lexOnly = false
              refList = false
              refDump = None
              listPlugins = false
              doHelpPragmas = false
              isDeterministic = false }

        let leftLinkers, rightLinkers = linkersIn

        let linkers =
            [ for l in leftLinkers @ rightLinkers ->
                l.sliceName,
                { RYSELinker.name = l.sliceName
                  dna = l.dna } ]
            |> Map.ofList


        /// Wrap up in a generic assembly
        let assemblyIn: DnaAssembly =
            { id = None
              dnaParts = slicesIn
              name = name
              uri = None
              linkerHint =
                  String.Join(",", [ for l in leftLinkers -> l.sliceName ])
                  + "|"
                  + String.Join(",", [ for l in rightLinkers -> l.sliceName ])
              pragmas = if isMegastitch then EmptyPragmas else makePragma "platform" [ "stitch" ]
              designParams = DesignParams.initialDesignParams
              docStrings = []
              materializedFrom =
                  { Assembly.name = None
                    parts = []
                    uri = None
                    linkerHint = ""
                    pragmas = EmptyPragmas
                    designParams = DesignParams.initialDesignParams
                    capabilities = Set.empty
                    docStrings = []
                    sourcePosition = [] }
              tags = Set.empty
              topology = Linear }

        let updatedAssembly =
            Ryse.mapRyseLinkers opts Map.empty linkers assemblyIn
        // Check expected and actual slice output
        SharedSliceTesting.checkSequence expected updatedAssembly.dnaParts


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