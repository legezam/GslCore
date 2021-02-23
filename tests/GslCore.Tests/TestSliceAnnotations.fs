namespace GslCore.Tests

open NUnit.Framework
open GslCore.Ast.LegacyParseTypes
open GslCore.Constants
open GslCore.Core.Types

[<TestFixture>]
type TestOrfAnnotation() =

    let checkIndices (annotation: OrfAnnotation) (correct: int list) =
        Assert.AreEqual
            (correct |> List.map (fun x -> x * 1<ZeroOffset>), annotation.CompleteCodonIndices() |> List.ofSeq)

    [<Test>]
    member x.TestCodonIndexGeneration() =
        // test really basic functionality
        let simpleOrf =
            { Left = 0<ZeroOffset>
              Right = 7<ZeroOffset>
              FrameOffset = Zero
              IsForward = true }

        checkIndices simpleOrf [ 0; 3 ]

        let backwardsOrf = { simpleOrf with IsForward = false }
        checkIndices backwardsOrf [ 7; 4 ]

        let offsetOrf = { simpleOrf with FrameOffset = Two }
        checkIndices offsetOrf [ 1; 4 ]

        let backwardsOffsetOrf = { backwardsOrf with FrameOffset = Two }
        checkIndices backwardsOffsetOrf [ 6; 3 ]

        let tinyOrf =
            { backwardsOrf with
                  Right = 1<ZeroOffset> }

        checkIndices tinyOrf []

    [<Test>]
    member x.TestAnnotationFromSlice() =
        // default range for gene
        let basicOrfSlice =
            { Left = { Position = 1<OneOffset>; RelativeTo = FivePrime }
              LeftApprox = false
              RightApprox = false
              Right =
                  { Position = -1<OneOffset>
                    RelativeTo = ThreePrime } }

        let featLen = 100

        let orfAnnotationFwd =
            OrfAnnotation.orfAnnotationFromSlice basicOrfSlice featLen true Genomic

        Assert.AreEqual(0<ZeroOffset>, orfAnnotationFwd.Left)
        Assert.AreEqual(99<ZeroOffset>, orfAnnotationFwd.Right)
        checkIndices orfAnnotationFwd [ 0 .. 3 .. 97 ]
        Assert.That(orfAnnotationFwd.IsForward)

        let orfAnnotationRev =
            OrfAnnotation.orfAnnotationFromSlice basicOrfSlice featLen false Genomic

        Assert.AreEqual(0<ZeroOffset>, orfAnnotationRev.Left)
        Assert.AreEqual(99<ZeroOffset>, orfAnnotationRev.Right)
        checkIndices orfAnnotationRev [ 99 .. -3 .. 2 ]
        Assert.That(not orfAnnotationRev.IsForward)
