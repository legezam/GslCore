module GslCore.Tests.Ast.Process.RelativePositionTranslationTest

open System.Collections
open GslCore.Ast.Types
open GslCore.Constants
open GslCore.GslResult
open NUnit.Framework
open GslCore.Ast.Process.RelativePositionTranslation

type TestCasesForCalculatePosition() =
    static member TestCasesForCalculatePosition: IEnumerable =
        seq {
            for position in [ -2; -1; 1; 2 ] do
                let pos = position * 1<OneOffset>

                TestCaseData(pos, None, Left)
                    .Returns(GslResult.ok (pos, FivePrime): GslResult<_, CalculationMessage>)

                TestCaseData(pos, Some S, Left)
                    .Returns(GslResult.ok (pos, FivePrime): GslResult<_, CalculationMessage>)

                TestCaseData(pos, Some S, Right)
                    .Returns(GslResult.ok (pos, FivePrime): GslResult<_, CalculationMessage>)

                TestCaseData(pos, Some E, Left)
                    .Returns(GslResult.ok (pos, ThreePrime): GslResult<_, CalculationMessage>)

                TestCaseData(pos, Some E, Right)
                    .Returns(GslResult.ok (pos, ThreePrime): GslResult<_, CalculationMessage>)

            for qualifier in [ A; AS; SA ] do
                let relPosition = Left

                TestCaseData(1<OneOffset>, Some qualifier, relPosition)
                    .Returns(GslResult.ok (1<OneOffset>, FivePrime): GslResult<_, CalculationMessage>)

                TestCaseData(2<OneOffset>, Some qualifier, relPosition)
                    .Returns(GslResult.ok (4<OneOffset>, FivePrime): GslResult<_, CalculationMessage>)

                TestCaseData(3<OneOffset>, Some qualifier, relPosition)
                    .Returns(GslResult.ok (7<OneOffset>, FivePrime): GslResult<_, CalculationMessage>)

                TestCaseData(0<OneOffset>, Some qualifier, relPosition)
                    .Returns(GslResult.err (PositionCannotBeZero): GslResult<int<OneOffset> * GeneEnd, _>)

                TestCaseData(-1<OneOffset>, Some qualifier, relPosition)
                    .Returns(GslResult.err (NegativeLeftAminoAcidStartPosition(-1<OneOffset>)): GslResult<int<OneOffset> * GeneEnd, _>)

            for qualifier in [ AE; EA ] do
                let relPosition = Left

                TestCaseData(-3<OneOffset>, Some qualifier, relPosition)
                    .Returns(GslResult.ok (-9<OneOffset>, ThreePrime): GslResult<_, CalculationMessage>)

                TestCaseData(-2<OneOffset>, Some qualifier, relPosition)
                    .Returns(GslResult.ok (-6<OneOffset>, ThreePrime): GslResult<_, CalculationMessage>)

                TestCaseData(-1<OneOffset>, Some qualifier, relPosition)
                    .Returns(GslResult.ok (-3<OneOffset>, ThreePrime): GslResult<_, CalculationMessage>)

                TestCaseData(0<OneOffset>, Some qualifier, relPosition)
                    .Returns(GslResult.err (PositionCannotBeZero): GslResult<int<OneOffset> * GeneEnd, _>)

                TestCaseData(1<OneOffset>, Some qualifier, relPosition)
                    .Returns(GslResult.ok (1<OneOffset>, ThreePrime): GslResult<_, CalculationMessage>)

                TestCaseData(2<OneOffset>, Some qualifier, relPosition)
                    .Returns(GslResult.ok (4<OneOffset>, ThreePrime): GslResult<_, CalculationMessage>)

                TestCaseData(3<OneOffset>, Some qualifier, relPosition)
                    .Returns(GslResult.ok (7<OneOffset>, ThreePrime): GslResult<_, CalculationMessage>)

            for qualifier in [ A; AS; SA ] do
                let relPosition = Right

                TestCaseData(-2<OneOffset>, Some qualifier, relPosition)
                    .Returns(GslResult.err (NegativeRightAminoAcidStartPosition(-2<OneOffset>)): GslResult<int<OneOffset> * GeneEnd, _>)

                TestCaseData(-1<OneOffset>, Some qualifier, relPosition)
                    .Returns(GslResult.err (NegativeRightAminoAcidStartPosition(-1<OneOffset>)): GslResult<int<OneOffset> * GeneEnd, _>)

                TestCaseData(0<OneOffset>, Some qualifier, relPosition)
                    .Returns(GslResult.err (PositionCannotBeZero): GslResult<int<OneOffset> * GeneEnd, _>)

                TestCaseData(1<OneOffset>, Some qualifier, relPosition)
                    .Returns(GslResult.ok (3<OneOffset>, FivePrime): GslResult<_, CalculationMessage>)

                TestCaseData(2<OneOffset>, Some qualifier, relPosition)
                    .Returns(GslResult.ok (6<OneOffset>, FivePrime): GslResult<_, CalculationMessage>)

                TestCaseData(3<OneOffset>, Some qualifier, relPosition)
                    .Returns(GslResult.ok (9<OneOffset>, FivePrime): GslResult<_, CalculationMessage>)

            for qualifier in [ AE; EA ] do
                let relPosition = Right

                TestCaseData(-2<OneOffset>, Some qualifier, relPosition)
                    .Returns(GslResult.ok (-4<OneOffset>, ThreePrime): GslResult<_, CalculationMessage>)

                TestCaseData(-1<OneOffset>, Some qualifier, relPosition)
                    .Returns(GslResult.ok (-1<OneOffset>, ThreePrime): GslResult<_, CalculationMessage>)

                TestCaseData(0<OneOffset>, Some qualifier, relPosition)
                    .Returns(GslResult.err (PositionCannotBeZero): GslResult<int<OneOffset> * GeneEnd, _>)

                TestCaseData(1<OneOffset>, Some qualifier, relPosition)
                    .Returns(GslResult.ok (3<OneOffset>, ThreePrime): GslResult<_, CalculationMessage>)

                TestCaseData(2<OneOffset>, Some qualifier, relPosition)
                    .Returns(GslResult.ok (6<OneOffset>, ThreePrime): GslResult<_, CalculationMessage>)

                TestCaseData(3<OneOffset>, Some qualifier, relPosition)
                    .Returns(GslResult.ok (9<OneOffset>, ThreePrime): GslResult<_, CalculationMessage>)
        } :> IEnumerable

[<TestCaseSource(typeof<TestCasesForCalculatePosition>, "TestCasesForCalculatePosition")>]
let ``Test if calculatePosition returns expected results`` (position: int<OneOffset>,
                                                            maybeQualifier: RelPosQualifier option,
                                                            relPosition: RelPosPosition)
                                                           : GslResult<int<OneOffset> * GeneEnd, CalculationMessage> =
    RelativePositionTranslation.calculatePosition position maybeQualifier relPosition

type TestCasesForGetProvidedPosition() =
    static member TestCasesForGetProvidedPosition: IEnumerable =
        seq {
            TestCaseData({ ParseRelativePosition.Item = AstNode.Int({ Node.Value = 12; Positions = [] })
                           Qualifier = None
                           Position = Left })
                .Returns(GslResult.ok 12: GslResult<_, RelativePositionTranslationMessage>)

            TestCaseData({ ParseRelativePosition.Item = AstNode.Int({ Node.Value = 12; Positions = [] })
                           Qualifier = None
                           Position = Right })
                .Returns(GslResult.ok 12: GslResult<_, RelativePositionTranslationMessage>)

            for qualifier in [ S; E; AS; AE; EA; SA; A ] do
                TestCaseData({ ParseRelativePosition.Item = AstNode.Int({ Node.Value = 12; Positions = [] })
                               Qualifier = Some qualifier
                               Position = Left })
                    .Returns(GslResult.ok 12: GslResult<_, RelativePositionTranslationMessage>)

                TestCaseData({ ParseRelativePosition.Item = AstNode.Int({ Node.Value = 12; Positions = [] })
                               Qualifier = Some qualifier
                               Position = Right })
                    .Returns(GslResult.ok 12: GslResult<_, RelativePositionTranslationMessage>)

            TestCaseData({ ParseRelativePosition.Item = AstNode.Float({ Node.Value = 12.0; Positions = [] })
                           Qualifier = None
                           Position = Right })
                .Returns(GslResult.err (PositionIsNotInteger(AstNode.Float({ Node.Value = 12.0; Positions = [] }))): GslResult<int, _>)

        } :> IEnumerable

[<TestCaseSource(typeof<TestCasesForGetProvidedPosition>, "TestCasesForGetProvidedPosition")>]
let ``Test if getProvidedPosition returns expected results`` (position: ParseRelativePosition)
                                                             : GslResult<int, RelativePositionTranslationMessage> =
    RelativePositionTranslation.getProvidedPosition position
