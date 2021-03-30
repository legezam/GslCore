﻿namespace GslCore.Tests


open GslCore
open GslCore.Ast.Phase1
open GslCore.GslResult
open GslCore.Ast
open GslCore.Core.Expansion
open GslCore.Pragma
open NUnit.Framework
open GslCore.AstAssertions
open GslCore.Ast.Algorithms
open GslCore.Ast.ErrorHandling
open GslCore.Constants
open GslCore.Core.Expansion.Level2Expansion

[<TestFixture>]
[<Category("Integration")>]
type TestL2Expansion() =


    let phase1WithL2Validation =
        (Phase1.phase1 AssemblyTestSupport.defaultPhase1Parameters
         >> GslResult.mapError Phase1Error.toAstMessage)
        >=> (Validation.validate Level2Expansion.validateNoAssemblyInL2Promoter)

    [<Test>]
    member x.TestDetectAssemblyInL2Promoter1() =
        let errorText = "Unsupported use of an Assembly."

        let source =
            """
(!gERG10 ; !pFBA1 ; pSLN1)>gADH1"""
            |> GslSourceCode

        let errors =
            compile phase1WithL2Validation source
            |> GslResult.assertError

        match errors with
        | Choice2Of2 message ->
            Assert.AreEqual(AstMessageType.L2ExpansionError, message.Type)
            Assert.IsTrue(message.Message.Contains errorText)
        | x -> Assert.Fail(sprintf "Expected AstMessage, got %O instead" x)
        
    [<Test>]
    member x.TestDetectAssemblyInL2Promoter2() =
        let errorText = "Unsupported use of an Assembly."

        let source =
            """
let myAssembly = (pSLN1; mERG10 ; pTDH3)
&myAssembly>gADH1"""
            |> GslSourceCode

        let errors =
            compile phase1WithL2Validation source
            |> GslResult.assertError

        match errors with
        | Choice2Of2 message ->
            Assert.AreEqual(AstMessageType.L2ExpansionError, message.Type)
            Assert.IsTrue(message.Message.Contains errorText)
        | x -> Assert.Fail(sprintf "Expected AstMessage, got %O instead" x)
        
    [<Test>]
    member x.TestDetectAssemblyInL2Promoter3() =
        let errorText = "Unsupported use of an Assembly."

        let source =
            """
let myAssembly = (pSLN1; mERG10 ; pTDH3)
gHO^ ; &myAssembly>gACS1"""
            |> GslSourceCode

        let errors =
            compile phase1WithL2Validation source
            |> GslResult.assertError

        match errors with
        | Choice2Of2 message ->
            Assert.AreEqual(AstMessageType.L2ExpansionError, message.Type)
            Assert.IsTrue(message.Message.Contains errorText)
        | x -> Assert.Fail(sprintf "Expected AstMessage, got %O instead" x)           

    [<Test>]
    member x.TestInlineL2PromoterSwap() =
        let source =
            GslSourceCode("!gERG10 ; !pFBA1 ; pSLN1>gADH1")

        let error =
            compile phase1WithL2Validation source
            |> GslResult.assertError

        match error with
        | Choice1Of2 _message -> Assert.Pass()
        | x -> Assert.Fail(sprintf "Expected AstMessage, got %O instead" x)



    [<Test>]
    member x.TestNativeL2PromoterSwap() =
        let source =
            """
pTDH3>gADH1
let myPromoter = pFBA1
&myPromoter>gSLN1
gHO^ ; pGAL1>mERG10"""
            |> GslSourceCode

        compile phase1WithL2Validation source
        |> GslResult.assertOk
        |> ignore

    [<Test>]
    member x.TestNativeL2PromoterSwapLinkers() =
        let source =
            """
pTDH3>gADH1
#linkers 0,2,A,3,9|0,B,4,9
pTDH3>gADH1"""
            |> GslSourceCode

        compile phase1WithL2Validation source
        |> GslResult.assertOk
        |> ignore


    [<Test>]
    member x.TestRabitL2PromoterSwap() =
        let source =
            """
@R41811>gADH1
let myPromoter = @R56707
&myPromoter>gSLN1
gHO^ ; @R52888>mERG10"""
            |> GslSourceCode

        compile phase1WithL2Validation source
        |> GslResult.assertOk
        |> ignore


    [<Test>]
    member x.TestCustomSeqL2PromoterSwap() =
        let source =
            """
let myPromoterAA = /$GTACVMPLQVGSASKYWALKERMYYQACLPH/
let myPromoterDNA = /GATCGATATTACGATGTAGTCGTAGTAGCTGTATATATATGCTGATGCTTCTAATAGCTAGC/
&myPromoterAA>gSLN1
gHO^ ; &myPromoterDNA>mERG10
/GATCGTATGCTTGGATCGTGATGTGCTGATCGACTGTAC/>gADH1
gDIT1^; /$PLQVGSASKYWALKERMYYQACLPH/>gACS1"""
            |> GslSourceCode

        compile phase1WithL2Validation source
        |> GslResult.assertOk
        |> ignore
