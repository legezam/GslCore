module GslCore.Tests.Ast.Process.VariableResolutionTest

open System.Collections
open GslCore.Ast.Process
open GslCore.Ast.Types
open GslCore.GslResult
open NUnit.Framework
open GslCore.Ast.Process.VariableResolution

type TestCasesForTypeElision() =
    static member TestCasesForTypeElision: IEnumerable =
        seq {
            TestCaseData(Part
                             { Node.Value =
                                   { ParsePart.BasePart = Int({ Value = 12; Positions = [] })
                                     Modifiers = []
                                     Pragmas = []
                                     IsForward = false }
                               Positions = [] })
                .Returns(Some PartType)

            TestCaseData(FunctionLocals
                             { Node.Value = { FunctionLocals.Names = [ "bar"; "baz" ] }
                               Positions = [] })
                .Returns(None)

            TestCaseData(Int({ Value = 12; Positions = [] }))
                .Returns(Some IntType)

            TestCaseData(Float({ Value = 12.0; Positions = [] }))
                .Returns(Some FloatType)

            TestCaseData(String({ Value = "foo"; Positions = [] }))
                .Returns(Some StringType)
        } :> IEnumerable

[<TestCaseSource(typeof<TestCasesForTypeElision>, "TestCasesForTypeElision")>]
let testTypeElision (node: AstNode): GslVariableType option = VariableResolution.elideType node


type TestCasesForTypeCheck() =
    static member TestCasesForTypeCheck: IEnumerable =
        let testBoundValue = Int({ Value = 888; Positions = [] })

        let nonElidableValue =
            FunctionLocals
                { Node.Value = { FunctionLocals.Names = [ "bar"; "baz" ] }
                  Positions = [] }

        let elisionTypeLookup =
            [ PartType,
              Part
                  { Node.Value =
                        { ParsePart.BasePart = Int({ Value = 12; Positions = [] })
                          Modifiers = []
                          Pragmas = []
                          IsForward = false }
                    Positions = [] }

              IntType, Int({ Value = 12; Positions = [] })
              FloatType, Float({ Value = 12.0; Positions = [] })
              StringType, String({ Value = "foo"; Positions = [] }) ]
            |> Map.ofList

        let allPossibleTypes =
            [ FloatType
              PartType
              IntType
              StringType
              NotYetTyped ]

        let ``All types except NotYetTyped`` =
            [ FloatType
              PartType
              IntType
              StringType ]

        seq {
            // No matter what is the bound value type, if target type notyettyped it's a check
            for boundType in allPossibleTypes do
                TestCaseData({| TargetType = NotYetTyped
                                BoundValueType = boundType
                                BoundValue = testBoundValue |})
                    .Returns(GslResult.ok testBoundValue: GslResult<AstNode, TypeCheckResult>)

            // if bound and target type are the same it's a check
            for sameType in allPossibleTypes do
                TestCaseData({| TargetType = sameType
                                BoundValueType = sameType
                                BoundValue = testBoundValue |})
                    .Returns(GslResult.ok testBoundValue: GslResult<AstNode, TypeCheckResult>)

            for notNotYetTypedType in ``All types except NotYetTyped`` do
                // if neither bound or target type is specified then type elision does the job
                TestCaseData({| TargetType = notNotYetTypedType
                                BoundValueType = NotYetTyped
                                BoundValue = elisionTypeLookup.[notNotYetTypedType] |})
                    .Returns(GslResult.ok elisionTypeLookup.[notNotYetTypedType]: GslResult<AstNode, TypeCheckResult>)

                let nodeThatIsElidedToDifferentTypeThanTargetType =
                    elisionTypeLookup
                    |> Map.toList
                    |> List.find (fun lookupType -> fst lookupType <> notNotYetTypedType)

                let (elidedType, boundValue) =
                    nodeThatIsElidedToDifferentTypeThanTargetType

                // if elision resolves to a different type than target type, it's a fail
                TestCaseData({| TargetType = notNotYetTypedType
                                BoundValueType = NotYetTyped
                                BoundValue = boundValue |})
                    .Returns(GslResult.err (VariableTypeMismatch(elidedType, notNotYetTypedType)): GslResult<AstNode, TypeCheckResult>)

                // if elision fails then it's a fail
                TestCaseData({| TargetType = notNotYetTypedType
                                BoundValueType = NotYetTyped
                                BoundValue = nonElidableValue |})
                    .Returns(GslResult.err (InternalTypeMismatch(nonElidableValue, notNotYetTypedType)): GslResult<AstNode, TypeCheckResult>)

                for notNotYetTypedType2 in ``All types except NotYetTyped`` do
                    if notNotYetTypedType <> notNotYetTypedType2 then
                        TestCaseData({| TargetType = notNotYetTypedType
                                        BoundValueType = notNotYetTypedType2
                                        BoundValue = testBoundValue |})
                            .Returns(GslResult.err
                                         (VariableTypeMismatch(notNotYetTypedType2, notNotYetTypedType)): GslResult<AstNode, TypeCheckResult>)

        } :> IEnumerable

[<TestCaseSource(typeof<TestCasesForTypeCheck>, "TestCasesForTypeCheck")>]
let testTypeCheck (input: {| TargetType: GslVariableType
                             BoundValueType: GslVariableType
                             BoundValue: AstNode |})
                  : GslResult<AstNode, TypeCheckResult> =
    VariableResolution.typeCheck input.TargetType input.BoundValueType input.BoundValue
