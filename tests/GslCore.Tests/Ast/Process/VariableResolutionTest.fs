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
            TestCaseData(AstNode.Part
                             { Node.Value =
                                   { ParsePart.BasePart = AstNode.Int({ Value = 12; Positions = [] })
                                     Modifiers = []
                                     Pragmas = []
                                     IsForward = false }
                               Positions = [] })
                .Returns(Some PartType)

            TestCaseData(AstNode.FunctionLocals
                             { Node.Value = { FunctionLocals.Names = [ "bar"; "baz" ] }
                               Positions = [] })
                .Returns(None)

            TestCaseData(AstNode.Int({ Value = 12; Positions = [] }))
                .Returns(Some IntType)

            TestCaseData(AstNode.Float({ Value = 12.0; Positions = [] }))
                .Returns(Some FloatType)

            TestCaseData(AstNode.String({ Value = "foo"; Positions = [] }))
                .Returns(Some StringType)
        } :> IEnumerable

[<TestCaseSource(typeof<TestCasesForTypeElision>, "TestCasesForTypeElision")>]
let testTypeElision (node: AstNode): GslVariableType option = VariableResolution.elideType node


type TestCasesForTypeCheck() =
    static member TestCasesForTypeCheck: IEnumerable =
        let testBoundValue =
            AstNode.Int({ Value = 888; Positions = [] })

        let nonElidableValue =
            AstNode.FunctionLocals
                { Node.Value = { FunctionLocals.Names = [ "bar"; "baz" ] }
                  Positions = [] }

        let elisionTypeLookup =
            [ PartType,
              AstNode.Part
                  { Node.Value =
                        { ParsePart.BasePart = AstNode.Int({ Value = 12; Positions = [] })
                          Modifiers = []
                          Pragmas = []
                          IsForward = false }
                    Positions = [] }

              IntType, AstNode.Int({ Value = 12; Positions = [] })
              FloatType, AstNode.Float({ Value = 12.0; Positions = [] })
              StringType, AstNode.String({ Value = "foo"; Positions = [] }) ]
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
                    .Returns(GslResult.ok testBoundValue: GslResult<AstNode, TypeCheckError>)

            // if bound and target type are the same it's a check
            for sameType in allPossibleTypes do
                TestCaseData({| TargetType = sameType
                                BoundValueType = sameType
                                BoundValue = testBoundValue |})
                    .Returns(GslResult.ok testBoundValue: GslResult<AstNode, TypeCheckError>)

            for notNotYetTypedType in ``All types except NotYetTyped`` do
                // if neither bound or target type is specified then type elision does the job
                TestCaseData({| TargetType = notNotYetTypedType
                                BoundValueType = NotYetTyped
                                BoundValue = elisionTypeLookup.[notNotYetTypedType] |})
                    .Returns(GslResult.ok elisionTypeLookup.[notNotYetTypedType]: GslResult<AstNode, TypeCheckError>)

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
                    .Returns(GslResult.err
                                 (TypeCheckError.ElisionConflict(elidedType, notNotYetTypedType)): GslResult<AstNode, TypeCheckError>)

                // if elision fails then it's a fail
                TestCaseData({| TargetType = notNotYetTypedType
                                BoundValueType = NotYetTyped
                                BoundValue = nonElidableValue |})
                    .Returns(GslResult.err (TypeCheckError.CannotElide(nonElidableValue, notNotYetTypedType)): GslResult<AstNode, TypeCheckError>)

                for notNotYetTypedType2 in ``All types except NotYetTyped`` do
                    if notNotYetTypedType <> notNotYetTypedType2 then
                        TestCaseData({| TargetType = notNotYetTypedType
                                        BoundValueType = notNotYetTypedType2
                                        BoundValue = testBoundValue |})
                            .Returns(GslResult.err
                                         (TypeCheckError.ElisionConflict
                                             (notNotYetTypedType2, notNotYetTypedType)): GslResult<AstNode, TypeCheckError>)

        } :> IEnumerable

[<TestCaseSource(typeof<TestCasesForTypeCheck>, "TestCasesForTypeCheck")>]
let testTypeCheck (input: {| TargetType: GslVariableType
                             BoundValueType: GslVariableType
                             BoundValue: AstNode |})
                  : GslResult<AstNode, TypeCheckError> =
    VariableResolution.typeCheck input.TargetType input.BoundValueType input.BoundValue
