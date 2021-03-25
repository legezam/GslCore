module GslCore.Tests.Ast.Process.FunctionInliningTest

open System.Collections
open System.Diagnostics
open GslCore.Ast.Algorithms
open GslCore.Ast.Process.Inlining
open GslCore.Ast.Process.VariableResolution
open GslCore.Ast.Types
open GslCore.AstAssertions
open GslCore.GslResult
open NUnit.Framework

let createTestNode (value: 'a): Node<'a> = { Node.Value = value; Positions = [] }

[<Test>]
[<Category("Phase1")>]
let ``checkArguments results in ok if same number of parameters are provided`` () =
    let parseFunction =
        { ParseFunction.Name = "foo"
          ArgumentNames = [ "param" ]
          Body = AstNode.String(createTestNode "") }

    let functionCall =
        { FunctionCall.Name = "foo"
          Arguments = [ AstNode.String(createTestNode "doesn't matter") ] }

    Inlining.checkArguments parseFunction functionCall
    |> GslResult.assertOk

[<Test>]
[<Category("Phase1")>]
let ``checkArguments results in error if different number of parameters are provided`` () =
    let parseFunction =
        { ParseFunction.Name = "foo"
          ArgumentNames = [ "param1"; "param2" ]
          Body = AstNode.String { Node.Value = ""; Positions = [] } }

    let functionCall =
        { FunctionCall.Name = "foo"
          Arguments = [ AstNode.String(createTestNode "doesn't matter") ] }

    let result =
        Inlining.checkArguments parseFunction functionCall
        |> GslResult.assertError

    Assert.AreEqual(FunctionValidationError.ParameterNumberMismatch(functionCall, 2, 1), result)

let mockResolveVariable (result: AstNode) (_node: AstNode): NodeTransformResult<VariableResolutionError> =
    GslResult.ok result

let mockResolveVariableWithAssert (parameter: AstNode)
                                  (result: AstNode)
                                  (node: AstNode)
                                  : NodeTransformResult<VariableResolutionError> =
    Assert.AreEqual(parameter, node)
    GslResult.ok result

type TestCasesForVariableBindingFromTypedValueAndName() =
    static member TestCasesForVariableBindingFromTypedValueAndName: IEnumerable =
        let defaultCase =
            let typedVariableToResolve =
                AstNode.TypedVariable(createTestNode ("fooVariable", GslVariableType.NotYetTyped))

            let resolvedValue = createTestNode "foo" |> AstNode.String

            let resultVariableBinding =
                { VariableBinding.Name = "fooParameter"
                  Type = GslVariableType.NotYetTyped
                  Value = resolvedValue }

            TestCaseData({| ResolveVariable = mockResolveVariableWithAssert typedVariableToResolve resolvedValue
                            Name = "fooParameter"
                            Value =
                                AstNode.TypedValue(createTestNode (GslVariableType.NotYetTyped, typedVariableToResolve)) |})

                .Returns(resultVariableBinding
                         |> createTestNode
                         |> AstNode.VariableBinding
                         |> GslResult.ok: GslResult<AstNode, InliningError>)
                .SetName("Default case, function parameter gets inlined as a local variable with the name of the TypedVariable")

        let typingCases =
            [ let variableTypes =
                [ GslVariableType.FloatType
                  GslVariableType.IntType
                  GslVariableType.PartType
                  GslVariableType.StringType
                  GslVariableType.NotYetTyped ]

              for variableType in variableTypes do
                  let typedVariableToResolve =
                      AstNode.TypedVariable(createTestNode ("fooVariable", GslVariableType.NotYetTyped))

                  let resolvedValue = createTestNode "foo" |> AstNode.String

                  let resultVariableBinding =
                      { VariableBinding.Name = "fooParameter"
                        Type = variableType
                        Value = resolvedValue }

                  TestCaseData({| ResolveVariable = mockResolveVariable resolvedValue
                                  Name = "fooParameter"
                                  Value = AstNode.TypedValue(createTestNode (variableType, typedVariableToResolve)) |})

                      .Returns(resultVariableBinding
                               |> createTestNode
                               |> AstNode.VariableBinding
                               |> GslResult.ok: GslResult<AstNode, InliningError>)
                      .SetName(sprintf
                                   "Resulting variable binding will receive the same type as the TypedValue: %A"
                                   variableType) ]

        let inputFormatCase =
            let ``Doesn't get called`` (_node: AstNode): NodeTransformResult<VariableResolutionError> =
                failwith "Illegal"


            let illegalNode =
                createTestNode "Illegal" |> AstNode.String

            TestCaseData({| ResolveVariable = ``Doesn't get called``
                            Name = "doesn't matter"
                            Value = illegalNode |})

                .Returns(GslResult.err (InliningError.FunctionCallTypeMismatch illegalNode): GslResult<AstNode, InliningError>)
                .SetName("Passed parameter to a function must be a TypedValue")

        let resolutionErrorCases =
            [ let resolutionErrors =
                let variableName = "varName"
                let node = AstNode.String(createTestNode "foo")

                [ VariableResolutionError.IllegalFunctionLocal(variableName, node)
                  VariableResolutionError.UnresolvedVariable(variableName, node)
                  VariableResolutionError.TypeCheck
                      (TypeCheckError.ElisionConflict(GslVariableType.IntType, GslVariableType.FloatType),
                       node,
                       variableName)
                  VariableResolutionError.TypeCheck
                      (TypeCheckError.CannotElide(node, GslVariableType.IntType), node, variableName) ]

              let inputNode =
                  AstNode.TypedValue
                      (createTestNode (GslVariableType.IntType, AstNode.String(createTestNode "doesn't matter")))

              for resolutionError in resolutionErrors do
                  let resolveVariable (_node: AstNode): NodeTransformResult<VariableResolutionError> =
                      GslResult.err resolutionError

                  TestCaseData({| ResolveVariable = resolveVariable
                                  Name = "doesn't matter"
                                  Value = inputNode |})

                      .Returns(GslResult.err (InliningError.VariableResolution resolutionError): GslResult<AstNode, InliningError>)
                      .SetName(sprintf "Variable resolution error is respected: %A" resolutionError) ]
        [ yield defaultCase
          yield! typingCases
          yield inputFormatCase
          yield! resolutionErrorCases ] :> IEnumerable

[<TestCaseSource(typeof<TestCasesForVariableBindingFromTypedValueAndName>,
                 "TestCasesForVariableBindingFromTypedValueAndName")>]
[<Category("Phase1")>]
let createVariableBindingFromTypedValueAndName (input: {| ResolveVariable: AstNode -> NodeTransformResult<VariableResolutionError>
                                                          Name: string
                                                          Value: AstNode |})
                                               : GslResult<AstNode, InliningError> =
    Inlining.createVariableBindingFromTypedValueAndName input.ResolveVariable input.Name input.Value


type TestCasesForInlinePassedArguments() =
    static member TestCasesForInlinePassedArguments: IEnumerable =
        let functionBodyIsNotBlock =
            let functionBody =
                AstNode.String(createTestNode "doesn't matter")

            let parseFunction =
                { ParseFunction.Body = functionBody
                  Name = "function name"
                  ArgumentNames = [] }

            let functionCall =
                { FunctionCall.Name = "function name"
                  Arguments = [] }

            let createVariableBinding: string -> AstNode -> GslResult<AstNode, InliningError> =
                fun _ _ -> failwith "illegal state"

            let result: GslResult<AstNode, InliningError> =
                GslResult.err (InliningError.FunctionBodyTypeMismatch functionBody)

            TestCaseData({| CreateVariableBinding = createVariableBinding
                            ParseFunction = parseFunction
                            FunctionCall = functionCall |})
                .Returns(result)
                .SetName("Function body must be a Block")

        let functionBodyIsEmptyBlock =
            let functionBody = [] |> createTestNode |> AstNode.Block

            let parseFunction =
                { ParseFunction.Body = functionBody
                  Name = "function name"
                  ArgumentNames = [] }

            let functionCall =
                { FunctionCall.Name = "function name"
                  Arguments = [] }

            let createVariableBinding: string -> AstNode -> GslResult<AstNode, InliningError> =
                fun _ _ -> failwith "illegal state"

            let result: GslResult<AstNode, InliningError> =
                GslResult.err (InliningError.MissingFunctionBody(parseFunction, functionBody))

            TestCaseData({| CreateVariableBinding = createVariableBinding
                            ParseFunction = parseFunction
                            FunctionCall = functionCall |})
                .Returns(result)
                .SetName("Function body must be a non empty Block")

        let functionBodyMustStartWithFunctionLocals =
            let functionBody =
                [ AstNode.String(createTestNode "foo") ]
                |> createTestNode
                |> AstNode.Block

            let parseFunction =
                { ParseFunction.Body = functionBody
                  Name = "function name"
                  ArgumentNames = [] }

            let functionCall =
                { FunctionCall.Name = "function name"
                  Arguments = [] }

            let createVariableBinding: string -> AstNode -> GslResult<AstNode, InliningError> =
                fun _ _ -> failwith "illegal state"

            let result: GslResult<AstNode, InliningError> =
                GslResult.err (InliningError.MissingFunctionLocals(parseFunction))

            TestCaseData({| CreateVariableBinding = createVariableBinding
                            ParseFunction = parseFunction
                            FunctionCall = functionCall |})
                .Returns(result)
                .SetName("Function body block must start with FunctionLocals node")

        let removesFunctionLocals =
            let functionBodyNodes =
                [ AstNode.FunctionLocals(createTestNode { FunctionLocals.Names = [] }) ]

            let functionBody =
                functionBodyNodes
                |> createTestNode
                |> AstNode.Block

            let resultFunctionBody =
                List.empty |> createTestNode |> AstNode.Block

            let parseFunction =
                { ParseFunction.Body = functionBody
                  Name = "function name"
                  ArgumentNames = [] }

            let functionCall =
                { FunctionCall.Name = "function name"
                  Arguments = [] }

            let createVariableBinding: string -> AstNode -> GslResult<AstNode, InliningError> =
                fun _ _ -> failwith "illegal state"

            let result: GslResult<AstNode, InliningError> = GslResult.ok resultFunctionBody

            TestCaseData({| CreateVariableBinding = createVariableBinding
                            ParseFunction = parseFunction
                            FunctionCall = functionCall |})
                .Returns(result)
                .SetName("Inlining removes function locals node")

        let restOfBodyRemainsIntact =
            let persistingBodyMember = createTestNode "foo" |> AstNode.String

            let functionBodyNodes =
                [ AstNode.FunctionLocals(createTestNode { FunctionLocals.Names = [] })
                  persistingBodyMember ]

            let functionBody =
                functionBodyNodes
                |> createTestNode
                |> AstNode.Block

            let resultFunctionBody =
                [ persistingBodyMember ]
                |> createTestNode
                |> AstNode.Block

            let parseFunction =
                { ParseFunction.Body = functionBody
                  Name = "function name"
                  ArgumentNames = [] }

            let functionCall =
                { FunctionCall.Name = "function name"
                  Arguments = [] }

            let createVariableBinding: string -> AstNode -> GslResult<AstNode, InliningError> =
                fun _ _ -> failwith "illegal state"

            let result: GslResult<AstNode, InliningError> = GslResult.ok resultFunctionBody

            TestCaseData({| CreateVariableBinding = createVariableBinding
                            ParseFunction = parseFunction
                            FunctionCall = functionCall |})
                .Returns(result)
                .SetName("Inlining does not modify non FunctionLocal nodes in body")

        let argumentInlining =
            let functionBody =
                // The actual names in the FunctionLocals are getting ignored completely. FunctionLocals really acts as a placeholder
                // for the inlined variable bindings
                [ AstNode.FunctionLocals(createTestNode { FunctionLocals.Names = List.empty }) ]
                |> createTestNode
                |> AstNode.Block

            let arguments =
                [ for index in 0 .. 3 do
                    {| Name = sprintf "arg%d" index
                       Value =
                           createTestNode (sprintf "argumentNode%d" index)
                           |> AstNode.String
                       ResolvedValue =
                           createTestNode (sprintf "argumentResolutionResult%d" index)
                           |> AstNode.String |} ]

            let resultBlock =
                [ for argument in arguments -> argument.ResolvedValue ]
                |> createTestNode
                |> AstNode.Block

            let parseFunction =
                { ParseFunction.Body = functionBody
                  Name = "function name"
                  ArgumentNames = [ for argument in arguments -> argument.Name ] }

            let functionCall =
                { FunctionCall.Name = "function name"
                  Arguments = [ for argument in arguments -> argument.Value ] }

            let argumentLookup =
                arguments
                |> List.map (fun arg -> arg.Name, arg)
                |> Map.ofList

            let createVariableBinding (variableName: string)
                                      (variableArgument: AstNode)
                                      : GslResult<AstNode, InliningError> =
                match argumentLookup |> Map.tryFind variableName with
                | None ->
                    Assert.Fail(sprintf "variable is missing from lookup: %s" variableName)
                    failwith "impossible"
                | Some argument ->
                    Assert.AreEqual(argument.Value, variableArgument)
                    argument.ResolvedValue |> GslResult.ok

            let result: GslResult<AstNode, InliningError> = GslResult.ok resultBlock

            TestCaseData({| CreateVariableBinding = createVariableBinding
                            ParseFunction = parseFunction
                            FunctionCall = functionCall |})
                .Returns(result)
                .SetName("Inlining does inline multiple arguments using variable binding creation")

        let argumentInliningError =
            let functionBody =
                [ AstNode.FunctionLocals(createTestNode { FunctionLocals.Names = List.empty }) ]
                |> createTestNode
                |> AstNode.Block

            let arguments =
                [ for index in 0 .. 3 do
                    {| Name = sprintf "arg%d" index
                       Value =
                           createTestNode (sprintf "argumentNode%d" index)
                           |> AstNode.String
                       ResolvedError =
                           InliningError.FunctionCallTypeMismatch
                               (createTestNode (sprintf "ErrorNode%d" index)
                                |> AstNode.String) |} ]

            let parseFunction =
                { ParseFunction.Body = functionBody
                  Name = "function name"
                  ArgumentNames = [ for argument in arguments -> argument.Name ] }

            let functionCall =
                { FunctionCall.Name = "function name"
                  Arguments = [ for argument in arguments -> argument.Value ] }

            let argumentLookup =
                arguments
                |> List.map (fun arg -> arg.Name, arg)
                |> Map.ofList

            let createVariableBinding (variableName: string)
                                      (variableArgument: AstNode)
                                      : GslResult<AstNode, InliningError> =
                match argumentLookup |> Map.tryFind variableName with
                | None ->
                    Assert.Fail(sprintf "variable is missing from lookup: %s" variableName)
                    failwith "impossible"
                | Some argument ->
                    Assert.AreEqual(argument.Value, variableArgument)
                    GslResult.err argument.ResolvedError

            let result: GslResult<AstNode, InliningError> =
                Error [ for argument in arguments -> argument.ResolvedError ]
                |> GslResult.Create

            TestCaseData({| CreateVariableBinding = createVariableBinding
                            ParseFunction = parseFunction
                            FunctionCall = functionCall |})
                .Returns(result)
                .SetName("Inlining respects errors in binding creation in an applicative manner")
        [ functionBodyIsNotBlock
          functionBodyIsEmptyBlock
          functionBodyMustStartWithFunctionLocals
          removesFunctionLocals
          restOfBodyRemainsIntact
          argumentInlining
          argumentInliningError ] :> IEnumerable

[<TestCaseSource(typeof<TestCasesForInlinePassedArguments>, "TestCasesForInlinePassedArguments")>]
[<Category("Phase1")>]
let inlinePassedArguments (input: {| CreateVariableBinding: string -> AstNode -> GslResult<AstNode, InliningError>
                                     ParseFunction: ParseFunction
                                     FunctionCall: FunctionCall |})
                          : GslResult<AstNode, InliningError> =
    Inlining.inlinePassedArguments input.CreateVariableBinding input.ParseFunction input.FunctionCall

/// Tests this simple scenario:
/// let myFunc(myArg) =
///     /GCAT/
/// end
/// let myVariable = "hello"
/// myFunc(&myVariable)
/// Should resolve to:
/// DO
/// let myArg = "hello"
/// /GCAT/
/// END
[<Test>]
[<Category("Phase1")>]
[<Category("Integration")>]
let testIntegration () =
    let someBodyContent = AstNode.InlineDna(createTestNode "GCAT")

    let myFuncDefinition =
        /// let myFunc(myArg) =
        ///     /GCAT/
        /// end
        { ParseFunction.Name = "myFunc"
          ArgumentNames = [ "myArg" ]
          Body =
              AstNode.Block
                  (createTestNode [ AstNode.FunctionLocals(createTestNode { FunctionLocals.Names = [ "myArg" ] })
                                    someBodyContent ]) }

    let myVariableBinding =
        /// let myVariable = "hello"
        { VariableBinding.Name = "myVariable"
          Type = GslVariableType.StringType
          Value = createTestNode "hello" |> AstNode.String }

    let myVariableDefinition =
        /// let myVariable = "hello"
        (createTestNode myVariableBinding)

    let myVariableDefinitionNode =
        /// let myVariable = "hello"
        AstNode.VariableBinding myVariableDefinition

    let inliningState =
        { FunctionInliningState.Definitions = [ "myFunc", myFuncDefinition ] |> Map.ofList
          Variables =
              [ "myVariable", VariableResolutionWrapper.VariableBinding myVariableDefinition ]
              |> Map.ofList
          Depth = 0 }

    let inputNode =
        // myFunc(&myVariable)
        AstNode.FunctionCall
            (createTestNode
                { FunctionCall.Name = "myFunc"
                  Arguments =
                      [ AstNode.TypedValue
                          (createTestNode
                              (GslVariableType.StringType,
                               AstNode.TypedVariable(createTestNode ("myVariable", GslVariableType.StringType)))) ] })

    let actualResult =
        Inlining.inlineFunctionCall
            Inlining.checkArguments
            Inlining.inlineArgumentsUsingVariableResolution
            inliningState
            inputNode
            

    let inlinedState = actualResult |> GslResult.assertOk
    printfn "%s" (inlinedState |> AstNode.decompile)    

    let expectedResult: GslResult<AstNode, InliningError> =
        /// DO
        /// let myArg = "hello"
        /// /GCAT/
        /// END
        AstNode.Block
            (createTestNode [ AstNode.VariableBinding
                                  (createTestNode
                                      { myVariableBinding with
                                            Name = "myArg" })
                              someBodyContent ])
        |> GslResult.ok

    Assert.AreEqual(expectedResult, actualResult)
    
