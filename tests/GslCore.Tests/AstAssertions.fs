/// Test helper functions and assertions.
module GslCore.AstAssertions

open System
open GslCore.GslResult
open NUnit.Framework
open GslCore.Ast.Types
open GslCore.Ast.ErrorHandling
open GslCore.Ast.Algorithms
open GslCore.AstFixtures
open GslCore.Ast
open GslCore.Constants

/// Lex and parse, in verbose mode.
let lexparse = LexAndParse.lexAndParse true


/// Assert that two trees are equal.  If they aren't, pretty print them.
/// Note that we never use source positions when comparing two AST nodes.
let assertTreesEqual (expected: AstTreeHead) (actual: AstTreeHead) =
    if expected <> actual then
        let dumpAst node =
            let items =
                AstNode.traverse expected
                |> Seq.map (fun n -> sprintf "%+A" n)
                |> List.ofSeq

            String.Join(", ", items)

        let msg =
            sprintf "ASTs were not equal.\nExpected:\n%s\n\nActual:\n%s\nExpected nodes:\n%+A\nActual nodes:\n%+A"
                (AstNode.decompile expected.wrappedNode) (AstNode.decompile actual.wrappedNode) (expected) (actual)

        Assert.Fail(msg)

/// Assert that passed tree decompiles to the provided source literal.
/// Optionally trim leading and trailing whitespace.
let assertDecompilesTo (source: string) (tree: AstTreeHead): unit =
    let treeAsText = AstNode.decompile tree.wrappedNode

    let cleanString (s: string) = s.Trim().Replace("\r\n", "\n")

    let expected, actual =
        cleanString source, cleanString treeAsText

    // search for where the two strings differ, if they do
    let diffPos =
        Seq.zip (Seq.cast<char> expected) (Seq.cast<char> actual)
        |> Seq.indexed
        |> Seq.tryPick (fun (i, (ec, ac)) -> if ec <> ac then Some(i) else None)

    match diffPos with
    | Some (p) ->
        let printSplitAtFirstDiff (s: string) =
            sprintf "%s(###diff site###)%s" s.[..p - 1] s.[p..]

        let msg =
            (sprintf "Expected and actual source differs at character %d.\nExpected:\n%s\n\nActual:\n%s" p expected
                 (printSplitAtFirstDiff actual))

        Assert.Fail(msg)
    | None ->
        // if they were the same when zipped, make sure they were the same length!
        if expected.Length <> actual.Length then
            Assert.Fail
                (sprintf "Expected string with %d characters, got a string with %d instead.\nExpected:\n%s\n\nActual:\n%s"
                     expected.Length actual.Length expected actual)

/// Assert that source compiles to an AST with the same top level block contents as the list of items passed.
/// Also assert that the tree decompiles correctly to the same literal source which was passed in.
let assertRoundtrip source astItems =
    let tree =
        lexparse (GslSourceCode(source))
        |> GslResult.valueOr (failwithf "%A")

    assertTreesEqual (treeify astItems) tree
    assertDecompilesTo source tree


let compile (op: AstTreeHead -> GslResult<'a, 'b>) (source: GslSourceCode): GslResult<'a, Choice<LexParseError, 'b>> =
    (lexparse source |> GslResult.mapError Choice1Of2)
    >>= (op >> GslResult.mapError Choice2Of2)

///<summary>
/// Compare expected and reprinted source for a provided source sample.
/// The source is parsed, the tree is operated on by op, and the
/// resulting source is compared to the expected source.
///</summary>
let sourceCompareTest (op: AstTreeHead -> GslResult<AstTreeHead, 'b>) (sourceIn: string) (expectedSource: string): unit =
    let source = GslSourceCode sourceIn

    source
    |> compile op
    |> GslResult.valueOr (fun err ->
        Assert.Fail(sprintf "%A" err)
        failwith "impossible")
    |> assertDecompilesTo expectedSource

///<summary>
///Given GSL source code, parse it, reprint the AST, and compare
///to the expectation.
///</summary>
let testExpectedReprinting (sourceIn: string) (expectedOut: string) =
    sourceCompareTest (GslResult.promote id) sourceIn expectedOut

module GslResult =
    open FsToolkit.ErrorHandling

    let mapErrorToAnything (result: GslResult<'a, 'b>): GslResult<'a, 'c> =
        result
        |> GslResult.mapError (failwithf "Expected no error, got %A instead")

    let assertOkSuccess (result: GslResult<'a, 'b>) =
        result.Value
        |> Result.either id (fun errorMessages ->
               let message =
                   [ "Expected Ok, received Error instead:"
                     for message in errorMessages do
                         sprintf "%O" message ]
                   |> String.concat Environment.NewLine

               Assert.Fail message
               failwith "Impossible")

    let assertOk (result: GslResult<'a, 'b>): 'a =
        result
        |> assertOkSuccess
        |> fun success -> success.Result

    let assertWarnings (result: GslResult<'a, 'b>) =
        assertOkSuccess result
        |> fun success -> success.Warnings

    let assertWarning (result: GslResult<'a, 'b>) =
        assertOkSuccess result
        |> fun success -> success.Warnings |> List.head

    let assertErrors (result: GslResult<'a, 'b>) =
        match result.Value with
        | Ok success ->
            let message =
                [ "Expected Error, received success instead:"
                  success.ToString() ]
                |> String.concat Environment.NewLine

            Assert.Fail message
            failwith "Impossible"
        | Error messages -> messages

    let assertError (result: GslResult<'a, 'b>) =
        match result.Value with
        | Ok success ->
            let message =
                [ "Expected Error, received success instead:"
                  success.ToString() ]
                |> String.concat Environment.NewLine

            Assert.Fail message
            failwith "Impossible"
        | Error messages -> messages |> List.head
