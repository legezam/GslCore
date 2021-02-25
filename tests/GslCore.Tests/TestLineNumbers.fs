namespace GslCore.Tests

open System
open GslCore
open GslCore.Ast.Phase1Message
open GslCore.GslResult
open GslCore.Ast
open NUnit.Framework
open GslCore.Ast.Types
open GslCore.AstAssertions
open GslCore.Constants

[<TestFixture>]
type TestLineNumbers() =
    let rec extractAssemblies (n: AstNode): AstNode list =
        [ match n with
          | AstNode.Block b ->
              let result =
                  b.Value |> List.collect extractAssemblies

              yield! result
          | AstNode.Splice s ->
              let result =
                  s
                  |> List.ofArray
                  |> List.collect extractAssemblies

              yield! result
          | AstNode.Part p ->
              match p.Value.BasePart with
              | AstNode.Assembly _ as x -> yield x
              | _ -> ()
          | AstNode.Assembly _ as x -> yield x
          | _ -> () ]

    /// NB: note lines are numbered from zero internally, and columns.
    /// The parser typically reports one character past the end of the identifier
    let checkPosition (pos: SourcePosition option) (startLine, startCol, endLine, endCol) =
        match pos with
        | None -> Assert.Fail("expected source position to be Some not None")
        | Some p ->
            let startP = p.Start
            let endP = p.End
            Assert.AreEqual(startLine, startP.Line)
            Assert.AreEqual(startCol, startP.Column)
            Assert.AreEqual(endLine, endP.Line)
            Assert.AreEqual(endCol, endP.Column)

    /// compile one GSL source example and extract assemblies
    let compileOne source =
        source
        |> GslSourceCode
        |> compile (Phase1.phase1 AssemblyTestSupport.defaultPhase1Parameters)
        |> GslResult.assertOk
        |> fun x -> extractAssemblies x.wrappedNode

    let tripleNestedCallExample = """#refgenome cenpk
#platform stitch

let fun1(up,down) = // line 3 (zero numbered)
&up ; &down // line 4
end

let funTwo(gene) = // line 7
fun1(&gene,&gene) // line 8
end

fun1(uADH1,dADH1) // line 11
funTwo(uADH4) // line 12

let functionThree() = // line 14
 funTwo(uADH7) // line 15
end


         functionThree() // line 19
"""

    [<Test>]
    member __.TestFunctionExpansion() =
        let source = """#refgenome cenpk
#platform stitch

let knockout(up,down) =
    &up ; &down
end


knockout(uADH1,dADH1)
knockout(uADH2,dADH2)
knockout(uADH3,dADH3)
knockout(uADH4,dADH4)"""

        let assemblies = compileOne source

        Assert.AreEqual(4, assemblies.Length)

        // test coordinates of first assembly
        let a1 = List.head assemblies
        checkPosition a1.pos (8, 0, 8, 8)

        // test coordinates of last assembly
        let a4 = List.item 3 assemblies
        checkPosition a4.pos (11, 0, 11, 8)

    [<Test>]
    member __.TestFunctionExpansionNoArgs() =
        let source = "#refgenome cenpk
#platform stitch

let knockout() =
    uADH1 ; dADH1
end
knockout()"

        let assemblies = compileOne source

        // test coordinates of first assembly
        let a1 = List.head assemblies
        checkPosition a1.pos (6, 0, 6, 8)

    [<Test>]
    member i__.TestRecursiveFunctionExpansion() =
        let assemblies = compileOne tripleNestedCallExample

        Assert.AreEqual(3, assemblies.Length)

        // test coordinates of first assembly
        let a1 = List.head assemblies
        checkPosition a1.pos (11, 0, 11, 4)

        // test coordinates of second assembly
        let a2 = List.item 1 assemblies
        checkPosition a2.pos (12, 0, 12, 6)

        // test coordinates of last assembly
        let a3 = List.item 2 assemblies
        checkPosition a3.pos (19, 9, 19, 22)

    [<Test>]
    member __.TestNestedFunctionExpansionHasFourLineNumbers() =

        let assemblies = compileOne tripleNestedCallExample

        // Helpful debugging tool
        let coordsFormatted =
            String.Join
                ("\n",
                 [ for a in assemblies ->
                     String.Join
                         (";",
                          [ for p in a.positions ->
                              sprintf "%d,%d->%d,%d" p.Start.Line p.Start.Column p.End.Line p.End.Column ]) ])

        let assembly3 = List.item 2 assemblies
        printfn "coordinates for triple nested example:%A" coordsFormatted
        // Expect assembly 3 to have coordinates on line 19, 15, 8 and 4 (4 sets)
        // Due to bug/missing feature, it currently returns only 19 and 4, and test is marked ignore
        match assembly3.positions with
        | [ p1; p2; p3; p4 ] ->
            checkPosition (Some p1) (19, 9, 19, 22)
            checkPosition (Some p2) (15, 1, 15, 7)
            checkPosition (Some p3) (8, 0, 8, 4)
            checkPosition (Some p4) (4, 0, 4, 11)

        | x -> Assert.Fail(sprintf "expected 4 positions but got %d :\n %A" x.Length x)
