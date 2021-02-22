module GslCore.TestPromTermLen

open System
open GslCore.Core
open GslCore.Reference
open NUnit.Framework
open GslCore.Ast.LegacyParseTypes
open GslCore.Constants
open GslCore.Core.Types
open GslCore.Pragma
open GslCore.GslResult

/// location of test gslc_lib fixtures
let testLibDir1 = @"../../../../TestGslcLib"
let testLibDir2 = @"../../../../../TestGslcLib"

[<TestFixture>]
type TestPromTermLen() =

    let emptyPragmas = PragmaCollection.empty

    let testLibDir =
        if System.IO.Directory.Exists testLibDir1 then testLibDir1 else testLibDir2

    let same context expected actual =
        if expected <> actual
        then failwithf "%s: expected= %d and actual=%d not equal" context expected actual

    let checkOneGenome pragmas name promLen termLen termLenMRNA =
        let gd =
            GenomeDefinition.createLazy testLibDir name


        let (envLookup, gd) =
            gd
            |> GenomeDefinition.envLenLookupLazy "termlen" 666

        printfn "XXX envlookup=%d" envLookup
        let (lookupTermLen, gd) = gd |> GenomeDefinition.getTermLenLazy
        printfn "XXX name=%s termlen=%d" name lookupTermLen
        let gd = gd |> GenomeDefinition.load

        let (envLookup, gd) =
            gd
            |> GenomeDefinition.envLenLookupLazy "termlen" 666

        printfn "XXX envlookup=%d" envLookup
        let (lookupTermLen, gd) = gd |> GenomeDefinition.getTermLenLazy
        printfn "XXX name=%s termlen=%d" name lookupTermLen

        let part =
            DnaCreation.translateGenePrefix pragmas gd StandardSlice.Terminator

        same
            "terminator length test"
            termLen
            ((part.right.Position - part.left.Position
              + 1<OneOffset>)
             / 1<OneOffset>) // +1 since ends are inclusive

        let part =
            DnaCreation.translateGenePrefix pragmas gd StandardSlice.Promoter

        same
            "promoter length test"
            promLen
            ((part.right.Position - part.left.Position
              + 1<OneOffset>)
             / 1<OneOffset>) // +1 since ends are inclusive

        let mRNA =
            DnaCreation.translateGenePrefix pragmas gd StandardSlice.MRNA

        same
            "termmrna length test"
            termLenMRNA
            (((mRNA.right.Position - 1<OneOffset>)
              + 1<OneOffset>)
             / 1<OneOffset>) // Use 1 (rel to 3' end as the start of the terminator region

    let testPragma name value refGenome expProm expTerm expTermMRNA =
        let pragma =
            PragmaBuilder.createPragmaFromNameValue name [ value ] PragmaBuilder.builtin
            |> GslResult.valueOr (fun _ -> failwith "building promlen pragma")

        let map =
            { PragmaCollection.Pragmas = [ pragma.Name, pragma ] |> Map.ofList }

        checkOneGenome map refGenome expProm expTerm expTermMRNA


    [<Test>]
    member __.TestGenomesLoadable() =
        GenomeDefinition.createEager testLibDir "TestGenome"
        |> ignore

    [<Test>]
    member __.TestPragmasExist() =
        let checkPragmaExists name =
            Assert.DoesNotThrow(fun () ->
                (PragmaBuilder.createPragmaFromNameValue name [ "250" ] PragmaBuilder.builtin)
                |> GslResult.valueOr (fun messages -> messages |> String.concat ";" |> failwith)
                |> ignore)

        checkPragmaExists "promlen"
        checkPragmaExists "termlen"
        checkPragmaExists "termlenmrna"

    [<Test>]
    member __.TestDefaultTerminatorLen() =
        checkOneGenome
            emptyPragmas
            "TestGenome"
            Default.PromoterLength
            Default.TerminatorLength
            Default.MRNATerminatorLength

    [<Test>]
    member __.TestCustomTerminatorLen() =
        let testFolderExists1 = System.IO.Directory.Exists testLibDir1
        let testFolderExists2 = System.IO.Directory.Exists testLibDir2
        Assert.IsTrue(testFolderExists1 || testFolderExists2)

        checkOneGenome emptyPragmas "TestGenome2" 750 250 300

    [<Test>]
    member __.TestPromLenPragma() =
        testPragma "promlen" "123" "TestGenome" 123 Default.TerminatorLength Default.MRNATerminatorLength

    [<Test>]
    member __.TestTermLenPragma() =
        testPragma "termlen" "123" "TestGenome" Default.PromoterLength 123 Default.MRNATerminatorLength

    [<Test>]
    member __.TestTermLenMRNAPragma() =
        testPragma "termlenmrna" "123" "TestGenome" Default.PromoterLength Default.TerminatorLength 123
