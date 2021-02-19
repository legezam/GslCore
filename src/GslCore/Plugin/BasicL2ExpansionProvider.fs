﻿module GslCore.BasicL2ExpansionProvider

///
/// Implementation of GSL Level 2 Expression Lines
/// Modelled roughly on roughage syntax  e.g. gHO^ ; a> b ; c>d etc
///
open GslCore.Ast.Process
open GslCore.Pragma
open GslCore.Ast.LegacyParseTypes
open GslCore.Ast.Types
open GslCore.Ast.Algorithms
open GslCore.Constants
open System
open GslCore.PluginTypes
open GslCore.Reference

/// Take a list of expression elements and organize them in a balanced
/// way - e.g. splitting between two halves of a megastitch
let balance (elems: BuiltL2Element list) =
    let roundUp (x: float) =
        if x - (x |> int |> float) > Double.Epsilon then ((int x) + 1) else int x
    // Dumb implementation, doesn't handle bipromoters
    // or any clever avoidance of repeats etc, part reuse
    let countA = float (elems.Length) / 2.0 |> roundUp
    let partsA = Seq.take countA elems |> List.ofSeq
    let partsB = Seq.skip countA elems |> List.ofSeq
    partsA, partsB

/// Base implementation for level 2 knock out / promoter titration
/// give it lowest score in case someone has a preferred implementation
let l2JobScorer _ = Some 0.0<PluginScore>

/// Takes a level-2 line regarding explicit locus and returns a list.
///
/// E.g. transforms a level-2 line, gHO^ ; pA > gB ; pC > gD into
/// {"uHO"; "pA" ; "gB" ; "###" ; "!gD ; !pA" ; "dHO"}

let generateOutputsExplicitLocus (locus: L2Id) (args: L2DesignParams) =

    let locusWithPrefix = locus.Id.Value
    assert locus.Prefix.IsNone

    let locusWithoutPrefix = locusWithPrefix.Substring(1)

    if not (locusWithPrefix.ToUpper().StartsWith("G"))
    then failwithf "ERROR: knockout target gene %s must start with g tag (e.g. gADH1)." locusWithPrefix

    let out =
        seq {
            let partsA, partsB = balance args.Line.parts

            // Emit replacement DNA name for knockout
            let replacementName =
                match args.Pragmas |> PragmaCollection.tryFind BuiltIn.namePragmaDef with
                // if the user provided a pragma name, create the donor DNA name with .donor
                // (we're not just using the name itself because it has to be distinguished
                // from the gRNAs which will also be named a variant of the user provided name
                | Some (p) ->
                    let providedName = p.Arguments.[0] // get the first argument to the name pragma
                    sprintf "#name %s" providedName

                // if no name is provided, use this as the default donor name
                | None -> sprintf "#name u%s__d%s" locusWithoutPrefix locusWithoutPrefix

            yield replacementName

            // Emit upstream flanking region
            yield sprintf "u%s" locusWithoutPrefix
            // First half of the parts before the marker
            for expItem in partsA do
                yield AstNode.decompile expItem.promoter
                yield sprintf "%s" (expItem.target.String)

            if args.IsMegastitch then yield "###" // Marker
            // Second half of the parts after the marker
            for expItem in partsB do
                yield (sprintf "!%s;!(%s)" expItem.target.String (AstNode.decompile expItem.promoter))
            // Emit downstream flanking region
            yield sprintf "d%s" locusWithoutPrefix
        }
        |> List.ofSeq

    // results come back as a list of strings but we need to treat first name as a separate line and ; concat remainder
    match out with
    | name :: rest ->
        let partsList = String.Join(";", rest)
        [ name; partsList ]
    | _ -> failwithf "ERROR: L2 parsing failed"
    |> String.concat "\n"
    |> GslSourceCode


/// Takes a level-2 line regarding promoter titrations and returns a list.
///
/// E.g. transforms a level-2 line, pA>gB ; pc>gD into
/// {"uB"; "pC" ; "gD" ; "###" ; "pA" ; "gB[1:~500]"}
let generateOutputsTitrations (args: L2DesignParams) =

    // separates the expression pGene>gGene from the rest of the line
    let locusExp, otherExp =
        match args.Line.parts with
        | [] -> failwithf "ERROR: unexpected empty L2 expression construct with no locus or parts\n"
        | hd :: tl -> hd, tl
    /// the titrated gene
    let locusGene = locusExp.target.Id.Value.Substring(1)

    if not (locusExp.target.Id.Value.ToUpper().StartsWith("G")) then
        failwithf
            "ERROR: titrating expression target %s must start with g tag (e.g. gADH1). Variables not supported for titrations."
            locusExp.target.String

    let partsA, partsB = balance otherExp
    /// the flank length
    let reference = args.ReferenceGenomes |> GenomeDefinitions.get
    let flank = reference.[args.ReferenceGenome] |> GenomeDefinition.getFlank

    let out =
        seq {

            // Emit replacement DNA name for promoter swap
            let replacementName =
                match args.Pragmas |> PragmaCollection.tryFind BuiltIn.namePragmaDef with
                // if the user provided a pragma name, create the donor DNA name with .donor
                // (we're not just using the name itself because it has to be distinguished
                // from the gRNAs which will also be named a variant of the user provided name
                | Some (p) ->
                    let providedName = p.Arguments.[0] // get the first argument to the name pragma
                    sprintf "#name %s" providedName

                // if no name is provided, use this as the default donor name
                | None -> sprintf "#name u%s_%s_d%s" locusGene (AstNode.decompile locusExp.promoter |> Naming.cleanHashName) locusGene

            // yield a new linker line because the default pattern will cause an A linker
            // to land on a marker (error: no A-9 markers)
            match args.Pragmas |> PragmaCollection.tryFind BuiltIn.linkersPragmaDef with
            | Some _ -> yield ""
            | None -> yield "#linkers 0,2,A,3,9|0,A,2,9"

            yield replacementName
            // Yield upstream flanking region.
            yield (sprintf "u%s" locusGene) // regular locus flanking seq
            // First half of the parts before the marker
            for expItem in partsA do
                yield AstNode.decompile expItem.promoter
                yield expItem.target.String

            if args.IsMegastitch then yield "###"
            // Second half of the parts after the marker
            for expItem in partsB do
                yield (sprintf "!%s;!(%s)" expItem.target.String (AstNode.decompile expItem.promoter))
            // Finally the titrating promoter
            yield AstNode.decompile locusExp.promoter
            // Emit downstream flanking region
            yield sprintf "%s[1:~%A] {#breed DS_CDS}" locusExp.target.String flank
        }
        |> List.ofSeq

    match out with
    | linkers :: name :: rest ->
        [ linkers
          name
          String.Join(";", rest) ]
    | _ -> failwithf "ERROR: L2 parsing failed"
    |> String.concat "\n"
    |> sprintf "do\n%s\nend"
    |> GslSourceCode

let basicL2ExpansionPlugin =
    { Name = "basic_L2_provider"
      Description = Some "Basic implemention of L2 promoter titration."
      Behaviors =
          [ { Name = None
              Description = None
              Behavior =
                  L2KOTitration
                      ({ JobScorer = l2JobScorer
                         ExplicitLocusProvider = generateOutputsExplicitLocus
                         ImplicitLocusProvider = generateOutputsTitrations }) } ]
      ProvidesPragmas = []
      ProvidesCapas = [] }