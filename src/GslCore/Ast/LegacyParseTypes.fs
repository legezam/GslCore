module GslCore.Ast.LegacyParseTypes

open Amyris.Dna
open GslCore.Ast.Process
open GslCore.Constants
open System
open GslCore.GslResult
open GslCore.Uri
open GslCore.Pragma
open GslCore.Ast.Types
open GslCore.Ast.ErrorHandling
open GslCore.DesignParams

type Slice =
    { Left: RelativePosition
      LeftApprox: bool
      Right: RelativePosition
      RightApprox: bool }

type SliceContext =
    | Genomic
    | Library of string // string payload for helpful error message

/// Return a tuple of OneOffset left/right slice bounds from a slice record.
/// These bounds are both relative to the FivePrime end.
/// Requires the length of the feature being sliced to be interpreted correctly.
let getBoundsFromSlice (slice: Slice) (featureLength: int) (context: SliceContext) =
    let left =
        match slice.Left.RelativeTo with
        | FivePrime -> slice.Left.Position
        | ThreePrime ->
            (featureLength + 1) * 1<OneOffset>
            + slice.Left.Position

    let right =
        match slice.Right.RelativeTo with
        | FivePrime -> slice.Right.Position
        | ThreePrime ->
            (featureLength + 1) * 1<OneOffset>
            + slice.Right.Position

    match context with
    | Genomic ->
        // no validation necessary
        Ok(left, right)
    | Library partId ->
        // the slice bounds are not allowed to fall outside the feature as we don't have
        // data on flanking regions in this context
        if left < 1<OneOffset>
           || right <= left
           || right > (featureLength * 1<OneOffset>) then
            Result.Error(sprintf "Illegal slice (%A) outside core gene range for library item %s." slice partId)
        else
            Ok(left, right)

[<RequireQualifiedAccess>]
type Modifier =
    | Mutation of Mutation
    | Slice of Slice
    | Dot of string

type LegacyPartId = { Id: string; Modifiers: Modifier list }

type GenePart =
    { Gene: string
      Modifiers: Modifier list
      Where: SourcePosition list }

type GenePartWithLinker =
    { Part: GenePart
      Linker: Linker option }

[<RequireQualifiedAccess>]
type Part =
    | GenePart of GenePartWithLinker
    | MarkerPart
    | InlineDna of Dna
    | InlineProtein of string
    | HeterologyBlock
    | SourceCode of GslSourceCode
    | PartId of LegacyPartId

/// Part plus a Pragma
and PartPlusPragma =
    { Part: Part
      Pragma: PragmaCollection
      IsForward: bool }

/// Namespace bounded tag for an assembly (Used in Assembly)
type AssemblyTag = { Namespace: string; Tag: string }

type Assembly =
    { Parts: PartPlusPragma list
      Name: string option
      Uri: Uri option
      LinkerHint: string
      Pragmas: PragmaCollection
      DesignParams: DesignParams
      Capabilities: Capabilities
      DocStrings: string list
      SourcePosition: SourcePosition list }
    interface ISourcePosition with
        member x.OptionalSourcePosition = x.SourcePosition


// ================================================
// Level 2 Definitions
// ================================================

/// Element of a level 2 line  e.g.  pABC1>gDEF2
type BuiltL2Element = { Promoter: AstNode; Target: L2Id }

/// L2 Top level container for the expression line  z^ ; a>b ; c > d etc
type BuiltL2Expression =
    { L2Locus: L2Id option
      Parts: BuiltL2Element List }

/// L2 Top level container
type L2Line =
    { L2Design: BuiltL2Expression
      Name: string option
      Uri: Uri option
      Pragmas: PragmaCollection
      Capabilities: Capabilities }

// ========================
// pretty-printing legacy assemblies as GSL source code
// ========================

/// Pretty print a RelPos
let printRP (position: RelativePosition) =
    sprintf
        "%A/%s"
        position.Position
        (match position.RelativeTo with
         | FivePrime -> "S"
         | ThreePrime -> "E")

let printSlice (s: Slice) =
    sprintf
        "[%s%A%s:%s%A%s]"
        (if s.LeftApprox then "~" else "")
        s.Left.Position
        (match s.Left.RelativeTo with
         | FivePrime -> "S"
         | ThreePrime -> "E")
        (if s.RightApprox then "~" else "")
        s.Right.Position
        (match s.Right.RelativeTo with
         | FivePrime -> "S"
         | ThreePrime -> "E")

let expandMods (ml: Modifier list) =
    seq {
        for m in ml do
            match m with
            | Modifier.Mutation (m) ->
                yield
                    sprintf
                        "%c%c%d%c"
                        (match m.Type with
                         | AA -> '$'
                         | NT -> '*')
                        m.From
                        m.Location
                        m.To
            | Modifier.Slice (s) -> yield printSlice s
            | Modifier.Dot (d) -> yield sprintf ".%s" d
    }
    |> fun x -> String.Join("", x)

let rec printPPP ppp =
    let partOut =
        match ppp.Part with
        | Part.HeterologyBlock -> "~ " // don't do anything at this level
        | Part.InlineDna (s) -> sprintf "/%O/ " s // inline DNA sequence
        | Part.InlineProtein (s) -> sprintf "/$%s/ " s // inline protein sequence
        | Part.MarkerPart -> "### "
        | Part.PartId (p) -> sprintf "@%s" p.Id + (expandMods p.Modifiers)
        | Part.SourceCode (s) -> s.String // Part that was already expanded into a string
        | Part.GenePart (gp) ->
            let lOut =
                match gp.Linker with
                | None -> ""
                | Some (l) -> sprintf "%s-%s-%s-" l.Linker1 l.Linker2 l.Orient // Emit linker

            let p = gp.Part

            let gOut = p.Gene
            let modOut = expandMods p.Modifiers

            lOut + gOut + modOut // String.Join("",Array.ofSeq modOut)
    // Now add in any inline pragma part with braces, ; separated etc
    let prOut =
        if ppp.Pragma.Pragmas.Count = 0 then
            ""
        else
            ppp.Pragma.Pragmas
            |> Seq.map (fun pv -> sprintf "#%s %s" pv.Key (pv.Value.Arguments |> String.concat " "))
            |> fun ss -> String.Join(";", ss)
            |> sprintf "{%s}"

    (if ppp.IsForward then "" else "!") + partOut + prOut

/// Pretty print a built GSL assembly
let prettyPrintAssembly (assembly: Assembly) =
    [ for ppp in assembly.Parts -> printPPP ppp ]
    |> String.concat ";"
    |> GslSourceCode

// =====================
// conversion from AST parts to legacy parts
// =====================

let private sliceFromAstSlice (parseSlice: ParseSlice) =
    match parseSlice.Left, parseSlice.Right with
    | RelPos (lw), RelPos (rw) ->
        GslResult.ok
            (Modifier.Slice
                ({ Left = lw.Value
                   LeftApprox = parseSlice.LeftApprox
                   Right = rw.Value
                   RightApprox = parseSlice.RightApprox }))
    | x, y ->
        let contextStr =
            sprintf "legacy slice construction; found [%s:%s]" x.TypeName y.TypeName

        AstResult.internalTypeMismatch (Some(contextStr)) "RelPos" x

let private astNodeToLegacyMod node =
    match node with
    | Slice (sw) -> sliceFromAstSlice sw.Value
    | Mutation (mw) -> GslResult.ok (Modifier.Mutation(mw.Value))
    | DotMod (dm) -> GslResult.ok (Modifier.Dot(dm.Value))
    | _ -> AstResult.internalTypeMismatch (Some "legacy mod conversion") "Slice or Mutation or DotMod" node

let private convertMods mods =
    mods
    |> List.map astNodeToLegacyMod
    |> GslResult.collectA

/// Convert an AST base part into a legacy Part.
let private createLegacyPart (part: Node<ParsePart>): AstResult<Part> =
    match part.Value.BasePart with
    | Gene geneWrapper ->
        convertMods part.Value.Modifiers
        |> GslResult.map (fun mods ->
            let genePart =
                { Gene = geneWrapper.Value.Gene
                  Modifiers = mods
                  Where = geneWrapper.Positions }

            Part.GenePart
                { Part = genePart
                  Linker = geneWrapper.Value.Linker })
    | Marker _ -> GslResult.ok Part.MarkerPart
    | InlineDna dnaSequence -> GslResult.ok (Part.InlineDna(Dna(dnaSequence.Value, true, AllowAmbiguousBases)))
    | InlineProtein proteinSequence -> GslResult.ok (Part.InlineProtein proteinSequence.Value)
    | HetBlock _ -> GslResult.ok Part.HeterologyBlock
    | PartId partId ->
        convertMods part.Value.Modifiers
        |> GslResult.map (fun mods -> Part.PartId { Id = partId.Value; Modifiers = mods })
    | x -> AstResult.internalTypeMismatch (Some "legacy part conversion") "legacy-compatible base part" x

let private createPPP part =
    match part with
    | Part p ->
        createLegacyPart p
        |> GslResult.map (fun legacyPart ->
            { Part = legacyPart
              Pragma = ParsePart.getPragmas p
              IsForward = p.Value.IsForward })
    | x -> AstResult.internalTypeMismatch (Some "legacy part conversion") "Part" x

/// For assembly conversion, we need to accumulate both a pragma environment and docstrings.
/// Combine these two accumulation functions and state datastructures.
type AssemblyConversionContext =
    { PragmaEnvironment: PragmaEnvironment
      Docs: DocstringEnvironment }

let emptyConversionContext =
    { PragmaEnvironment = PragmaEnvironment.empty
      Docs = DocstringEnvironment.empty }

/// Accumulate both pragma and docstring context.
let updateConversionContext mode s node =
    let newPragmaEnv =
        AssemblyStuffing.updatePragmaEnvironment mode s.PragmaEnvironment node

    let newDocsEnv =
        Docstrings.updateDocstringEnvironment mode s.Docs node

    { s with
          PragmaEnvironment = newPragmaEnv
          Docs = newDocsEnv }

/// Convert an AST assembly into a legacy assembly.
let convertAssembly (context: AssemblyConversionContext)
                    (partWrapper: Node<ParsePart>, aplw: Node<AstNode list>)
                    : AstResult<Assembly> =
    let assemblyPragmas = ParsePart.getPragmas partWrapper

    let name =
        assemblyPragmas
        |> PragmaCollection.tryGetValue BuiltIn.namePragmaDef

    let uri =
        assemblyPragmas
        |> PragmaCollection.tryGetValue BuiltIn.uriPragmaDef

    let linkerHint =
        assemblyPragmas
        |> PragmaCollection.tryGetValues BuiltIn.linkersPragmaDef
        |> Option.map (String.concat "")
        |> Option.defaultValue ""


    let parameters =
        DesignParams.fromPragmas DesignParams.identity assemblyPragmas
        |> GslResult.mapError (fun message ->
            AstMessage.createErrorWithStackTrace PragmaError message (Part(partWrapper)))

    let parts =
        aplw.Value
        |> List.map createPPP
        |> GslResult.collectA

    (parts, parameters)
    ||> GslResult.map2 (fun parts designParams ->
            { Assembly.Parts = parts
              Name = name
              Uri = uri
              LinkerHint = linkerHint
              Pragmas = assemblyPragmas
              DesignParams = designParams
              Capabilities = context.PragmaEnvironment.Capabilities
              DocStrings = context.Docs.Assigned
              SourcePosition = partWrapper.Positions })

// ======================
// conversion from L2 AST node to legacy L2 line type
// ======================

/// Build a concrete L2 element from an AST node.
let private buildL2Element node =
    match node with
    | L2Element (nw) ->
        match nw.Value.Promoter, nw.Value.Target with
        | L2Id _, L2Id (tw)
        | Part _, L2Id (tw) ->
            GslResult.ok
                { Promoter = nw.Value.Promoter
                  Target = tw.Value }

        | x, y ->
            let contextStr =
                sprintf "L2 element construction; found [%s>%s]" x.TypeName y.TypeName

            AstResult.internalTypeMismatch (Some(contextStr)) "L2Id" node
    | x -> AstResult.internalTypeMismatch (Some("L2 element construction")) "L2Id" x

let private unpackLocus nodeopt =
    match nodeopt with
    | Some (L2Id (lw)) -> GslResult.ok (Some(lw.Value))
    | Some (x) -> AstResult.internalTypeMismatch (Some("L2 locus unpacking")) "L2Id" x
    | None -> GslResult.ok None

/// Build a concrete L2 expression from an AST node.
let private buildL2Expression (ew: Node<L2Expression>): AstResult<BuiltL2Expression> =
    let parts =
        ew.Value.Parts
        |> List.map buildL2Element
        |> GslResult.collectA

    let locus = unpackLocus ew.Value.Locus

    (locus, parts)
    ||> GslResult.map2 (fun locus parts -> { L2Locus = locus; Parts = parts })

/// Build a L2Line from an AST node and pragma environment.
let convertL2Line (pragmaEnv: PragmaEnvironment) (l2Expression: Node<L2Expression>): AstResult<L2Line> =
    let pragmas =
        pragmaEnv.Persistent
        |> PragmaCollection.mergeInCollection pragmaEnv.AssignedTransients

    let name =
        pragmas
        |> PragmaCollection.tryGetValue BuiltIn.namePragmaDef

    let uri =
        pragmas
        |> PragmaCollection.tryGetValue BuiltIn.uriPragmaDef

    buildL2Expression l2Expression
    |> GslResult.map (fun l2Design ->
        { L2Line.L2Design = l2Design
          Name = name
          Uri = uri
          Pragmas = pragmas
          Capabilities = pragmaEnv.Capabilities })
