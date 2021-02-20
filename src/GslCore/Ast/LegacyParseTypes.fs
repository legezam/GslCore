module GslCore.Ast.LegacyParseTypes

open Amyris.ErrorHandling
open Amyris.Dna
open GslCore.Ast.Process
open GslCore.Constants
open System
open GslCore.Uri
open GslCore.Pragma
open GslCore.Ast.Types
open GslCore.Ast.ErrorHandling
open GslCore.DesignParams

type Slice =
    { left: RelativePosition
      lApprox: bool
      right: RelativePosition
      rApprox: bool }

type SliceContext =
    | Genomic
    | Library of string // string payload for helpful error message

/// Return a tuple of OneOffset left/right slice bounds from a slice record.
/// These bounds are both relative to the FivePrime end.
/// Requires the length of the feature being sliced to be interpreted correctly.
let getBoundsFromSlice (slice: Slice) featureLength context =
    let left =
        match slice.left.RelativeTo with
        | FivePrime -> slice.left.Position
        | ThreePrime ->
            (featureLength + 1) * 1<OneOffset>
            + slice.left.Position

    let right =
        match slice.right.RelativeTo with
        | FivePrime -> slice.right.Position
        | ThreePrime ->
            (featureLength + 1) * 1<OneOffset>
            + slice.right.Position

    match context with
    | Genomic ->
        // no validation necessary
        ok (left, right)
    | Library partId ->
        // the slice bounds are not allowed to fall outside the feature as we don't have
        // data on flanking regions in this context
        if left < 1<OneOffset>
           || right <= left
           || right > (featureLength * 1<OneOffset>) then
            fail (sprintf "Illegal slice (%A) outside core gene range for library item %s." slice partId)
        else
            ok (left, right)

type Mod =
    | MUTATION of Mutation
    | SLICE of Slice
    | DOTMOD of string

type PartIdLegacy = { id: string; mods: Mod list }

type GenePart =
    { gene: string
      mods: Mod list
      where: SourcePosition list }

type GenePartWithLinker =
    { part: GenePart
      linker: Linker option }

type Part =
    | GENEPART of GenePartWithLinker
    | MARKERPART
    | INLINEDNA of Dna
    | INLINEPROT of string
    | HETBLOCK
    | SOURCE_CODE of GslSourceCode
    | PARTID of PartIdLegacy

/// Part plus a Pragma
and PPP =
    { part: Part
      pr: PragmaCollection
      fwd: bool }

/// Namespace bounded tag for an assembly (Used in Assembly)
type AssemblyTag = { nameSpace: string; tag: string }

type Assembly =
    { parts: PPP list
      name: string option
      uri: Uri option
      linkerHint: string
      pragmas: PragmaCollection
      designParams: DesignParams
      capabilities: Capabilities
      docStrings: string list
      sourcePosition: SourcePosition list }
    interface ISourcePosition with
        member x.OptionalSourcePosition = x.sourcePosition


// ================================================
// Level 2 Definitions
// ================================================

/// Element of a level 2 line  e.g.  pABC1>gDEF2
type BuiltL2Element = { promoter: AstNode; target: L2Id }

/// L2 Top level container for the expression line  z^ ; a>b ; c > d etc
type BuiltL2Expression =
    { l2Locus: L2Id option
      parts: BuiltL2Element List }

/// L2 Top level container
type L2Line =
    { l2Design: BuiltL2Expression
      name: string option
      uri: Uri option
      pragmas: PragmaCollection
      capabilities: Capabilities }

// ========================
// pretty-printing legacy assemblies as GSL source code
// ========================

/// Pretty print a RelPos
let printRP (l: RelativePosition) =
    sprintf
        "%A/%s"
        l.Position
        (match l.RelativeTo with
         | FivePrime -> "S"
         | ThreePrime -> "E")

let printSlice (s: Slice) =
    sprintf
        "[%s%A%s:%s%A%s]"
        (if s.lApprox then "~" else "")
        s.left.Position
        (match s.left.RelativeTo with
         | FivePrime -> "S"
         | ThreePrime -> "E")
        (if s.rApprox then "~" else "")
        s.right.Position
        (match s.right.RelativeTo with
         | FivePrime -> "S"
         | ThreePrime -> "E")

let expandMods (ml: Mod list) =
    seq {
        for m in ml do
            match m with
            | MUTATION (m) ->
                yield
                    sprintf
                        "%c%c%d%c"
                        (match m.Type with
                         | AA -> '$'
                         | NT -> '*')
                        m.From
                        m.Location
                        m.To
            | SLICE (s) -> yield printSlice s
            | DOTMOD (d) -> yield sprintf ".%s" d
    }
    |> fun x -> String.Join("", x)

let rec printPPP ppp =
    let partOut =
        match ppp.part with
        | HETBLOCK -> "~ " // don't do anything at this level
        | INLINEDNA (s) -> sprintf "/%O/ " s // inline DNA sequence
        | INLINEPROT (s) -> sprintf "/$%s/ " s // inline protein sequence
        | MARKERPART -> "### "
        | PARTID (p) -> sprintf "@%s" p.id + (expandMods p.mods)
        | SOURCE_CODE (s) -> s.String // Part that was already expanded into a string
        | GENEPART (gp) ->
            let lOut =
                match gp.linker with
                | None -> ""
                | Some (l) -> sprintf "%s-%s-%s-" l.Linker1 l.Linker2 l.Orient // Emit linker

            let p = gp.part

            let gOut = p.gene
            let modOut = expandMods p.mods

            lOut + gOut + modOut // String.Join("",Array.ofSeq modOut)
    // Now add in any inline pragma part with braces, ; separated etc
    let prOut =
        if ppp.pr.Pragmas.Count = 0 then
            ""
        else
            ppp.pr.Pragmas
            |> Seq.map (fun pv -> sprintf "#%s %s" pv.Key (pv.Value.Arguments |> String.concat " "))
            |> fun ss -> String.Join(";", ss)
            |> sprintf "{%s}"

    (if ppp.fwd then "" else "!") + partOut + prOut

/// Pretty print a built GSL assembly
let prettyPrintAssembly (assembly: Assembly) =
    [ for ppp in assembly.parts -> printPPP ppp ]
    |> String.concat ";"
    |> GslSourceCode

// =====================
// conversion from AST parts to legacy parts
// =====================

let private sliceFromAstSlice (s: ParseSlice) =
    match s.Left, s.Right with
    | RelPos (lw), RelPos (rw) ->
        ok
            (SLICE
                ({ left = lw.Value
                   lApprox = s.LeftApprox
                   right = rw.Value
                   rApprox = s.RightApprox }))
    | x, y ->
        let contextStr =
            sprintf "legacy slice construction; found [%s:%s]" x.TypeName y.TypeName

        AstMessage.internalTypeMismatch (Some(contextStr)) "RelPos" x

let private astNodeToLegacyMod node =
    match node with
    | Slice (sw) -> sliceFromAstSlice sw.Value
    | Mutation (mw) -> ok (MUTATION(mw.Value))
    | DotMod (dm) -> ok (DOTMOD(dm.Value))
    | _ -> AstMessage.internalTypeMismatch (Some "legacy mod conversion") "Slice or Mutation or DotMod" node

let private convertMods mods =
    mods |> List.map astNodeToLegacyMod |> collect

/// Convert an AST base part into a legacy Part.
let private createLegacyPart (part: Node<ParsePart>): Result<Part, AstMessage> =
    match part.Value.BasePart with
    | Gene (gw) ->
        convertMods part.Value.Modifiers
        >>= (fun mods ->
            let genePart =
                { gene = gw.Value.Gene
                  mods = mods
                  where = gw.Positions }

            ok
                (GENEPART
                    ({ part = genePart
                       linker = gw.Value.Linker })))
    | Marker _ -> ok MARKERPART
    | InlineDna (s) -> ok (INLINEDNA(Dna(s.Value, true, AllowAmbiguousBases)))
    | InlineProtein (s) -> ok (INLINEPROT s.Value)
    | HetBlock _ -> ok HETBLOCK
    | PartId (p) ->
        convertMods part.Value.Modifiers
        >>= (fun mods -> ok (PARTID({ id = p.Value; mods = mods })))
    | x -> AstMessage.internalTypeMismatch (Some "legacy part conversion") "legacy-compatible base part" x

let private createPPP part =
    match part with
    | Part (p) ->
        createLegacyPart p
        >>= (fun legacyPart ->
            ok
                { part = legacyPart
                  pr = ParsePart.getPragmas p
                  fwd = p.Value.IsForward })
    | x -> AstMessage.internalTypeMismatch (Some "legacy part conversion") "Part" x

/// For assembly conversion, we need to accumulate both a pragma environment and docstrings.
/// Combine these two accumulation functions and state datastructures.
type AssemblyConversionContext =
    { pragmaEnv: PragmaEnvironment
      docs: DocstringEnvironment }

let emptyConversionContext =
    { pragmaEnv = PragmaEnvironment.empty
      docs = DocstringEnvironment.empty }

/// Accumulate both pragma and docstring context.
let updateConversionContext mode s node =
    let newPragmaEnv =
        AssemblyStuffing.updatePragmaEnvironment mode s.pragmaEnv node

    let newDocsEnv =
        Docstrings.updateDocstringEnvironment mode s.docs node

    { s with
          pragmaEnv = newPragmaEnv
          docs = newDocsEnv }

/// Convert an AST assembly into a legacy assembly.
let convertAssembly (context: AssemblyConversionContext)
                    (partWrapper: Node<ParsePart>, aplw: Node<AstNode list>)
                    : Result<Assembly, AstMessage> =
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

    
    DesignParams.fromPragmas DesignParams.identity assemblyPragmas
    |> mapMessages (fun message -> AstMessage.createErrorWithStackTrace PragmaError message (Part(partWrapper)))
    |> tupleResults (aplw.Value |> List.map createPPP |> collect)
    >>= fun (parts, designParams) ->
            ok
                { Assembly.parts = parts
                  name = name
                  uri = uri
                  linkerHint = linkerHint
                  pragmas = assemblyPragmas
                  designParams = designParams
                  capabilities = context.pragmaEnv.Capabilities
                  docStrings = context.docs.Assigned
                  sourcePosition = partWrapper.Positions }

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
            ok
                { promoter = nw.Value.Promoter
                  target = tw.Value }

        | x, y ->
            let contextStr =
                sprintf "L2 element construction; found [%s>%s]" x.TypeName y.TypeName

            AstMessage.internalTypeMismatch (Some(contextStr)) "L2Id" node
    | x -> AstMessage.internalTypeMismatch (Some("L2 element construction")) "L2Id" x

let private unpackLocus nodeopt =
    match nodeopt with
    | Some (L2Id (lw)) -> ok (Some(lw.Value))
    | Some (x) -> AstMessage.internalTypeMismatch (Some("L2 locus unpacking")) "L2Id" x
    | None -> ok None

/// Build a concrete L2 expression from an AST node.
let private buildL2Expression (ew: Node<L2Expression>) =
    ew.Value.Parts
    |> List.map buildL2Element
    |> collect
    |> tupleResults (unpackLocus ew.Value.Locus)
    >>= (fun (locus, parts) -> ok { l2Locus = locus; parts = parts })

/// Build a L2Line from an AST node and pragma environment.
let convertL2Line (pragmaEnv: PragmaEnvironment) (l2Expression: Node<L2Expression>): Result<L2Line, AstMessage> =
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
    >>= fun l2Design ->
            ok
                { L2Line.l2Design = l2Design
                  name = name
                  uri = uri
                  pragmas = pragmas
                  capabilities = pragmaEnv.Capabilities }
