namespace GslCore.Legacy.Types

open Amyris.Dna
open GslCore.Ast.Process
open GslCore.Ast.Process.AssemblyStuffing
open GslCore.Constants
open GslCore.Uri
open GslCore.Pragma
open GslCore.Ast.Types
open GslCore.DesignParams


type Slice =
    { Left: RelativePosition
      LeftApprox: bool
      Right: RelativePosition
      RightApprox: bool }

type SliceContext =
    | Genomic
    | Library of string // string payload for helpful error message
    

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
    
/// For assembly conversion, we need to accumulate both a pragma environment and docstrings.
/// Combine these two accumulation functions and state datastructures.
type AssemblyConversionContext =
    { PragmaEnvironment: PragmaEnvironment
      Docs: DocstringEnvironment }

module AssemblyConversionContext =
    let empty =
        { PragmaEnvironment = PragmaEnvironment.empty
          Docs = DocstringEnvironment.empty }
    