/// Types and functions for AST creation.
module GslCore.AstTypes

open FSharp.Text.Lexing
open GslCore.Constants
open System
open GslCore

// =======================
// types and functions for lexing
// =======================

/// Convert matched characters during lexing into a string.
let lexeme = LexBuffer<_>.LexemeString

type SourcePosition =
    { Start: Position
      End: Position }
    override this.ToString() =
        sprintf "@%d,%d-%d,%d" (this.Start.Line + 1) (this.Start.Column + 1) (this.End.Line + 1) (this.End.Column + 1)

module SourcePosition =
    /// Return a nicely-formatted message for the start of this source position.
    let format (this: SourcePosition): string =
        sprintf "near line %d col %d" (this.Start.Line + 1) (this.Start.Column + 1)
    /// Provide a code snippet with an indication of the start of this position.
    /// Returned as a sequence of strings, one sequence item for each line.
    /// Optionally override the default number of lines to use for context, defaults to 5.
    let sourceContextWithSize (GslSourceCode source) (contextLines: int) (this: SourcePosition): string seq =
        seq {

            let lines =
                source
                    .Replace("\r\n", "\n")
                    .Split([| '\n'; '\r' |])

            let p = this.Start

            for line in max 0 (p.Line - contextLines) .. min (p.Line + contextLines) (lines.Length - 1) do
                yield sprintf "%s" lines.[line]
                if line = p.Line then yield sprintf "%s^" (Utils.pad p.Column)
        }

    let sourceContext source this = sourceContextWithSize source 5 this

    /// Expand possibly multiple levels of source positions into a formatted string
    let formatSourcePositionList (positions: SourcePosition list) =
        positions |> List.map format |> String.concat "; "

    let empty =
        { SourcePosition.Start = Position.FirstLine("")
          End = Position.FirstLine("") }

    let fromLexbuf (lexbuf: LexBuffer<_>) =
        { SourcePosition.Start = lexbuf.StartPos
          End = lexbuf.EndPos }

/// Interface type to allow generic retrieval of a source code position.
type ISourcePosition =
    abstract OptionalSourcePosition: SourcePosition list

// TODO: We may want to collapse the distinction between Positioned and Node.  At present they
// are identical types except for the record field names.  We may wish to leave them distinct to
// avoid adding cruft to the lexer type, leaving Node free to accumulate more parsing-related
// fields that we don't want to have to add placeholders to.

/// Generic lexer token that stores lexing position.
type Positioned<'T> = { Item: 'T; Position: SourcePosition }

type PUnit = Positioned<unit>
type PString = Positioned<string>
type PInt = Positioned<int>
type PFloat = Positioned<float>

module Positioned =
    /// Tokenize the item in the lex buffer using a pased conversion function.
    let tokenize (operator: string -> 'a) (lexbuf: LexBuffer<char>): Positioned<'a> =
        let item = operator (lexeme lexbuf)

        { Positioned.Item = item
          Position = SourcePosition.fromLexbuf lexbuf }

    /// Create a unit token with position from lexbuf.
    let tokenizeUnit: LexBuffer<char> -> PUnit = tokenize (ignore)

    /// Tokenize a lex item as a string.
    let tokenizeString: LexBuffer<char> -> PString = tokenize id

    /// Tokenize a lex item as a string literal, stripping off the quotes.
    let tokenizeStringLiteral: LexBuffer<char> -> PString =
        tokenize (fun stringWithQuotes -> stringWithQuotes.[1..stringWithQuotes.Length - 2])

    /// Tokenize a lex item as an int.
    let tokenizeInt: LexBuffer<char> -> PInt = tokenize int

    /// Tokenize a lex item as a float.
    let tokenizeFloat: LexBuffer<char> -> PFloat = tokenize float

    /// Tokenize a pragma name by trimming the first character
    let tokenizeStringTrimFirstChar (lexbuf: LexBuffer<char>): PString =
        let name = (lexeme lexbuf).[1..]

        { Positioned.Item = name
          Position = SourcePosition.fromLexbuf lexbuf }

    /// Create a new position bracketed by a pair of positions.
    let posBracketTokens (left: Positioned<'a>) (right: Positioned<'b>): SourcePosition =
        { SourcePosition.Start = left.Position.Start
          End = right.Position.End }

// =============================
// Wrapper type for all AST nodes.
// =============================

/// Wrapper type for every AST node.
/// This enables adding extensible metadata to the AST for tracking things such as source
/// code position.  pos tracks position history of this node (when functions expand) with highest level call first
/// with
[<CustomEquality>]
[<NoComparison>]
type Node<'T when 'T: equality> =
    { Value: 'T
      Positions: SourcePosition list }
    /// Override equality to ignore source code position.  We just care about semantic comparison.
    /// This is mostly to aid in testing.  We shouldn't need to be comparing generic AST nodes during parsing.
    override this.Equals other =
        match other with
        | :? (Node<'T>) as o -> this.Value = o.Value
        | _ -> false

    override this.GetHashCode() = this.Value.GetHashCode()


/// Generic helper functions for wrapping node payloads.
module Node =
    /// Push a new source position onto stack of positions during function expansion
    /// with first (most recently pushed) value being highest level function call
    let prependPositionsNode (newPositions: SourcePosition list) (node: Node<'T>): Node<'T> =
        { node with
              Positions = newPositions @ node.Positions }

    /// Wrap a value in a NodeWrapper without source position.
    let wrapNode (value: 'a) = { Node.Value = value; Positions = [] }

    /// Wrap a valye in a NodeWrapper with a position taken from a Positioned token.
    let wrapNodeWithTokenPosition (token: Positioned<'a>) (value: 'b): Node<'b> =
        { Node.Value = value
          Positions = [ token.Position ] }

    /// Convert a Positioned token into a node by applying a function to the token's payload.
    let tokenAsNodeAfter (f: 'a -> 'b) (token: Positioned<'a>): Node<'b> =
        { Node.Value = f token.Item
          Positions = [ token.Position ] }

    /// Straight-up convert a Positioned token into a node.
    let tokenAsNode (token: Positioned<'a>): Node<'a> =
        { Node.Value = token.Item
          Positions = [ token.Position ] }

// ==================
// AST type declaration
// ==================

// ------ Internal data types for certain nodes, non-recursive ------

/// One line of a docstring
type DocstringLine = PString

/// Amino acid vs. dna base mutation
and MutationType =
    | AA
    | NT

type Mutation =
    { From: char
      To: char
      Location: int
      Type: MutationType }

type Linker =
    { Linker1: string
      Linker2: string
      Orient: string }

type ParseGene = { Gene: string; Linker: Linker option }

/// Supported binary operations on nodes.
type BinaryOperator =
    | Add
    | Subtract
    | Multiply
    | Divide

/// Supported types for variables.
/// We need NotYetTyped to allow constructs like let foo = &bar, as we cannot elide a type for &bar at this point.
type GslVariableType =
    | NotYetTyped
    | IntType
    | FloatType
    | StringType
    | PartType
    /// Print the actual name of the type.
    override this.ToString() =
        match this with
        | NotYetTyped -> "Untyped"
        | IntType -> "Int"
        | FloatType -> "Float"
        | StringType -> "String"
        | PartType -> "Part"

// ------ The AST itself. Node definitions follow. ------

///<summary>
/// Newtype declaration to ensure we don't mix up operations that are intended to operate
/// on single nodes vs. those which recursively operate on an entire tree.  Functions that operate
/// at the nodal level should accept AstNode and return, and those which operate on an entire tree
/// should accept and return one of these wrappers instead.
/// </summary>
type AstTreeHead =
    | AstTreeHead of AstNode
    member this.wrappedNode =
        match this with
        | AstTreeHead node -> node

/// AST for GSL.
and AstNode =
    // leaf nodes that hold values
    | Int of Node<int>
    | Float of Node<float>
    | String of Node<string>
    // docstrings
    | Docstring of Node<string>
    // variable leaf node
    | TypedVariable of Node<string * GslVariableType>
    // variable binding
    | VariableBinding of Node<VariableBinding>
    // typed value
    | TypedValue of Node<GslVariableType * AstNode>
    // Simple operations on values
    | BinaryOperation of Node<BinaryOperation>
    | Negation of Node<AstNode>
    // Slicing
    | ParseRelPos of Node<ParseRelPos>
    | RelPos of Node<RelPos>
    | Slice of Node<ParseSlice>
    // non-slice part mods
    | Mutation of Node<Mutation>
    | DotMod of Node<string>
    // generic part with mods, pragmas, direction
    | Part of Node<ParsePart>
    // AST nodes for base part types
    | Marker of Node<unit>
    | PartId of Node<string>
    | InlineDna of Node<string>
    | InlineProtein of Node<string>
    | HetBlock of Node<unit>
    | Gene of Node<ParseGene>
    | Assembly of Node<AstNode list>
    // AST nodes for Level 2 syntax support
    | L2Id of Node<L2Id>
    | L2Element of Node<L2Element>
    | L2Expression of Node<L2Expression>
    // Roughage support
    | Roughage of Node<Roughage>
    // pragmas
    | ParsePragma of Node<ParsePragma>
    | Pragma of Node<Pragma.Pragma>
    // Block of code
    | Block of Node<AstNode list>
    // Function definition and call
    | FunctionDef of Node<ParseFunction>
    | FunctionLocals of Node<FunctionLocals>
    | FunctionCall of Node<FunctionCall>
    // Error during parsing, injected by the parser.
    | ParseError of Node<string>
    // Bootstrapping can turn one node into several that all need to be spliced in.
    // We keep track of this using this node type.
    // It acts purely as an opaque container, so foldmap operations do not recurse into it.
    // In a bootstrapping operation, we create these, and immediately follow with a cleaning step
    // to explode them into their outer contexts.
    | Splice of AstNode []
    /// Get most recent position (highest level in nested function expansion) for node.
    member this.pos = this.positions |> List.tryHead
    /// Return list of all line numbers for the AstNode.  Where multiple line numbers due to function expansion
    /// the highest level call is first and lowest last.
    member x.positions =
        match x with
        | Int (nw) -> nw.Positions
        | Float (nw) -> nw.Positions
        | String (nw) -> nw.Positions
        | Docstring (nw) -> nw.Positions
        | TypedVariable (nw) -> nw.Positions
        | TypedValue (nw) -> nw.Positions
        | VariableBinding (nw) -> nw.Positions
        | BinaryOperation (nw) -> nw.Positions
        | Negation (nw) -> nw.Positions
        | ParseRelPos (nw) -> nw.Positions
        | RelPos (nw) -> nw.Positions
        | Slice (nw) -> nw.Positions
        | Mutation (nw) -> nw.Positions
        | DotMod (nw) -> nw.Positions
        | Part (nw) -> nw.Positions
        | Marker (nw) -> nw.Positions
        | PartId (nw) -> nw.Positions
        | InlineDna (nw) -> nw.Positions
        | InlineProtein (nw) -> nw.Positions
        | HetBlock (nw) -> nw.Positions
        | Gene (nw) -> nw.Positions
        | L2Id (nw) -> nw.Positions
        | L2Element (nw) -> nw.Positions
        | L2Expression (nw) -> nw.Positions
        | Roughage (nw) -> nw.Positions
        | ParsePragma (nw) -> nw.Positions
        | Pragma (nw) -> nw.Positions
        | Block (nw) -> nw.Positions
        | FunctionDef (nw) -> nw.Positions
        | FunctionLocals (nw) -> nw.Positions
        | FunctionCall (nw) -> nw.Positions
        | Assembly (nw) -> nw.Positions
        | ParseError (nw) -> nw.Positions
        | Splice _ -> []

    /// Get a string representation of the type of this node.
    member x.TypeName = Utils.getUnionCaseName x

// ----- general programming nodes ------
/// A binding from a name to a type and value.
and VariableBinding =
    { Name: string
      Type: GslVariableType
      Value: AstNode }

/// A parsed function.
/// Body should be a Block, and the first line of the block should be FunctionLocals.
and ParseFunction =
    { Name: string
      ArgumentNames: string list
      Body: AstNode }

/// In-block declaration of the local variables passed in as function arguments.
/// This is used as a placeholder inside the block to allow for easy block-scoped fold operations.
// This is a record type to allow for easy later extension, to add support for advanced features
// like functions with default arguments.
and FunctionLocals = { Names: string list }

/// A function invocation.
and FunctionCall =
    { Name: string
      Arguments: AstNode list }

/// Binary operation on two nodes.
and BinaryOperation =
    { Operator: BinaryOperator
      Left: AstNode
      Right: AstNode }

// ----- domain-specific nodes ------

/// Parse type for pragmas.  Values may be variables.
and ParsePragma = { Name: string; Values: AstNode list }

/// Enclosing node for recursively-defined parts.
and ParsePart =
    { BasePart: AstNode
      Modifiers: AstNode list
      Pragmas: AstNode list
      IsForward: bool }

/// Qualifiers on relative positioning specifications.
and RelPosQualifier =
    | S
    | E
    | A
    | AS
    | SA
    | AE
    | EA

/// Which side of the slice expression does this appear?
and RelPosPosition =
    | Left
    | Right

/// Relative positioning.
/// i should reduce to an integer
and ParseRelPos =
    { Item: AstNode
      Qualifier: RelPosQualifier option
      Position: RelPosPosition }

/// Slicing.
and ParseSlice =
    { Left: AstNode
      LeftApprox: bool
      Right: AstNode
      RightApprox: bool }

// ------ GSL level 2 syntax ------

// At the moment we don't explicitly support variables in Level 2, but we've still broken out
// the constituent parts as AST nodes for future flexibility.

/// Level 2 identifier.
and L2Id =
    { Prefix: Node<string> option
      Id: Node<string> }
    member this.String =
        match this.Prefix with
        | None -> this.Id.Value
        | Some prefix -> sprintf "%s.%s" prefix.Value this.Id.Value

/// Level 2 element, eg pABC1>gDEF2.
/// Both subnodes should resolve to L2Id.
and L2Element = { Promoter: AstNode; Target: AstNode }

/// A level 2 expression, eg z^ ; a>b ; c > d
and L2Expression =
    { Locus: AstNode option
      Parts: AstNode list }

// ------ Roughage definitions ------

// GSLc support parsing Roughage as a preprocessor-style step.  Roughage is then converted to L2 GSL.
// Because we end up doing a fairly transparent conversion step, we parse a lot of roughage directly
// into the L2 datatypes.

and RoughagePTPair =
    { Promoter: Node<L2Id>
      Target: Node<L2Id> }
    member this.ToString(dir) =
        match dir with
        | RoughageFwd -> sprintf "%s>%s" this.Promoter.Value.String this.Target.Value.String
        | RoughageRev -> sprintf "%s<%s" this.Target.Value.String this.Promoter.Value.String

and RoughagePartDirection =
    | RoughageFwd
    | RoughageRev

/// Single part in a roughage expression.
and RoughageElement =
    { PromoterAndTarget1: Node<RoughagePTPair>
      PromoterAndTarget2: Node<RoughagePTPair> option
      Marker: Node<string> option }

/// One classic roughage construct.
and Roughage =
    { Locus: Node<L2Id> option
      Marker: Node<string> option
      Parts: Node<RoughageElement> list }
    member x.HasMarker =
        match x.Marker with
        | None -> // No marker attached to the locus knockout
            match x.Parts
                  |> List.tryPick (fun re -> re.Value.Marker) with
            | None -> None // No marker attached to a part either
            | Some (mw) -> Some(mw.Value)
        | Some (mw) -> Some(mw.Value) // Yes there is a marker attached to the locus knockout

/// Helper function for pushing new source code positions onto stack during function expansion
/// Most recently (first) postition refers to highest level function invocation.
let prependPositionsAstNode (newPos: SourcePosition list): AstNode -> AstNode =
    function
    | Int node -> node |> Node.prependPositionsNode newPos |> Int
    | Float node -> node |> Node.prependPositionsNode newPos |> Float
    | String node -> node |> Node.prependPositionsNode newPos |> String
    | Docstring node ->
        node
        |> Node.prependPositionsNode newPos
        |> Docstring
    | TypedVariable node ->
        node
        |> Node.prependPositionsNode newPos
        |> TypedVariable
    | TypedValue node ->
        node
        |> Node.prependPositionsNode newPos
        |> TypedValue
    | VariableBinding node ->
        node
        |> Node.prependPositionsNode newPos
        |> VariableBinding
    | BinaryOperation node ->
        node
        |> Node.prependPositionsNode newPos
        |> BinaryOperation
    | Negation node ->
        node
        |> Node.prependPositionsNode newPos
        |> Negation
    | ParseRelPos node ->
        node
        |> Node.prependPositionsNode newPos
        |> ParseRelPos
    | RelPos node -> node |> Node.prependPositionsNode newPos |> RelPos
    | Slice node -> node |> Node.prependPositionsNode newPos |> Slice
    | Mutation node ->
        node
        |> Node.prependPositionsNode newPos
        |> Mutation
    | DotMod node -> node |> Node.prependPositionsNode newPos |> DotMod
    | Part node -> node |> Node.prependPositionsNode newPos |> Part
    | Marker node -> node |> Node.prependPositionsNode newPos |> Marker
    | PartId node -> node |> Node.prependPositionsNode newPos |> PartId
    | InlineDna node ->
        node
        |> Node.prependPositionsNode newPos
        |> InlineDna
    | InlineProtein node ->
        node
        |> Node.prependPositionsNode newPos
        |> InlineProtein
    | HetBlock node ->
        node
        |> Node.prependPositionsNode newPos
        |> HetBlock
    | Gene node -> node |> Node.prependPositionsNode newPos |> Gene
    | L2Id node -> node |> Node.prependPositionsNode newPos |> L2Id
    | L2Element node ->
        node
        |> Node.prependPositionsNode newPos
        |> L2Element
    | L2Expression node ->
        node
        |> Node.prependPositionsNode newPos
        |> L2Expression
    | Roughage node ->
        node
        |> Node.prependPositionsNode newPos
        |> Roughage
    | ParsePragma node ->
        node
        |> Node.prependPositionsNode newPos
        |> ParsePragma
    | Pragma node -> node |> Node.prependPositionsNode newPos |> Pragma
    | Block node -> node |> Node.prependPositionsNode newPos |> Block
    | FunctionDef node ->
        node
        |> Node.prependPositionsNode newPos
        |> FunctionDef
    | FunctionLocals node ->
        node
        |> Node.prependPositionsNode newPos
        |> FunctionLocals
    | FunctionCall node ->
        node
        |> Node.prependPositionsNode newPos
        |> FunctionCall
    | Assembly node ->
        node
        |> Node.prependPositionsNode newPos
        |> Assembly
    | ParseError node ->
        node
        |> Node.prependPositionsNode newPos
        |> ParseError
    | Splice _ as x -> x // Slices are defined to have no position so don't try to update (see .pos below)

// ------ Active patterns on the AST of general interest ------

// Note: active patterns allow expressing complex match idioms in a compact syntax.
// This technique allows us to create "categories" of nodes and use them in pattern matching,
// as well as create helpful unpackings of those node structures.

/// An active pattern to match only leaf nodes.
/// As the tree grows new leaves, this pattern should be updated which will automatically
/// propagate to all of the clients of this pattern.
let (|Leaf|_|) (node: AstNode): AstNode option =
    match node with
    | Int _
    | Float _
    | String _
    | Docstring _
    | DotMod _
    | Mutation _
    | Marker _
    | PartId _
    | InlineDna _
    | InlineProtein _
    | HetBlock _
    | Gene _
    | TypedVariable _
    | FunctionLocals _
    | L2Id _
    | Roughage _
    | Pragma _
    | RelPos _
    | ParseError _
    | Splice _ -> Some(node)
    | _ -> None

/// Match parts and their base parts together as a pair.
/// Unpacks both nodes for convenience.
// TODO: add other part combos as we need them
let (|AssemblyPart|GenePart|RecursivePart|Other|) (node: AstNode) =
    match node with
    | Part (pw) ->
        match pw.Value.BasePart with
        | Assembly (aw) -> AssemblyPart(pw, aw)
        | Gene (gp) -> GenePart(pw, gp)
        | Part (pwInner) -> RecursivePart(pw, pwInner)
        | _ -> Other
    | _ -> Other

/// Match all nodes which are valid as base parts.
let (|ValidBasePart|_|) (node: AstNode): AstNode option =
    match node with
    | TypedVariable _
    | PartId _
    | Marker _
    | InlineDna _
    | InlineProtein _
    | HetBlock _
    | Gene _
    | Part _
    | Assembly _ -> Some node
    | _ -> None

/// Match all nodes which have no literal representation in source code.
let (|BookkeepingNode|_|) (node: AstNode): AstNode option =
    match node with
    | FunctionLocals _ -> Some node
    | _ -> None

/// Extract the type of a node if it is a numeric variable.
let (|IntVariable|FloatVariable|OtherVariable|NotAVariable|) (node: AstNode) =
    match node with
    | TypedVariable ({ Value = (_, t); Positions = _ }) ->
        match t with
        | IntType -> IntVariable
        | FloatType -> FloatVariable
        | _ -> OtherVariable(t)
    | _ -> NotAVariable

/// Match nodes allowed in math expressions.
let (|AllowedInMathExpression|_|) (node: AstNode) =
    match node with
    | Int _
    | IntVariable -> Some node
    | _ -> None

/// Match variable declarations that effectively create a pathological self-reference.
let (|SelfReferentialVariable|_|) (node: AstNode) =
    match node with
    | VariableBinding (vb) ->
        match vb.Value.Value with
        | TypedVariable (vbInner) -> // variable pointing to another variable
            if vb.Value.Name = fst vbInner.Value then Some vb else None
        | _ -> None
    | _ -> None


// ====================
// helper functions for creating AST nodes in the parser
// ====================

// ------ deriving source code positions ------

/// Create a new position bracketed by a pair of positions.
let posBracket (left: AstNode) (right: AstNode): SourcePosition option =
    match left.pos, right.pos with
    | Some leftPos, Some rightPos ->
        { SourcePosition.Start = leftPos.Start
          End = rightPos.End }
        |> Some
    | _ -> None

/// Create a new position bracketed by the first and last item in a list of nodes that have positions
let posFromList (nodes: AstNode list): SourcePosition option =
    let rec go (head: AstNode option) (tail: AstNode list) =
        match head, tail with
        | Some (h), [] -> h.pos
        | Some (h), [ t ] -> posBracket h t
        | Some (h), _ :: tl -> go (Some(h)) tl
        | None, [] -> None
        | None, [ t ] -> t.pos
        | None, hd :: tl -> go (Some(hd)) tl

    go None nodes


// ------ general-purpose ------

/// Create a parse error with message and position.
let createParseError (msg: string) (positions: SourcePosition list): AstNode =
    ParseError
        { Node.Value = msg
          Positions = positions }

/// Wrap a value with position taken from another node.
let nodeWrapWithNodePosition (node: AstNode) (item: 'a): Node<'a> =
    { Node.Value = item
      Positions = node.positions }

/// Convert a string token to a TypedVariable
let tokenToVariable (token: PString) (variableType: GslVariableType): AstNode =
    let name = token.Item
    TypedVariable(Node.wrapNodeWithTokenPosition token (name, variableType))

// ------ general programming ------

/// Parse two integer literals separated by a dot as a float.
let createFloat (intPart: PInt) (fracPart: PInt): AstNode =
    // position is bracketed by the two pieces
    let position =
        { SourcePosition.Start = intPart.Position.Start
          End = fracPart.Position.End }

    let value =
        sprintf "%d.%d" intPart.Item fracPart.Item
        |> float

    Float
        { Node.Value = value
          Positions = [ position ] }

/// Create a binary operation node from two other AST nodes.
let createBinaryOp (op: BinaryOperator) (left: AstNode) (right: AstNode): AstNode =
    BinaryOperation
        { Node.Value =
              { BinaryOperation.Operator = op
                Left = left
                Right = right }
          Positions = posBracket left right |> Option.toList }

/// Create an AST node for negation.
let negate (node: AstNode): AstNode =
    Negation(nodeWrapWithNodePosition node node)

/// Create an AST node for a typed variable declaration.
// TODO: improve positioning
let createVariableBinding (name: PString) (varType: GslVariableType) (value: AstNode): AstNode =
    let binding =
        { VariableBinding.Name = name.Item
          Type = varType
          Value = value }

    VariableBinding(Node.wrapNodeWithTokenPosition name binding)


/// Create an AST node for a typed value passed to a function argument.
let createTypedValue (variableType: GslVariableType) (value: AstNode): AstNode =
    TypedValue(nodeWrapWithNodePosition value (variableType, value))

/// Create an AST node for a function declaration.
let createFunctionDeclaration (name: PString) (args: string list) (bodyLines: AstNode list): AstNode =
    let functionLocals =
        FunctionLocals(Node.wrapNodeWithTokenPosition name { FunctionLocals.Names = args })
    // tack the function local variables on the front of the block
    let block =
        Block(Node.wrapNode (functionLocals :: bodyLines))

    let parseFunction =
        { ParseFunction.Name = name.Item
          ArgumentNames = args
          Body = block }

    FunctionDef(Node.wrapNodeWithTokenPosition name parseFunction)

/// Create an AST node for a function call.
let createFunctionCall (name: PString) (args: AstNode list): AstNode =
    let functionCall =
        { FunctionCall.Name = name.Item
          Arguments = args }

    FunctionCall(Node.wrapNodeWithTokenPosition name functionCall)

/// Create a pragma from pieces.
let createPragma (pname: PString) (arguments: AstNode list): AstNode =
    let parsePragma =
        { ParsePragma.Name = pname.Item
          Values = arguments }
    // Take position from the name.  Could try to be fancier here in the future.
    ParsePragma(Node.wrapNodeWithTokenPosition pname parsePragma)


// ------ creating nodes for parts and assemblies ------


let private stringToRelPosQualifier (input: string): RelPosQualifier =
    match input.ToUpper() with
    | "S" -> S
    | "E" -> E
    | "A" -> A
    | "AS" -> AS
    | "SA" -> SA
    | "AE" -> AE
    | "EA" -> EA
    | x -> failwithf "%s is not a valid qualifier for a relative position." x

let relPosQualifierToString (rpq: RelPosQualifier): string =
    match rpq with
    | S -> "S"
    | E -> "E"
    | A -> "A"
    | AS -> "AS"
    | SA -> "SA"
    | AE -> "AE"
    | EA -> "EA"

/// Encode the logic for parsing and computing relative positions in slices.
/// Use the position from the number as the position of this token.
let createParseRelPos (number: AstNode) (maybeQualifier: PString option) (position: RelPosPosition): AstNode =
    let parseRelPos =
        { ParseRelPos.Item = number
          Qualifier = None
          Position = position }

    match maybeQualifier with
    | None ->
        // basic case, just given a number
        ParseRelPos(nodeWrapWithNodePosition number parseRelPos)
    | Some qualifier ->
        // We've been passed a qualifying string.  Parse it as a valid union case.
        let qualifier = stringToRelPosQualifier qualifier.Item

        let parseRelPos =
            { parseRelPos with
                  Qualifier = Some qualifier }

        ParseRelPos(nodeWrapWithNodePosition number parseRelPos)

/// Create a parse slice AST node.
let createParseSlice (leftRPInt: AstNode, leftRPQual: PString option)
                     (rightRPInt: AstNode, rightRPQual: PString option)
                     (lApprox: bool)
                     (rApprox: bool)
                     : AstNode =
    let left =
        createParseRelPos leftRPInt leftRPQual Left

    let right =
        createParseRelPos rightRPInt rightRPQual Right

    let pos = posBracket left right |> Option.toList

    let parseSlice =
        { ParseSlice.Left = left
          Right = right
          LeftApprox = lApprox
          RightApprox = rApprox }

    Slice
        ({ Node.Value = parseSlice
           Positions = pos })


/// Create a mutation AST node.
let createMutation (value: PString) (mutationType: MutationType): AstNode =
    let mutationTerm = value.Item
    let from = mutationTerm.[1]
    let tto = mutationTerm.[mutationTerm.Length - 1]

    let position =
        Convert.ToInt32(mutationTerm.[2..mutationTerm.Length - 2])

    let mutation =
        { Mutation.From = from
          To = tto
          Location = position
          Type = mutationType }

    Mutation
        ({ Value = mutation
           Positions = [ value.Position ] })

/// Create a top-level part.
let createPart (modifiers: AstNode list) (pragmas: AstNode list) (basePart: AstNode): AstNode =
    Part
        ({ Node.Value =
               { ParsePart.BasePart = basePart
                 Modifiers = modifiers
                 Pragmas = pragmas
                 IsForward = true }
           Positions = basePart.positions })

/// Create a top-level part with empty collections and default values from a base part.
let createPartWithBase: AstNode -> AstNode = createPart [] []

/// Create a top-level part given a gene ID.
let createGenePart (gene: PString) (linker: Linker option) =
    // The base part for this part will be a Gene AST node.
    createPartWithBase
        (Gene
            ({ Node.Value =
                   { ParseGene.Gene = gene.Item
                     Linker = linker }
               Positions = [ gene.Position ] }))

/// Capture a list of parsed mods and stuff them into their associated part.
let stuffModsIntoPart (astPart: AstNode) (modifiers: AstNode list): AstNode =
    match astPart with
    | Part partWrapper ->
        let part = partWrapper.Value

        let stuffedPart =
            { part with
                  Modifiers = part.Modifiers @ modifiers }

        Part(nodeWrapWithNodePosition astPart stuffedPart)
    | x -> failwithf "Mods may only be applied to Parts.  Tried to apply mods to %A." x

/// Capture a list of parsed inline pragmas and stuff them into their associated part.
let stuffPragmasIntoPart (astPart: AstNode) (prags: AstNode list): AstNode =
    match astPart with
    | Part partWrapper ->
        let part = partWrapper.Value

        let stuffedPart =
            { part with
                  Pragmas = part.Pragmas @ prags }

        Part(nodeWrapWithNodePosition astPart stuffedPart)
    | x -> failwithf "Inline pragmas may only be applied to Parts.  Tried to apply pragmas to %A." x

/// Reverse the direction of a part.
let revPart (astPart: AstNode): AstNode =
    match astPart with
    | Part partWrapper ->
        Part
            (nodeWrapWithNodePosition
                astPart
                 { partWrapper.Value with
                       IsForward = false })
    | x -> failwithf "Can only apply the ! operator to Parts.  Tried to reverse a %A." x

/// Create a part whose base part is an assembly of the passed list of parts.
let createAssemblyPart (parts: AstNode list): AstNode =
    let pos = posFromList parts

    let assembly =
        Assembly
            ({ Node.Value = parts
               Positions = pos |> Option.toList })

    createPart [] [] assembly

// ------ creating level 2 GSL nodes ------

let createL2IdNode (maybePrefix: Node<string> option) (id: Node<string>): Node<L2Id> =
    let pos =
        match maybePrefix with
        | Some prefix ->
            posBracket (String(prefix)) (String(id))
            |> Option.toList // be lazy and wrap these as nodes to use existing function
        | None -> id.Positions

    { Node.Value = { L2Id.Prefix = maybePrefix; Id = id }
      Positions = pos }

/// Create a level 2 id from optional prefix and id
let createL2Id (prefix: Node<string> option) (id: Node<string>) = L2Id(createL2IdNode prefix id)

/// Create a level 2 element from a promoter and target.
/// Promoter and target should be L2 IDs.
let createL2Element (promoter: AstNode) (target: AstNode): AstNode =
    let pos =
        posBracket promoter target |> Option.toList

    L2Element
        { Node.Value =
               { L2Element.Promoter = promoter
                 Target = target }
          Positions = pos }

/// Create a level 2 expression from optional locus and list of elements.
let createL2Expression (maybeLocus: AstNode option) (parts: AstNode list): AstNode =
    let pos =
        match maybeLocus with
        | Some locus -> posFromList (locus :: parts)
        | None -> posFromList parts

    L2Expression
        ({ Node.Value =
               { L2Expression.Locus = maybeLocus
                 Parts = parts }
           Positions = pos |> Option.toList })

// ------ creating Roughage AST node ------

let createRoughagePart (dir: RoughagePartDirection) (promoter: Node<L2Id>) (target: Node<L2Id>): Node<RoughagePTPair> =
    let elem =
        { RoughagePTPair.Promoter = promoter
          Target = target }

    let pos =
        match dir with
        | RoughageFwd -> posBracket (L2Id(promoter)) (L2Id(target))
        | RoughageRev -> posBracket (L2Id(target)) (L2Id(promoter))

    { Node.Value = elem
      Positions = pos |> Option.toList }

let createRoughageElement (partFwd: Node<RoughagePTPair>)
                          (partRev: Node<RoughagePTPair> option)
                          (marker: Node<string> option)
                          : Node<RoughageElement> =
    let pos = partFwd.Positions

    { Node.Value =
          { RoughageElement.PromoterAndTarget1 = partFwd
            PromoterAndTarget2 = partRev
            Marker = marker }
      Positions = pos }

let createRoughageLine (locus: Node<L2Id> option, marker: Node<string> option)
                       (parts: Node<RoughageElement> list)
                       : AstNode =
    // be lazy and use the position of whatever the first part is
    let pos =
        match parts with
        | [] -> []
        | hd :: _ -> hd.Positions

    Roughage
        ({ Node.Value =
               { Roughage.Locus = locus
                 Marker = marker
                 Parts = parts }
           Positions = pos })
