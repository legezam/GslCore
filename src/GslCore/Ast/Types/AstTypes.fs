/// Types and functions for AST creation.
namespace GslCore.Ast.Types

open GslCore.Constants
open GslCore


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
with
    override this.ToString() =
        sprintf "[Linker O=%s; %s-%s]" this.Orient this.Linker1 this.Linker2

type ParseGene = { Gene: string; Linker: Linker option }
with
    override this.ToString () =
        match this.Linker with
        | None -> sprintf "[Gene=%s]" this.Gene
        | Some linker -> sprintf "[Gene=%s;Linker=%O]" this.Gene linker

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
    | ParseRelPos of Node<ParseRelativePosition>
    | RelPos of Node<RelativePosition>
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
    
    with
    
    override this.ToString() =
        let modifiers = this.Modifiers |> List.map (sprintf "%O") |> String.concat ","
        let pragmas = this.Pragmas |> List.map (sprintf "%O") |> String.concat ","
        sprintf "[Base: %O; Mod: %s; Prag: %s]" this.BasePart modifiers pragmas

/// Qualifiers on relative positioning specifications.
/// S = Start
/// E = End
/// A = Amino acid (coordinates are considered in Amino Acid coordinate space)
/// AS = SA = A
/// ES = SE
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
    with
        override this.ToString() =
            match this with
            | Left -> "L"
            | Right -> "R"

/// Raw Relative Position from parsing. Becomes `RelativePosition`
and ParseRelativePosition =
    { Item: AstNode
      Qualifier: RelPosQualifier option
      Position: RelPosPosition }
    
    with
        override this.ToString() =
            
            sprintf "[RelPos P:%O Q:%A %O]" this.Position this.Qualifier this.Item

/// Slicing.
and ParseSlice =
    { Left: AstNode
      LeftApprox: bool
      Right: AstNode
      RightApprox: bool }
    
    with
        override this.ToString() =
            let leftApprox = if this.LeftApprox then "~" else ""
            let rightApprox = if this.RightApprox then "~" else ""
            sprintf "[Slice {%s%O-%s%O}]" leftApprox this.Left rightApprox this.Right

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

