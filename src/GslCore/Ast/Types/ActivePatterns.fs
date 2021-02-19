[<AutoOpen>]
module GslCore.Ast.Types.ActivePatterns


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

