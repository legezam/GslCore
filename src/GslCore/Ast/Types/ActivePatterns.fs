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
    | AstNode.Int _
    | AstNode.Float _
    | AstNode.String _
    | AstNode.Docstring _
    | AstNode.DotMod _
    | AstNode.Mutation _
    | AstNode.Marker _
    | AstNode.PartId _
    | AstNode.InlineDna _
    | AstNode.InlineProtein _
    | AstNode.HetBlock _
    | AstNode.Gene _
    | AstNode.TypedVariable _
    | AstNode.FunctionLocals _
    | AstNode.L2Id _
    | AstNode.Roughage _
    | AstNode.Pragma _
    | AstNode.RelPos _
    | AstNode.ParseError _
    | AstNode.Splice _ -> Some(node)
    | _ -> None

/// Match parts and their base parts together as a pair.
/// Unpacks both nodes for convenience.
// TODO: add other part combos as we need them
let (|AssemblyPart|GenePart|RecursivePart|Other|) (node: AstNode) =
    match node with
    | AstNode.Part (pw) ->
        match pw.Value.BasePart with
        | AstNode.Assembly (aw) -> AssemblyPart(pw, aw)
        | AstNode.Gene (gp) -> GenePart(pw, gp)
        | AstNode.Part (pwInner) -> RecursivePart(pw, pwInner)
        | _ -> Other
    | _ -> Other

/// Match all nodes which are valid as base parts.
let (|ValidBasePart|_|) (node: AstNode): AstNode option =
    match node with
    | AstNode.TypedVariable _
    | AstNode.PartId _
    | AstNode.Marker _
    | AstNode.InlineDna _
    | AstNode.InlineProtein _
    | AstNode.HetBlock _
    | AstNode.Gene _
    | AstNode.Part _
    | AstNode.Assembly _ -> Some node
    | _ -> None

/// Match all nodes which have no literal representation in source code.
let (|BookkeepingNode|_|) (node: AstNode): AstNode option =
    match node with
    | AstNode.FunctionLocals _ -> Some node
    | _ -> None

/// Extract the type of a node if it is a numeric variable.
let (|IntVariable|FloatVariable|OtherVariable|NotAVariable|) (node: AstNode) =
    match node with
    | AstNode.TypedVariable ({ Value = (_, t); Positions = _ }) ->
        match t with
        | IntType -> IntVariable
        | FloatType -> FloatVariable
        | _ -> OtherVariable(t)
    | _ -> NotAVariable

/// Match nodes allowed in math expressions.
let (|AllowedInMathExpression|_|) (node: AstNode) =
    match node with
    | AstNode.Int _
    | IntVariable -> Some node
    | _ -> None

/// Match variable declarations that effectively create a pathological self-reference.
let (|SelfReferentialVariable|_|) (node: AstNode) =
    match node with
    | AstNode.VariableBinding variableBindingWrapper ->
        match variableBindingWrapper.Value.Value with
        | AstNode.TypedVariable innerVariableBindingWrapper -> // variable pointing to another variable
            let (innerName, _) = innerVariableBindingWrapper.Value
            let outerName = variableBindingWrapper.Value.Name
            if outerName = innerName then Some variableBindingWrapper else None
        | _ -> None
    | _ -> None
