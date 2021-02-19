module GslCore.Ast.Types.Utils

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
