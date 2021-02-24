module GslCore.Ast.Types.Utils

/// Helper function for pushing new source code positions onto stack during function expansion
/// Most recently (first) postition refers to highest level function invocation.
let prependPositionsAstNode (newPos: SourcePosition list): AstNode -> AstNode =
    function
    | AstNode.Int node ->
        node
        |> Node.prependPositionsNode newPos
        |> AstNode.Int
    | AstNode.Float node ->
        node
        |> Node.prependPositionsNode newPos
        |> AstNode.Float
    | AstNode.String node ->
        node
        |> Node.prependPositionsNode newPos
        |> AstNode.String
    | AstNode.Docstring node ->
        node
        |> Node.prependPositionsNode newPos
        |> AstNode.Docstring
    | AstNode.TypedVariable node ->
        node
        |> Node.prependPositionsNode newPos
        |> AstNode.TypedVariable
    | AstNode.TypedValue node ->
        node
        |> Node.prependPositionsNode newPos
        |> AstNode.TypedValue
    | AstNode.VariableBinding node ->
        node
        |> Node.prependPositionsNode newPos
        |> AstNode.VariableBinding
    | AstNode.BinaryOperation node ->
        node
        |> Node.prependPositionsNode newPos
        |> AstNode.BinaryOperation
    | AstNode.Negation node ->
        node
        |> Node.prependPositionsNode newPos
        |> AstNode.Negation
    | AstNode.ParseRelPos node ->
        node
        |> Node.prependPositionsNode newPos
        |> AstNode.ParseRelPos
    | AstNode.RelPos node ->
        node
        |> Node.prependPositionsNode newPos
        |> AstNode.RelPos
    | AstNode.Slice node ->
        node
        |> Node.prependPositionsNode newPos
        |> AstNode.Slice
    | AstNode.Mutation node ->
        node
        |> Node.prependPositionsNode newPos
        |> AstNode.Mutation
    | AstNode.DotMod node ->
        node
        |> Node.prependPositionsNode newPos
        |> AstNode.DotMod
    | AstNode.Part node ->
        node
        |> Node.prependPositionsNode newPos
        |> AstNode.Part
    | AstNode.Marker node ->
        node
        |> Node.prependPositionsNode newPos
        |> AstNode.Marker
    | AstNode.PartId node ->
        node
        |> Node.prependPositionsNode newPos
        |> AstNode.PartId
    | AstNode.InlineDna node ->
        node
        |> Node.prependPositionsNode newPos
        |> AstNode.InlineDna
    | AstNode.InlineProtein node ->
        node
        |> Node.prependPositionsNode newPos
        |> AstNode.InlineProtein
    | AstNode.HetBlock node ->
        node
        |> Node.prependPositionsNode newPos
        |> AstNode.HetBlock
    | AstNode.Gene node ->
        node
        |> Node.prependPositionsNode newPos
        |> AstNode.Gene
    | AstNode.L2Id node ->
        node
        |> Node.prependPositionsNode newPos
        |> AstNode.L2Id
    | AstNode.L2Element node ->
        node
        |> Node.prependPositionsNode newPos
        |> AstNode.L2Element
    | AstNode.L2Expression node ->
        node
        |> Node.prependPositionsNode newPos
        |> AstNode.L2Expression
    | AstNode.Roughage node ->
        node
        |> Node.prependPositionsNode newPos
        |> AstNode.Roughage
    | AstNode.ParsePragma node ->
        node
        |> Node.prependPositionsNode newPos
        |> AstNode.ParsePragma
    | AstNode.Pragma node ->
        node
        |> Node.prependPositionsNode newPos
        |> AstNode.Pragma
    | AstNode.Block node ->
        node
        |> Node.prependPositionsNode newPos
        |> AstNode.Block
    | AstNode.FunctionDef node ->
        node
        |> Node.prependPositionsNode newPos
        |> AstNode.FunctionDef
    | AstNode.FunctionLocals node ->
        node
        |> Node.prependPositionsNode newPos
        |> AstNode.FunctionLocals
    | AstNode.FunctionCall node ->
        node
        |> Node.prependPositionsNode newPos
        |> AstNode.FunctionCall
    | AstNode.Assembly node ->
        node
        |> Node.prependPositionsNode newPos
        |> AstNode.Assembly
    | AstNode.ParseError node ->
        node
        |> Node.prependPositionsNode newPos
        |> AstNode.ParseError
    | AstNode.Splice _ as x -> x // Slices are defined to have no position so don't try to update (see .pos below)

/// Create a parse error with message and position.
let createParseError (msg: string) (positions: SourcePosition list): AstNode =
    AstNode.ParseError
        { Node.Value = msg
          Positions = positions }

/// Wrap a value with position taken from another node.
let nodeWrapWithNodePosition (node: AstNode) (item: 'a): Node<'a> =
    { Node.Value = item
      Positions = node.positions }

/// Convert a string token to a TypedVariable
let tokenToVariable (token: PString) (variableType: GslVariableType): AstNode =
    let name = token.Item
    AstNode.TypedVariable(Node.wrapNodeWithTokenPosition token (name, variableType))
