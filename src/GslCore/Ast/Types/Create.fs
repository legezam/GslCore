module GslCore.Ast.Types.Create

/// Parse two integer literals separated by a dot as a float.
let float (intPart: PInt) (fracPart: PInt): AstNode =
    // position is bracketed by the two pieces
    let position =
        { SourcePosition.Start = intPart.Position.Start
          End = fracPart.Position.End }

    let value =
        sprintf "%d.%d" intPart.Item fracPart.Item
        |> float

    AstNode.Float
        { Node.Value = value
          Positions = [ position ] }

/// Create a binary operation node from two other AST nodes.
let binaryOperation (op: BinaryOperator) (left: AstNode) (right: AstNode): AstNode =
    AstNode.BinaryOperation
        { Node.Value =
              { BinaryOperation.Operator = op
                Left = left
                Right = right }
          Positions = SourcePositionBuilder.fromBracket left right |> Option.toList }

/// Create an AST node for negation.
let negation (node: AstNode): AstNode =
    AstNode.Negation(Utils.nodeWrapWithNodePosition node node)

/// Create an AST node for a typed variable declaration.
// TODO: improve positioning
let variableBinding (name: PString) (varType: GslVariableType) (value: AstNode): AstNode =
    let binding =
        { VariableBinding.Name = name.Item
          Type = varType
          Value = value }

    AstNode.VariableBinding(Node.wrapNodeWithTokenPosition name binding)


/// Create an AST node for a typed value passed to a function argument.
let typedValue (variableType: GslVariableType) (value: AstNode): AstNode =
    AstNode.TypedValue(Utils.nodeWrapWithNodePosition value (variableType, value))

/// Create an AST node for a function declaration.
let functionDefinition (name: PString) (args: string list) (bodyLines: AstNode list): AstNode =
    let functionLocals =
        AstNode.FunctionLocals(Node.wrapNodeWithTokenPosition name { FunctionLocals.Names = args })
    // tack the function local variables on the front of the block
    let block =
        AstNode.Block(Node.wrapNode (functionLocals :: bodyLines))

    let parseFunction =
        { ParseFunction.Name = name.Item
          ArgumentNames = args
          Body = block }

    AstNode.FunctionDef(Node.wrapNodeWithTokenPosition name parseFunction)

/// Create an AST node for a function call.
let functionCall (name: PString) (args: AstNode list): AstNode =
    let functionCall =
        { FunctionCall.Name = name.Item
          Arguments = args }

    AstNode.FunctionCall(Node.wrapNodeWithTokenPosition name functionCall)

/// Create a pragma from pieces.
let pragma (pname: PString) (arguments: AstNode list): AstNode =
    let parsePragma =
        { ParsePragma.Name = pname.Item
          Values = arguments }
    // Take position from the name.  Could try to be fancier here in the future.
    AstNode.ParsePragma(Node.wrapNodeWithTokenPosition pname parsePragma)
