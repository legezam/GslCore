namespace GslCore.Ast.Types


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


