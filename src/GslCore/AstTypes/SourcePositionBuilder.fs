module GslCore.AstTypes.SourcePositionBuilder

// ------ deriving source code positions ------

/// Create a new position bracketed by a pair of positions.
let fromBracket (left: AstNode) (right: AstNode): SourcePosition option =
    match left.pos, right.pos with
    | Some leftPos, Some rightPos ->
        { SourcePosition.Start = leftPos.Start
          End = rightPos.End }
        |> Some
    | _ -> None

/// Create a new position bracketed by the first and last item in a list of nodes that have positions
let fromList (nodes: AstNode list): SourcePosition option =
    let rec go (head: AstNode option) (tail: AstNode list) =
        match head, tail with
        | Some (h), [] -> h.pos
        | Some (h), [ t ] -> fromBracket h t
        | Some (h), _ :: tl -> go (Some(h)) tl
        | None, [] -> None
        | None, [ t ] -> t.pos
        | None, hd :: tl -> go (Some(hd)) tl

    go None nodes