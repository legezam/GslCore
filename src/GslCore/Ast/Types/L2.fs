module GslCore.Ast.Types.L2

let createL2IdNode (maybePrefix: Node<string> option) (id: Node<string>): Node<L2Id> =
    let pos =
        match maybePrefix with
        | Some prefix ->
            SourcePositionBuilder.fromBracket (AstNode.String(prefix)) (AstNode.String(id))
            |> Option.toList // be lazy and wrap these as nodes to use existing function
        | None -> id.Positions

    { Node.Value = { L2Id.Prefix = maybePrefix; Id = id }
      Positions = pos }

/// Create a level 2 id from optional prefix and id
let createL2Id (prefix: Node<string> option) (id: Node<string>) = AstNode.L2Id(createL2IdNode prefix id)

/// Create a level 2 element from a promoter and target.
/// Promoter and target should be L2 IDs.
let createL2Element (promoter: AstNode) (target: AstNode): AstNode =
    let pos =
        SourcePositionBuilder.fromBracket promoter target |> Option.toList

    AstNode.L2Element
        { Node.Value =
               { L2Element.Promoter = promoter
                 Target = target }
          Positions = pos }

/// Create a level 2 expression from optional locus and list of elements.
let createL2Expression (maybeLocus: AstNode option) (parts: AstNode list): AstNode =
    let pos =
        match maybeLocus with
        | Some locus -> SourcePositionBuilder.fromList (locus :: parts)
        | None -> SourcePositionBuilder.fromList parts

    AstNode.L2Expression
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
        | RoughageFwd -> SourcePositionBuilder.fromBracket (AstNode.L2Id(promoter)) (AstNode.L2Id(target))
        | RoughageRev -> SourcePositionBuilder.fromBracket (AstNode.L2Id(target)) (AstNode.L2Id(promoter))

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

    AstNode.Roughage
        ({ Node.Value =
               { Roughage.Locus = locus
                 Marker = marker
                 Parts = parts }
           Positions = pos })


