module GslCore.Ast.Types.Parts

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
        ParseRelPos(Utils.nodeWrapWithNodePosition number parseRelPos)
    | Some qualifier ->
        // We've been passed a qualifying string.  Parse it as a valid union case.
        let qualifier = stringToRelPosQualifier qualifier.Item

        let parseRelPos =
            { parseRelPos with
                  Qualifier = Some qualifier }

        ParseRelPos(Utils.nodeWrapWithNodePosition number parseRelPos)

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

    let pos = SourcePositionBuilder.fromBracket left right |> Option.toList

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
        System.Convert.ToInt32(mutationTerm.[2..mutationTerm.Length - 2])

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

        Part(Utils.nodeWrapWithNodePosition astPart stuffedPart)
    | x -> failwithf "Mods may only be applied to Parts.  Tried to apply mods to %A." x

/// Capture a list of parsed inline pragmas and stuff them into their associated part.
let stuffPragmasIntoPart (astPart: AstNode) (prags: AstNode list): AstNode =
    match astPart with
    | Part partWrapper ->
        let part = partWrapper.Value

        let stuffedPart =
            { part with
                  Pragmas = part.Pragmas @ prags }

        Part(Utils.nodeWrapWithNodePosition astPart stuffedPart)
    | x -> failwithf "Inline pragmas may only be applied to Parts.  Tried to apply pragmas to %A." x

/// Reverse the direction of a part.
let revPart (astPart: AstNode): AstNode =
    match astPart with
    | Part partWrapper ->
        Part
            (Utils.nodeWrapWithNodePosition
                astPart
                 { partWrapper.Value with
                       IsForward = false })
    | x -> failwithf "Can only apply the ! operator to Parts.  Tried to reverse a %A." x

/// Create a part whose base part is an assembly of the passed list of parts.
let createAssemblyPart (parts: AstNode list): AstNode =
    let pos = SourcePositionBuilder.fromList parts

    let assembly =
        Assembly
            ({ Node.Value = parts
               Positions = pos |> Option.toList })

    createPart [] [] assembly
