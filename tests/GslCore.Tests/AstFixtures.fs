/// Fixtures and fixture creation for testing the AST.
module GslCore.AstFixtures


open GslCore.Ast.Types
open GslCore.Ast
open GslCore.Constants
open GslCore.GslResult


/// Parse text and return the contents of the top-level block.
/// Raise an exception if this fails.
let bootstrapParseOnly source =
    match LexAndParse.lexAndParse true (GslSourceCode(source))
          |> GslResult.GetValue with
    | Ok { Result = AstTreeHead (AstNode.Block (b))
           Warnings = _ } -> b.Value
    | x -> failwithf "Illegal: %A" x


let wrapInt v = AstNode.Int(Node.wrapNode v)
let wrapFloat v = AstNode.Float(Node.wrapNode v)
let wrapString v = AstNode.String(Node.wrapNode v)

/// Wrap AST items as a block.
let blockify items = AstNode.Block(Node.wrapNode items)

/// Group AST items as a block, wrapped as a whole tree.
let treeify items = AstTreeHead(blockify items)

/// Make a bare-bones assembly part out of some parts.
let assemble = Parts.createAssemblyPart

let addPragsToPart prags a =
    match a with
    | AstNode.Part (pw) ->
        AstNode.Part
            ({ pw with
                   Value = { pw.Value with Pragmas = prags } })
    | x -> failwithf "Illegal: %A" x

let variableize name v =
    let t =
        match v with
        | AstNode.BinaryOperation _ -> NotYetTyped // right now we only support integer math so this is OK
        | AstNode.Int _ -> NotYetTyped
        | AstNode.Float _ -> NotYetTyped
        | AstNode.String _ -> NotYetTyped
        | AstNode.Part _ -> PartType
        | x -> failwithf "Illegal: %A" x

    AstNode.VariableBinding(Node.wrapNode { Name = name; Type = t; Value = v })

let functionalize name args bodyLines =
    let locals =
        AstNode.FunctionLocals(Node.wrapNode { Names = args })

    AstNode.FunctionDef
        (Node.wrapNode
            { Name = name
              ArgumentNames = args
              Body = blockify (locals :: bodyLines) })

let emptyBlock = AstNode.Block(Node.wrapNode [])
let fooEqual1 = variableize "foo" (wrapInt 1)

let namePragmaFoo =
    AstNode.ParsePragma
        (Node.wrapNode
            { Name = "name"
              Values = [ AstNode.String(Node.wrapNode "foo") ] })


let createGenePart name =
    AstNode.Gene(Node.wrapNode { Gene = name; Linker = None })
// fixtures and helper functions for parts
let fooGene = createGenePart "gFOO"

let createPart mods prags basePart =
    AstNode.Part
        (Node.wrapNode
            { BasePart = basePart
              Modifiers = mods
              Pragmas = prags
              IsForward = true })

let basePartWrap = createPart [] []

let fooGenePart = basePartWrap fooGene

let relPosLeft =
    AstNode.ParseRelPos
        (Node.wrapNode
            { Item = AstNode.Int(Node.wrapNode 20)
              Qualifier = None
              Position = Left })

let relPosRight =
    AstNode.ParseRelPos
        (Node.wrapNode
            { Item = AstNode.Int(Node.wrapNode 200)
              Qualifier = None
              Position = Right })

let testSlice =
    AstNode.Slice
        (Node.wrapNode
            { Left = relPosLeft
              Right = relPosRight
              LeftApprox = true
              RightApprox = true })

let fooGeneWithSlice = createPart [ testSlice ] [] fooGene

let fooGeneWithPragma = createPart [] [ namePragmaFoo ] fooGene

let partVariable name =
    basePartWrap (AstNode.TypedVariable(Node.wrapNode (name, PartType)))

let typedValue t v = AstNode.TypedValue(Node.wrapNode (t, v))
