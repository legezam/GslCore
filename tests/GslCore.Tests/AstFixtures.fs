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
    | Ok { Result = AstTreeHead (Block (b))
           Warnings = _ } -> b.Value
    | x -> failwithf "Illegal: %A" x


let wrapInt v = Int(Node.wrapNode v)
let wrapFloat v = Float(Node.wrapNode v)
let wrapString v = String(Node.wrapNode v)

/// Wrap AST items as a block.
let blockify items = Block(Node.wrapNode items)

/// Group AST items as a block, wrapped as a whole tree.
let treeify items = AstTreeHead(blockify items)

/// Make a bare-bones assembly part out of some parts.
let assemble = Parts.createAssemblyPart

let addPragsToPart prags a =
    match a with
    | Part (pw) ->
        Part
            ({ pw with
                   Value = { pw.Value with Pragmas = prags } })
    | x -> failwithf "Illegal: %A" x

let variableize name v =
    let t =
        match v with
        | BinaryOperation _ -> IntType // right now we only support integer math so this is OK
        | Int _ -> IntType
        | Float _ -> FloatType
        | String _ -> StringType
        | Part _ -> PartType
        | x -> failwithf "Illegal: %A" x

    VariableBinding(Node.wrapNode { Name = name; Type = t; Value = v })

let functionalize name args bodyLines =
    let locals =
        FunctionLocals(Node.wrapNode { Names = args })

    FunctionDef
        (Node.wrapNode
            { Name = name
              ArgumentNames = args
              Body = blockify (locals :: bodyLines) })

let emptyBlock = Block(Node.wrapNode [])
let fooEqual1 = variableize "foo" (wrapInt 1)

let namePragmaFoo =
    ParsePragma
        (Node.wrapNode
            { Name = "name"
              Values = [ String(Node.wrapNode "foo") ] })


let createGenePart name =
    Gene(Node.wrapNode { Gene = name; Linker = None })
// fixtures and helper functions for parts
let fooGene = createGenePart "gFOO"

let createPart mods prags basePart =
    Part
        (Node.wrapNode
            { BasePart = basePart
              Modifiers = mods
              Pragmas = prags
              IsForward = true })

let basePartWrap = createPart [] []

let fooGenePart = basePartWrap fooGene

let relPosLeft =
    ParseRelPos
        (Node.wrapNode
            { Item = Int(Node.wrapNode 20)
              Qualifier = None
              Position = Left })

let relPosRight =
    ParseRelPos
        (Node.wrapNode
            { Item = Int(Node.wrapNode 200)
              Qualifier = None
              Position = Right })

let testSlice =
    Slice
        (Node.wrapNode
            { Left = relPosLeft
              Right = relPosRight
              LeftApprox = true
              RightApprox = true })

let fooGeneWithSlice = createPart [ testSlice ] [] fooGene

let fooGeneWithPragma = createPart [] [ namePragmaFoo ] fooGene

let partVariable name =
    basePartWrap (TypedVariable(Node.wrapNode (name, PartType)))

let typedValue t v = TypedValue(Node.wrapNode (t, v))
