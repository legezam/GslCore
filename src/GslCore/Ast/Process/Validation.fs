namespace GslCore.Ast.Process.Validation

open GslCore.Ast.Types
open GslCore.Ast.Algorithms

open GslCore.GslResult

type ParseErrorType = ParseError of message: string * node: AstNode

// ====================
// validation routines
// ====================
module ParseErrorValidation =
    /// Return an error if this node is a parse error.
    let checkParseError (node: AstNode): GslResult<unit, ParseErrorType> =
        match node with
        | AstNode.ParseError errorWrapper -> GslResult.err (ParseError(errorWrapper.Value, node))
        | _ -> GslResult.ok ()

// ===============
// validation of parts
// ===============
type PartBaseValidationError = NotValidBasePart of node: AstNode

type PartModifierValidationError = NotAValidModifierTarget of node: AstNode

module PartValidation =
    let validatePart (op: ParsePart -> GslResult<unit, 'a>) (node: AstNode): GslResult<unit, 'a> =
        match node with
        | Part ({ Value = pp; Positions = _ }) -> op pp
        | _ -> GslResult.ok ()

    // TODO: this may be either a step too far, or just on example of something we need a lot more of
    // Ideally the parser structure should make this kind of check unnecessary.
    let private validBasePartPP (pp: ParsePart): GslResult<unit, PartBaseValidationError> =
        match pp.BasePart with
        | ValidBasePart _ -> GslResult.ok ()
        | x -> GslResult.err (NotValidBasePart(x))

    let validBasePart = validatePart validBasePartPP

    // validtion functions on ParseParts
    let private checkModsPP (parsePart: ParsePart): GslResult<unit, PartModifierValidationError> =
        if not parsePart.Modifiers.IsEmpty then
            match parsePart.BasePart with
            | Gene _ -> GslResult.ok ()
            | PartId _ -> GslResult.ok ()
            | x -> GslResult.err (NotAValidModifierTarget x)
        else
            GslResult.ok ()

    let validateModifiers = validatePart checkModsPP

// ===================
// refusing to compile recursive function calls
// ===================

type RecursiveCallCheckError = RecursiveCallFoundError of functionCall: FunctionCall * node: AstNode

module RecursiveCalls =
    /// Maintain a stack of the function defintion context.
    let private updateRecursiveCheckState mode (s: string list) node =
        match node with
        | FunctionDef (fd) ->
            match mode with
            | PreTransform -> fd.Value.Name :: s
            | PostTransform ->
                match s with
                | [] -> []
                | _ :: tl -> tl
        | _ -> s


    /// If we find a function call to a function def we're already inside, fail.
    let private checkRecursiveCall (functionStack: string list)
                                   (node: AstNode)
                                   : GslResult<AstNode, RecursiveCallCheckError> =
        match node with
        | FunctionCall functionCall when functionStack
                                         |> List.contains functionCall.Value.Name ->
            GslResult.err (RecursiveCallFoundError(functionCall.Value, node))
        | _ -> GslResult.ok node

    /// Fail if a GSL program contains recursively-defined functions.
    let check =
        let foldMapParameters =
            { FoldMapParameters.Direction = TopDown
              Mode = Serial
              StateUpdate = updateRecursiveCheckState
              Map = checkRecursiveCall }

        FoldMap.foldMap [] foldMapParameters
