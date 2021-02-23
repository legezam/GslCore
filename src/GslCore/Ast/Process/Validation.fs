module GslCore.Ast.Process.Validation

open GslCore.Ast.Types
open GslCore.Ast.ErrorHandling
open GslCore.Ast.Algorithms

open GslCore.GslResult
open GslCore.Pragma

// ====================
// validation routines
// ====================

/// Return an error if this node is a parse error.
let checkParseError node =
    match node with
    | ParseError errorWrapper ->
        let msg =
            AstMessage.createErrorWithStackTrace ParserError errorWrapper.Value node

        GslResult.err msg
    | _ -> Validation.good

// ===============
// validation of parts
// ===============

let validatePart op node =
    match node with
    | Part ({ Value = pp; Positions = _ }) -> op pp
    | _ -> Validation.good

// FIXME: this may be either a step too far, or just on example of something we need a lot more of
// Ideally the parser structure should make this kind of check unnecessary.
let private validBasePartPP (pp: ParsePart): ValidationResult =
    match pp.BasePart with
    | ValidBasePart _ -> Validation.good
    | x ->
        AstResult.errStringF (InternalError(PartError)) "%s is not a valid base part." x.TypeName x

let validBasePart = validatePart validBasePartPP

// validtion functions on ParseParts
let private checkModsPP pp =
    if not pp.Modifiers.IsEmpty then
        match pp.BasePart with
        | Gene _ -> Validation.good
        | PartId _ -> Validation.good
        | x -> AstResult.errStringF PartError "Can only apply part mods to Gene or PartId, not %s" x.TypeName x
    else
        Validation.good

let checkMods = validatePart checkModsPP

// ===================
// refusing to compile recursive function calls
// ===================

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
let private checkRecursiveCall (s: string list) node =
    match node with
    | FunctionCall (fc) when s |> List.contains fc.Value.Name ->
        AstResult.errStringF
            RecursiveFunctionCall
            "Found a recursive call to '%s'. GSL does not support recursive functions."
            fc.Value.Name
            node
    | _ -> GslResult.ok node

/// Fail if a GSL program contains recursively-defined functions.
let checkRecursiveCalls =
    let foldMapParameters =
        { FoldMapParameters.Direction = TopDown
          Mode = Serial
          StateUpdate = updateRecursiveCheckState
          Map = checkRecursiveCall }

    FoldMap.foldMap [] foldMapParameters
