/// Define AST validations that provide warnings and errors about the structure and content of code.
/// Useful for easing language transitions, these should be updated and removed as the language and
/// compiler abilities change.
module GslCore.Ast.Linting

open GslCore.Ast.Types
open GslCore.Ast.Algorithms
open GslCore.Ast.ErrorHandling
open Amyris.ErrorHandling
open System.Text.RegularExpressions

let private rabitPartRegex =
    Regex
        ("R\d+",
         RegexOptions.Compiled
         ||| RegexOptions.CultureInvariant)

let private warnOnPartThatIsLikelyVariable (node: AstNode): ValidationResult =
    match node with
    | PartId partWrapper ->
        if rabitPartRegex.IsMatch(partWrapper.Value) then
            Validation.good
        else
            let msgText =
                sprintf "The syntax for using a variable has changed to &myVar from @myVar.\n@%s looks like it should probably be &%s."
                    partWrapper.Value partWrapper.Value

            let warnMsg = AstMessage.createWarning msgText node
            warn warnMsg ()
    | _ -> Validation.good

let private failOnPushAndPop (node: AstNode): Result<unit, AstMessage> =
    match node with
    | ParsePragma (parsePragmaWrapper) ->
        if parsePragmaWrapper.Value.Name = "push"
           || parsePragmaWrapper.Value.Name = "pop" then
            AstMessage.createError
                PragmaError
                "#push and #pop have been removed from GSL.  Please port your code to use do/end blocks."
                node
        else
            Validation.good
    | _ -> Validation.good


let private allLinters =
    warnOnPartThatIsLikelyVariable
    &&& failOnPushAndPop

/// Perform all linting passes on an AST.
let linters = Validation.validate allLinters
