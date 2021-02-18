/// Define AST validations that provide warnings and errors about the structure and content of code.
/// Useful for easing language transitions, these should be updated and removed as the language and
/// compiler abilities change.
module GslCore.AstLinting

open GslCore.AstTypes
open GslCore.AstAlgorithms
open GslCore.AstErrorHandling
open Amyris.ErrorHandling
open System.Text.RegularExpressions

let private rabitPartRegex = Regex("R\d+", RegexOptions.Compiled ||| RegexOptions.CultureInvariant)

let private warnOnPartThatIsLikelyVariable node =
    match node with
    | PartId (pw) ->
        if rabitPartRegex.IsMatch(pw.Value) then
            Validation.good
        else
            let msgText =
                sprintf "The syntax for using a variable has changed to &myVar from @myVar.\n@%s looks like it should probably be &%s."
                    pw.Value pw.Value

            let warnMsg = AstMessage.createWarning msgText node
            warn warnMsg ()
    | _ -> Validation.good

let private failOnPushAndPop node =
    match node with
    | ParsePragma (pp) ->
        if pp.Value.Name = "push" || pp.Value.Name = "pop"
        then AstMessage.createError
                 PragmaError
                 "#push and #pop have been removed from GSL.  Please port your code to use do/end blocks."
                 node
        else Validation.good
    | _ -> Validation.good


let private allLinters =
    warnOnPartThatIsLikelyVariable
    &&& failOnPushAndPop

/// Perform all linting passes on an AST.
let linters = Validation.validate allLinters
