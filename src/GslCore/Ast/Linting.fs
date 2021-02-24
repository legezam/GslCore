/// Define AST validations that provide warnings and errors about the structure and content of code.
/// Useful for easing language transitions, these should be updated and removed as the language and
/// compiler abilities change.
namespace GslCore.Ast.Linting

open GslCore.Ast.Types
open GslCore.Ast.Algorithms

open System.Text.RegularExpressions
open GslCore.GslResult

type LinterHint =
    | VariableReferenceDeprecated of variableName: string * node: AstNode
    | PushPopDeprecated of node: AstNode

module Linter =

    let private rabitPartRegex =
        Regex
            ("R\d+",
             RegexOptions.Compiled
             ||| RegexOptions.CultureInvariant)


    let private warnOnPartThatIsLikelyVariable (node: AstNode): GslResult<unit, LinterHint> =
        match node with
        | PartId partWrapper ->
            if rabitPartRegex.IsMatch(partWrapper.Value) then
                GslResult.ok ()
            else
                let warnMsg =
                    VariableReferenceDeprecated(partWrapper.Value, node)

                GslResult.warn warnMsg ()
        | _ -> GslResult.ok ()

    let private failOnPushAndPop (node: AstNode): GslResult<unit, LinterHint> =
        match node with
        | ParsePragma (parsePragmaWrapper) ->
            if parsePragmaWrapper.Value.Name = "push"
               || parsePragmaWrapper.Value.Name = "pop" then
                GslResult.err (PushPopDeprecated node)
            else
                GslResult.ok ()
        | _ -> GslResult.ok ()


    let private allLinters: AstNode -> GslResult<unit, LinterHint> =
        warnOnPartThatIsLikelyVariable
        &&& failOnPushAndPop


    /// Perform all linting passes on an AST.
    let linters = Validation.validate allLinters
