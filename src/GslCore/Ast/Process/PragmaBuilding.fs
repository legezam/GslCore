namespace GslCore.Ast.Process

open Amyris.ErrorHandling
open GslCore.Ast.Types
open GslCore.Ast.ErrorHandling
open GslCore.Ast.Algorithms
open GslCore.Pragma

// =====================
// compiling parsed pragmas into built pragmas
// =====================

// keep track of the enclosing context to catch pragmas that are out of place.
type PragmaConstructionContext =
    | BlockLevel
    | PartLevel

module PragmaBuilding =

    let private updatePragmaConstructionContext (mode: StateUpdateMode)
                                                (contexts: PragmaConstructionContext list)
                                                (node: AstNode)
                                                : PragmaConstructionContext list =
        let maybeContext =
            match node with
            | Block _ -> Some BlockLevel
            | Part _ -> Some PartLevel
            | _ -> None

        match maybeContext with
        | Some context ->
            match mode with
            | PreTransform -> context :: contexts
            | PostTransform ->
                match contexts with
                | [] -> []
                | _ :: tail -> tail
        | None -> contexts

    let private checkPragmaArg: AstNode -> Result<string, AstMessage> =
        function
        | String stringWrapper -> ok stringWrapper.Value
        | Int intWrapper -> ok (intWrapper.Value.ToString())
        | Float floatWrapper -> ok (floatWrapper.Value.ToString())
        | TypedVariable ({ Value = (name, _); Positions = _ }) as astNode ->
            AstMessage.createErrorf
                (InternalError(UnresolvedVariable))
                "Unresolved variable in pragma: '%s'"
                name
                astNode
        | x -> AstMessage.internalTypeMismatch (Some "pragma value") "String, Int, or Float" x

    let private checkDeprecated (node: AstNode) (pragma: Pragma): Result<Pragma, AstMessage> =
        match PragmaDeprecation.deprecatedPragmas
              |> Map.tryFind pragma.Name with
        | Some depreciation -> // deprecated pragma, issue a warning and replace it
            let warningMsg =
                AstMessage.create None DeprecationWarning depreciation.WarningMessage node

            let replacedPragma = depreciation.Replace pragma
            warn warningMsg replacedPragma
        | None -> ok pragma

    // check if this pragma is a capability declaration.
    // if so, validate it.
    let private checkCapa (legalCapas: Capabilities) (node: AstNode) (pragma: Pragma): Result<Pragma, AstMessage> =
        if pragma |> Pragma.isCapa then
            let isNotLegalCapa =
                not (legalCapas.Contains(pragma.Arguments.[0]))

            if isNotLegalCapa then
                let goodCapas =
                    legalCapas
                    |> Set.toList
                    |> List.sort
                    |> String.concat ", "

                let msg =
                    sprintf "Undeclared capability: %s.  Declared capabilities are %s" pragma.Name goodCapas

                AstMessage.createError PragmaError msg node
            else
                ok pragma
        else
            ok pragma

    let private checkScope (contexts: PragmaConstructionContext list)
                           (node: AstNode)
                           (pragma: Pragma)
                           : Result<Pragma, AstMessage> =
        match contexts with
        | headContext :: _ ->
            let errCond =
                match pragma.Definition.Scope, headContext with
                | BlockOnly _, BlockLevel
                | PartOnly, PartLevel
                | BlockOrPart _, _ -> None
                | BlockOnly _, PartLevel -> Some("block-level", "part-level")
                | PartOnly, BlockLevel -> Some("part-level", "block-level")

            match errCond with
            | Some (allowedScope, usedIn) ->
                let msg =
                    sprintf "#%s is used at %s, but is restricted to %s." pragma.Name usedIn allowedScope

                AstMessage.createError PragmaError msg node
            | None -> ok pragma
        | [] -> AstMessage.createError (InternalError(PragmaError)) "Pragma scope context is empty." node


    /// Attempt to build a real pragma from a parsed pragma.
    let private compilePragma (parameters: Phase1Parameters)
                              (contexts: PragmaConstructionContext list)
                              (node: AstNode)
                              : Result<AstNode, AstMessage> =

        match node with
        | ParsePragma pragmaWrapper ->
            let pragma = pragmaWrapper.Value
            /// Building pragmas returns strings at the moment.
            /// Wrap them in an AST message.
            // TODO: fix this sad state of affairs once the big changes have landed in default.
            let wrapPragmaErrorString s =
                AstMessage.createErrorWithStackTrace PragmaError s node

            pragma.Values
            |> List.map checkPragmaArg
            |> collect
            >>= (fun values ->
                parameters.PragmaBuilder
                |> PragmaBuilder.createPragmaFromNameValue pragma.Name values
                |> (mapMessages wrapPragmaErrorString))
            >>= (checkDeprecated node)
            >>= (checkScope contexts node)
            >>= (checkCapa parameters.LegalCapabilities node)
            >>= (fun builtPragma ->
                ok
                    (Pragma
                        ({ Value = builtPragma
                           Positions = pragmaWrapper.Positions })))
        | _ -> ok node

    /// Build genuine pragmas from reduced parsed pragmas.
    let buildPragmas (parameters: Phase1Parameters): AstTreeHead -> TreeTransformResult =
        let foldMapParameters =
            { FoldMapParameters.Direction = TopDown
              Mode = Serial
              StateUpdate = updatePragmaConstructionContext
              Map = compilePragma parameters }

        FoldMap.foldMap [] foldMapParameters
