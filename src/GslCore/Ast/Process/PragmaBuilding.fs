namespace GslCore.Ast.Process

open GslCore.Ast.Types
open GslCore.Ast.ErrorHandling
open GslCore.Ast.Algorithms
open GslCore.GslResult
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

    let private checkPragmaArg: AstNode -> AstResult<string> =
        function
        | String stringWrapper -> GslResult.ok stringWrapper.Value
        | Int intWrapper -> GslResult.ok (intWrapper.Value.ToString())
        | Float floatWrapper -> GslResult.ok (floatWrapper.Value.ToString())
        | TypedVariable ({ Value = (name, _); Positions = _ }) as astNode ->
            AstResult.errStringF (InternalError(UnresolvedVariable)) "Unresolved variable in pragma: '%s'" name astNode
        | x -> AstResult.internalTypeMismatch (Some "pragma value") "String, Int, or Float" x

    let private checkDeprecated (node: AstNode) (pragma: Pragma): AstResult<Pragma> =
        match PragmaDeprecation.deprecatedPragmas
              |> Map.tryFind pragma.Name with
        | Some depreciation -> // deprecated pragma, issue a warning and replace it
            let warningMsg =
                AstMessage.create None DeprecationWarning depreciation.WarningMessage node

            let replacedPragma = depreciation.Replace pragma
            GslResult.warn warningMsg replacedPragma
        | None -> GslResult.ok pragma

    // check if this pragma is a capability declaration.
    // if so, validate it.
    let private checkCapa (legalCapas: Capabilities) (node: AstNode) (pragma: Pragma): AstResult<Pragma> =
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

                AstResult.errString PragmaError msg node
            else
                GslResult.ok pragma
        else
            GslResult.ok pragma

    let private checkScope (contexts: PragmaConstructionContext list) (node: AstNode) (pragma: Pragma): AstResult<Pragma> =
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

                AstResult.errString PragmaError msg node
            | None -> GslResult.ok pragma
        | [] -> AstResult.errString (InternalError(PragmaError)) "Pragma scope context is empty." node


    /// Attempt to build a real pragma from a parsed pragma.
    let private compilePragma (parameters: Phase1Parameters)
                              (contexts: PragmaConstructionContext list)
                              (node: AstNode)
                              : AstResult<AstNode> =

        match node with
        | ParsePragma pragmaWrapper ->
            let pragma = pragmaWrapper.Value
            /// Building pragmas returns strings at the moment.
            /// Wrap them in an AST message.
            // TODO: fix this sad state of affairs once the big changes have landed in default.
            let wrapPragmaErrorString (message: string): AstMessage =
                AstMessage.createErrorWithStackTrace PragmaError message node

            pragma.Values
            |> List.map checkPragmaArg
            |> GslResult.collectA
            >>= (fun values ->
                parameters.PragmaBuilder
                |> PragmaBuilder.createPragmaFromNameValue pragma.Name values
                |> GslResult.mapError wrapPragmaErrorString)
            >>= (checkDeprecated node)
            >>= (checkScope contexts node)
            >>= (checkCapa parameters.LegalCapabilities node)
            |> GslResult.map (fun builtPragma ->
                Pragma
                    { Value = builtPragma
                      Positions = pragmaWrapper.Positions })
        | _ -> GslResult.ok node

    /// Build genuine pragmas from reduced parsed pragmas.
    let buildPragmas (parameters: Phase1Parameters): AstTreeHead -> TreeTransformResult =
        let foldMapParameters =
            { FoldMapParameters.Direction = TopDown
              Mode = Serial
              StateUpdate = updatePragmaConstructionContext
              Map = compilePragma parameters }

        FoldMap.foldMap [] foldMapParameters
