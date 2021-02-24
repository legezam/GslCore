namespace GslCore.Ast.Process.PragmaBuilding

open GslCore.Ast.Types
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

type PragmaBuildingError =
    | UnresolvedVariableInPragma of node: AstNode * variableName: string
    | IllegalPragmaArgumentType of argumentNode: AstNode
    | UndeclaredCapability of capability: string * capabilities: Capabilities * node: AstNode
    | PragmaIsUsedInWrongScope of pragmaName: string * allowedScope: string * usedInScope: string * node: AstNode
    | EmptyPragmaScope of node: AstNode
    | PragmaCreationError of message: string * node: AstNode
    | PragmaDeprecated of depreciation: PragmaDeprecation * node: AstNode

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

    let private checkPragmaArg: AstNode -> GslResult<string, PragmaBuildingError> =
        function
        | String stringWrapper -> GslResult.ok stringWrapper.Value
        | Int intWrapper -> GslResult.ok (intWrapper.Value.ToString())
        | Float floatWrapper -> GslResult.ok (floatWrapper.Value.ToString())
        | TypedVariable ({ Value = (name, _); Positions = _ }) as astNode ->
            GslResult.err (UnresolvedVariableInPragma(astNode, name))
        | x -> GslResult.err (EmptyPragmaScope x)

    let private checkDeprecated (node: AstNode) (pragma: Pragma): GslResult<Pragma, PragmaBuildingError> =
        match PragmaDeprecation.deprecatedPragmas
              |> Map.tryFind pragma.Name with
        | Some depreciation -> // deprecated pragma, issue a warning and replace it
            let warning = PragmaDeprecated(depreciation, node)

            let replacedPragma = depreciation.Replace pragma
            GslResult.warn warning replacedPragma
        | None -> GslResult.ok pragma

    // check if this pragma is a capability declaration.
    // if so, validate it.
    let private checkCapa (legalCapas: Capabilities)
                          (node: AstNode)
                          (pragma: Pragma)
                          : GslResult<Pragma, PragmaBuildingError> =
        if pragma |> Pragma.isCapa then
            let isNotLegalCapa =
                not (legalCapas.Contains(pragma.Arguments.[0]))

            if isNotLegalCapa
            then GslResult.err (UndeclaredCapability(pragma.Name, legalCapas, node))
            else GslResult.ok pragma
        else
            GslResult.ok pragma

    let private checkScope (contexts: PragmaConstructionContext list)
                           (node: AstNode)
                           (pragma: Pragma)
                           : GslResult<Pragma, PragmaBuildingError> =
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
                GslResult.err (PragmaIsUsedInWrongScope(pragma.Name, allowedScope, usedIn, node))
            | None -> GslResult.ok pragma
        | [] -> GslResult.err (EmptyPragmaScope node)


    /// Attempt to build a real pragma from a parsed pragma.
    let private compilePragma (parameters: Phase1Parameters)
                              (contexts: PragmaConstructionContext list)
                              (node: AstNode)
                              : GslResult<AstNode, PragmaBuildingError> =

        match node with
        | ParsePragma pragmaWrapper ->
            let pragma = pragmaWrapper.Value
            /// Building pragmas returns strings at the moment.
            /// Wrap them in an AST message.
            // TODO: fix this sad state of affairs once the big changes have landed in default.
            let wrapPragmaErrorString (message: string): PragmaBuildingError = PragmaCreationError(message, node)

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
    let buildPragmas (parameters: Phase1Parameters): AstTreeHead -> TreeTransformResult<PragmaBuildingError> =
        let foldMapParameters =
            { FoldMapParameters.Direction = TopDown
              Mode = Serial
              StateUpdate = updatePragmaConstructionContext
              Map = compilePragma parameters }

        FoldMap.foldMap [] foldMapParameters
