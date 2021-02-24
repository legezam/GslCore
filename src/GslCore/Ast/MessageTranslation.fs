namespace GslCore.Ast.MessageTranslation

open GslCore.Ast.ErrorHandling


module NoMessage =
    let toAstMessage (_input: 'a): AstMessage = failwith "Impossible"

module private RelativePositionTranslationMessage =
    open GslCore.Ast.Process.RelativePositionTranslation

    let toAstMessage: RelativePositionTranslationMessage -> AstMessage =
        function
        | CalculationError (node, calcError) ->
            match calcError with
            | NegativeLeftAminoAcidStartPosition position ->
                AstMessage.createErrorWithStackTrace
                    ValueError
                    (sprintf "Cannot begin with a negative amino acid offset: %d" position)
                    node

            | NegativeRightAminoAcidStartPosition position ->
                AstMessage.createErrorWithStackTrace
                    ValueError
                    (sprintf "Cannot offset negative amino acids from start: %d" position)
                    node
            | PositionCannotBeZero ->
                AstMessage.createErrorWithStackTrace ValueError (sprintf "Slice index cannot be zero") node

        | PositionIsNotInteger node -> AstResult.internalTypeMismatchMsg (Some "relative position building") "Int" node

module private VariableResolutionMessage =
    open GslCore.Ast.Process.VariableResolution

    let toAstMessage: VariableResolutionError -> AstMessage =
        function
        | TypeCheckError (typeCheckResult, node, variableName) ->
            match typeCheckResult with
            | ElisionFailure (boundValue, targetType) ->
                AstResult.internalTypeMismatchMsg (Some("variable type checking")) (targetType.ToString()) boundValue
            | ElisionResolvesToDifferentTypeError (elidedType, targetType) ->
                AstResult.variableTypeMismatchMsg variableName elidedType targetType node

        | IllegalFunctionLocal (variableName, node) ->
            AstResult.errStringFMsg
                (InternalError(AstMessageType.UnresolvedVariable))
                "A variable resolved to a function local during strict variable resolution: %s"
                variableName
                node
        | UnresolvedVariable (variableName, node) ->
            AstResult.errStringMsg AstMessageType.UnresolvedVariable variableName node

module private FunctionInliningMessage =
    open GslCore.Ast.Process.Inlining

    let toAstMessage: FunctionInliningError -> AstMessage =
        function
        | VariableResolution err -> err |> VariableResolutionMessage.toAstMessage
        | ParameterNumberMismatchError (functionCallNode, functionCall, neededArgs, passedArgs) ->
            AstResult.errStringMsg
                TypeError
                (sprintf "Function '%s' expects %d arguments but received %d." functionCall.Name neededArgs passedArgs)
                functionCallNode
        | MissingFunctionLocalsError parseFunction ->
            AstResult.errStringMsg
                (InternalError(TypeError))
                "No function locals node found in function definition block."
                parseFunction.Body
        | InternalFunctionCallTypeMismatch node ->
            AstResult.internalTypeMismatchMsg (Some "function call") "typed value" node
        | InternalFunctionBodyTypeMismatch node -> AstResult.internalTypeMismatchMsg (Some "function body") "Block" node
        | UnresolvedFunction (functionCall, node) ->
            AstResult.errStringMsg AstMessageType.UnresolvedFunction functionCall.Name node

module private ExpressionReductionMessage =
    open GslCore.Ast.Process.ExpressionReduction

    let toAstMessage: ExpressionReductionError -> AstMessage =
        function
        | ExpectedNumericVariable (node, foundType) ->
            AstResult.errStringMsg TypeError (sprintf "Expecting a numeric variable type, but found %O." foundType) node
        | TypeIsNotAllowedInBinaryExpression node ->
            AstResult.errStringMsg
                TypeError
                (sprintf "'%s' is not allowed to appear in a numeric binary operation." node.TypeName)
                node
        | TypeIsNotAllowedInNegationExpression node ->
            AstResult.errStringMsg TypeError (sprintf "'%s' is not allowed to appear in a negation." node.TypeName) node

module private PragmaBuildingMessage =
    open GslCore.Ast.Process.PragmaBuilding

    let toAstMessage: PragmaBuildingError -> AstMessage =
        function
        | UnresolvedVariableInPragma (node, variableName) ->
            AstResult.errStringFMsg
                (InternalError(UnresolvedVariable))
                "Unresolved variable in pragma: '%s'"
                variableName
                node
        | IllegalPragmaArgumentType argumentNode ->
            AstResult.internalTypeMismatchMsg (Some "pragma value") "String, Int, or Float" argumentNode
        | UndeclaredCapability (capability, legalCapas, node) ->
            let legalCapas =
                legalCapas
                |> Set.toList
                |> List.sort
                |> String.concat ", "

            let msg =
                sprintf "Undeclared capability: %s.  Declared capabilities are %s" capability legalCapas

            AstResult.errStringMsg PragmaError msg node

        | PragmaIsUsedInWrongScope (pragmaName, allowedScope, usedInScope, node) ->
            let msg =
                sprintf "#%s is used at %s, but is restricted to %s." pragmaName usedInScope allowedScope

            AstResult.errStringMsg PragmaError msg node
        | EmptyPragmaScope node ->
            AstResult.errStringMsg (InternalError(PragmaError)) "Pragma scope context is empty." node
        | PragmaCreationError (message, node) -> AstMessage.createErrorWithStackTrace PragmaError message node
        | PragmaDeprecated (depreciation, node) ->
            AstMessage.create None DeprecationWarning depreciation.WarningMessage node

module private AssemblyFlatteningMessage =
    open GslCore.Ast.Types
    open GslCore.Ast.Process.AssemblyFlattening

    let toAstMessage: AssemblyFlatteningError -> AstMessage =
        function
        | FlippingTrailingFuse part ->
            AstResult.errStringMsg PragmaError "Found a trailing #fuse in an assembly that needs to flip." (Part part)
        | PragmaMergeError msg -> msg

module internal AssemblyStuffingMessage =
    open GslCore.Ast.Process.AssemblyStuffing
    open GslCore.Pragma

    let toAstMessage: AssemblyStuffingError -> AstMessage =
        function
        | CollidingPragmas (incoming, existing, node) ->
            let formatPragma (pragma: Pragma) = pragma.Arguments |> String.concat " "

            let msg =
                sprintf
                    "The pragma #%s is set in this assembly as well as in the enclosing environment with conflicting values.  Incoming: '%s'.  Existing: '%s'."
                    existing.Name
                    (formatPragma incoming)
                    (formatPragma existing)

            AstResult.errStringMsg PragmaError msg node

module private PragmaWarningMessage =
    open GslCore.Ast.Process.PragmaWarning

    let toAstMessage: PragmaWarningError -> AstMessage =
        function
        | PragmaWarningError (pragma, node) ->
            let msg = pragma.Arguments |> String.concat " "

            AstMessage.createWarning msg node

module private RoughageExpansionMessage =
    open GslCore.Ast.Algorithms
    open GslCore.Ast.Process.RoughageExpansion

    let toAstMessage: RoughageExpansionError -> AstMessage =
        function
        | ConstructHasIndeterminateLocus node ->
            AstResult.errStringFMsg
                ValueError
                "Roughage construct has indeterminate locus: %s"
                (AstNode.decompile node)
                node

module private ParseErrorMessage =
    open GslCore.Ast.Process.Validation

    let toAstMessage: ParseErrorType -> AstMessage =
        function
        | ParseError (message, node) -> AstMessage.createErrorWithStackTrace ParserError message node

module private PartBaseValidationMessage =
    open GslCore.Ast.Process.Validation

    let toAstMessage: PartBaseValidationError -> AstMessage =
        function
        | NotValidBasePart node ->
            AstResult.errStringFMsg (InternalError(PartError)) "%s is not a valid base part." node.TypeName node

module private PartModifierValidationMessage =
    open GslCore.Ast.Process.Validation

    let toAstMessage: PartModifierValidationError -> AstMessage =
        function
        | NotAValidModifierTarget node ->
            AstResult.errStringFMsg PartError "Can only apply part mods to Gene or PartId, not %s" node.TypeName node

module private RecursiveCallCheckMessage =
    open GslCore.Ast.Process.Validation

    let toAstMessage: RecursiveCallCheckError -> AstMessage =
        function
        | RecursiveCallFoundError (functionCall, node) ->
            AstResult.errStringFMsg
                RecursiveFunctionCall
                "Found a recursive call to '%s'. GSL does not support recursive functions."
                functionCall.Name
                node

module private LinterHintMessage =
    open GslCore.Ast.Linting

    let toAstMessage: LinterHint -> AstMessage =
        function
        | VariableReferenceDeprecated (variableName, node) ->
            let msgText =
                sprintf "The syntax for using a variable has changed to &myVar from @myVar.\n@%s looks like it should probably be &%s."
                    variableName variableName

            AstMessage.createWarning msgText node
        | PushPopDeprecated node ->
            AstResult.errStringMsg
                PragmaError
                "#push and #pop have been removed from GSL.  Please port your code to use do/end blocks."
                node

type Phase1Message =
    | RelativePositionTranslationMessage of
        GslCore.Ast.Process.RelativePositionTranslation.RelativePositionTranslationMessage
    | VariableResolutionError of GslCore.Ast.Process.VariableResolution.VariableResolutionError
    | FunctionInliningError of GslCore.Ast.Process.Inlining.FunctionInliningError
    | ExpressionReductionError of GslCore.Ast.Process.ExpressionReduction.ExpressionReductionError
    | PragmaBuildingError of GslCore.Ast.Process.PragmaBuilding.PragmaBuildingError
    | AssemblyFlatteningError of GslCore.Ast.Process.AssemblyFlattening.AssemblyFlatteningError
    | AssemblyStuffingError of GslCore.Ast.Process.AssemblyStuffing.AssemblyStuffingError
    | PragmaWarningError of GslCore.Ast.Process.PragmaWarning.PragmaWarningError
    | RoughageExpansionError of GslCore.Ast.Process.RoughageExpansion.RoughageExpansionError
    | ParseErrorType of GslCore.Ast.Process.Validation.ParseErrorType
    | PartBaseValidationError of GslCore.Ast.Process.Validation.PartBaseValidationError
    | PartModifierValidationError of GslCore.Ast.Process.Validation.PartModifierValidationError
    | RecursiveCallCheckError of GslCore.Ast.Process.Validation.RecursiveCallCheckError
    | LinterHint of GslCore.Ast.Linting.LinterHint
    | NoError of unit

module Phase1Message =
    let toAstMessage: Phase1Message -> AstMessage =
        function
        | RelativePositionTranslationMessage msg -> RelativePositionTranslationMessage.toAstMessage msg
        | VariableResolutionError msg -> VariableResolutionMessage.toAstMessage msg
        | ExpressionReductionError msg -> ExpressionReductionMessage.toAstMessage msg
        | PragmaBuildingError msg -> PragmaBuildingMessage.toAstMessage msg
        | AssemblyFlatteningError msg -> AssemblyFlatteningMessage.toAstMessage msg
        | AssemblyStuffingError msg -> AssemblyStuffingMessage.toAstMessage msg
        | PragmaWarningError msg -> PragmaWarningMessage.toAstMessage msg
        | RoughageExpansionError msg -> RoughageExpansionMessage.toAstMessage msg
        | ParseErrorType msg -> ParseErrorMessage.toAstMessage msg
        | PartBaseValidationError msg -> PartBaseValidationMessage.toAstMessage msg
        | PartModifierValidationError msg -> PartModifierValidationMessage.toAstMessage msg
        | RecursiveCallCheckError msg -> RecursiveCallCheckMessage.toAstMessage msg
        | LinterHint msg -> LinterHintMessage.toAstMessage msg
        | FunctionInliningError msg -> FunctionInliningMessage.toAstMessage msg
        | NoError _ -> failwith "IMPOSSIBLE"
