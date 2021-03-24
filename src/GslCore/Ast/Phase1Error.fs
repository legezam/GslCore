namespace GslCore.Ast.Phase1

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

module private VariableResolutionError =
    open GslCore.Ast.Process.VariableResolution

    let toAstMessage: VariableResolutionError -> AstMessage =
        function
        | VariableResolutionError.TypeCheck (typeCheckResult, node, variableName) ->
            match typeCheckResult with
            | TypeCheckError.CannotElide (boundValue, targetType) ->
                AstResult.internalTypeMismatchMsg (Some("variable type checking")) (targetType.ToString()) boundValue
            | TypeCheckError.ElisionConflict (elidedType, targetType) ->
                AstResult.variableTypeMismatchMsg variableName elidedType targetType node

        | VariableResolutionError.IllegalFunctionLocal (variableName, node) ->
            AstResult.errStringFMsg
                (InternalError(AstMessageType.UnresolvedVariable))
                "A variable resolved to a function local during strict variable resolution: %s"
                variableName
                node
        | VariableResolutionError.UnresolvedVariable (variableName, node) ->
            AstResult.errStringMsg AstMessageType.UnresolvedVariable variableName node

module private FunctionInliningError =
    open GslCore.Ast.Process.Inlining

    let toAstMessage: InliningError -> AstMessage =
        function
        | InliningError.VariableResolution err -> err |> VariableResolutionError.toAstMessage
        | InliningError.Validation (functionCallNode, innerError) ->
            match innerError with
            | FunctionValidationError.ParameterNumberMismatch (functionCall, neededArgs, passedArgs) ->
                AstResult.errStringMsg
                    TypeError
                    (sprintf
                        "Function '%s' expects %d arguments but received %d."
                         functionCall.Name
                         neededArgs
                         passedArgs)
                    functionCallNode
        | InliningError.MissingFunctionLocals parseFunction ->
            AstResult.errStringMsg
                (InternalError(TypeError))
                "No function locals node found in function definition block."
                parseFunction.Body
        | InliningError.FunctionCallTypeMismatch node ->
            AstResult.internalTypeMismatchMsg (Some "function call") "typed value" node
        | InliningError.FunctionBodyTypeMismatch node ->
            AstResult.internalTypeMismatchMsg (Some "function body") "Block" node
        | InliningError.UnresolvedFunction (functionCall, node) ->
            AstResult.errStringMsg AstMessageType.UnresolvedFunction functionCall.Name node
        | InliningError.MissingFunctionBody (parseFunction, node) ->
            AstResult.errStringMsg
                AstMessageType.GeneralError
                (sprintf "function '%s' has missing body" parseFunction.Name)
                node

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
        | UnsupportedStringOperation (operator, node) ->
            AstResult.errStringMsg TypeError (sprintf "String doesn't support operator %A" operator) node

module private PragmaBuildingError =
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

module private AssemblyFlatteningError =
    open GslCore.Ast.Types
    open GslCore.Ast.Process.AssemblyFlattening

    let toAstMessage: AssemblyFlatteningError -> AstMessage =
        function
        | FlippingTrailingFuse part ->
            AstResult.errStringMsg
                PragmaError
                "Found a trailing #fuse in an assembly that needs to flip."
                (AstNode.Part part)
        | PragmaMergeError msg -> msg

module internal AssemblyStuffingError =
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

module private PragmaWarningError =
    open GslCore.Ast.Process.PragmaWarning

    let toAstMessage: PragmaWarningError -> AstMessage =
        function
        | PragmaWarningError (pragma, node) ->
            let msg = pragma.Arguments |> String.concat " "

            AstMessage.createWarning msg node

module private RoughageExpansionError =
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

module private ParseErrorType =
    open GslCore.Ast.Process.Validation

    let toAstMessage: ParseErrorType -> AstMessage =
        function
        | ParseError (message, node) -> AstMessage.createErrorWithStackTrace ParserError message node

module private PartBaseValidationError =
    open GslCore.Ast.Process.Validation

    let toAstMessage: PartBaseValidationError -> AstMessage =
        function
        | NotValidBasePart node ->
            AstResult.errStringFMsg (InternalError(PartError)) "%s is not a valid base part." node.TypeName node

module private PartModifierValidationError =
    open GslCore.Ast.Process.Validation

    let toAstMessage: PartModifierValidationError -> AstMessage =
        function
        | NotAValidModifierTarget node ->
            AstResult.errStringFMsg PartError "Can only apply part mods to Gene or PartId, not %s" node.TypeName node

module private RecursiveCallCheckError =
    open GslCore.Ast.Process.Validation

    let toAstMessage: RecursiveCallCheckError -> AstMessage =
        function
        | RecursiveCallFoundError (functionCall, node) ->
            AstResult.errStringFMsg
                RecursiveFunctionCall
                "Found a recursive call to '%s'. GSL does not support recursive functions."
                functionCall.Name
                node

module private LinterHint =
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

open GslCore.Ast.Process.RelativePositionTranslation
open GslCore.Ast.Process.VariableResolution
open GslCore.Ast.Process.Inlining
open GslCore.Ast.Process.ExpressionReduction
open GslCore.Ast.Process.PragmaBuilding
open GslCore.Ast.Process.AssemblyFlattening
open GslCore.Ast.Process.AssemblyStuffing
open GslCore.Ast.Process.PragmaWarning
open GslCore.Ast.Process.RoughageExpansion
open GslCore.Ast.Process.Validation
open GslCore.Ast.Linting

[<RequireQualifiedAccess>]
type Phase1Error =
    | RelativePositionTranslationMessage of RelativePositionTranslationMessage
    | VariableResolutionError of VariableResolutionError
    | FunctionInliningError of InliningError
    | ExpressionReductionError of ExpressionReductionError
    | PragmaBuildingError of PragmaBuildingError
    | AssemblyFlatteningError of AssemblyFlatteningError
    | AssemblyStuffingError of AssemblyStuffingError
    | PragmaWarningError of PragmaWarningError
    | RoughageExpansionError of RoughageExpansionError
    | ParseErrorType of ParseErrorType
    | PartBaseValidationError of PartBaseValidationError
    | PartModifierValidationError of PartModifierValidationError
    | RecursiveCallCheckError of RecursiveCallCheckError
    | LinterHint of LinterHint
    | NoError of unit

module Phase1Error =
    let toAstMessage: Phase1Error -> AstMessage =
        function
        | Phase1Error.RelativePositionTranslationMessage msg -> RelativePositionTranslationMessage.toAstMessage msg
        | Phase1Error.VariableResolutionError msg -> VariableResolutionError.toAstMessage msg
        | Phase1Error.ExpressionReductionError msg -> ExpressionReductionMessage.toAstMessage msg
        | Phase1Error.PragmaBuildingError msg -> PragmaBuildingError.toAstMessage msg
        | Phase1Error.AssemblyFlatteningError msg -> AssemblyFlatteningError.toAstMessage msg
        | Phase1Error.AssemblyStuffingError msg -> AssemblyStuffingError.toAstMessage msg
        | Phase1Error.PragmaWarningError msg -> PragmaWarningError.toAstMessage msg
        | Phase1Error.RoughageExpansionError msg -> RoughageExpansionError.toAstMessage msg
        | Phase1Error.ParseErrorType msg -> ParseErrorType.toAstMessage msg
        | Phase1Error.PartBaseValidationError msg -> PartBaseValidationError.toAstMessage msg
        | Phase1Error.PartModifierValidationError msg -> PartModifierValidationError.toAstMessage msg
        | Phase1Error.RecursiveCallCheckError msg -> RecursiveCallCheckError.toAstMessage msg
        | Phase1Error.LinterHint msg -> LinterHint.toAstMessage msg
        | Phase1Error.FunctionInliningError msg -> FunctionInliningError.toAstMessage msg
        | Phase1Error.NoError _ -> failwith "IMPOSSIBLE"
