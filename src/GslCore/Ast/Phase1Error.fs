namespace GslCore.Ast.Phase1

open GslCore.Ast.ErrorHandling
open GslCore.Ast.Types
open GslCore.Pragma


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
                    AstMessageType.ValueError
                    (sprintf "Cannot begin with a negative amino acid offset: %d" position)
                    node

            | NegativeRightAminoAcidStartPosition position ->
                AstMessage.createErrorWithStackTrace
                    AstMessageType.ValueError
                    (sprintf "Cannot offset negative amino acids from start: %d" position)
                    node
            | PositionCannotBeZero ->
                AstMessage.createErrorWithStackTrace
                    AstMessageType.ValueError
                    (sprintf "Slice index cannot be zero")
                    node

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
                (AstMessageType.InternalError(AstMessageType.UnresolvedVariable))
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
                    AstMessageType.TypeError
                    (sprintf
                        "Function '%s' expects %d arguments but received %d."
                         functionCall.Name
                         neededArgs
                         passedArgs)
                    functionCallNode
        | InliningError.MissingFunctionLocals parseFunction ->
            AstResult.errStringMsg
                (AstMessageType.InternalError(AstMessageType.TypeError))
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
            AstResult.errStringMsg
                AstMessageType.TypeError
                (sprintf "Expecting a numeric variable type, but found %O." foundType)
                node
        | TypeIsNotAllowedInBinaryExpression node ->
            AstResult.errStringMsg
                AstMessageType.TypeError
                (sprintf "'%s' is not allowed to appear in a numeric binary operation." node.TypeName)
                node
        | TypeIsNotAllowedInNegationExpression node ->
            AstResult.errStringMsg
                AstMessageType.TypeError
                (sprintf "'%s' is not allowed to appear in a negation." node.TypeName)
                node
        | UnsupportedStringOperation (operator, node) ->
            AstResult.errStringMsg AstMessageType.TypeError (sprintf "String doesn't support operator %A" operator) node

module PragmaArgumentError =
    let toAstMessage (node: AstNode): PragmaArgumentError -> AstMessage =
        function
        | PragmaArgumentError.Validation message -> AstResult.errStringMsg AstMessageType.PragmaError message node
        | PragmaArgumentError.Shape argShapeError ->
            match argShapeError with
            | ArgumentShapeError.SetMismatch (name, vals, numOfArguments, arguments) ->
                let message =
                    sprintf
                        "Pragma %s expected any number of arguments in the set %A but got %d: %A"
                        name
                        vals
                        numOfArguments
                        arguments

                AstResult.errStringMsg AstMessageType.PragmaError message node
            | ArgumentShapeError.ArgumentNumberMismatch (name, expected, numOfArguments, arguments) ->
                let message =
                    sprintf "Pragma #%s expected %d argument(s) but got %d: %A" name expected numOfArguments arguments

                AstResult.errStringMsg AstMessageType.PragmaError message node
            | ArgumentShapeError.TooFewArguments (name, min, numOfArguments, arguments) ->
                let message =
                    sprintf
                        "Pragma #%s expected at least %d argument(s) but got %d: %A"
                        name
                        min
                        numOfArguments
                        arguments

                AstResult.errStringMsg AstMessageType.PragmaError message node
            | ArgumentShapeError.TooManyArguments (name, max, numOfArguments, arguments) ->
                let message =
                    sprintf
                        "Pragma #%s expected at most %d argument(s) but got %d: %A"
                        name
                        max
                        numOfArguments
                        arguments

                AstResult.errStringMsg AstMessageType.PragmaError message node

module private PragmaBuildingError =
    open GslCore.Ast.Process.PragmaBuilding

    let toAstMessage: PragmaBuildingError -> AstMessage =
        function
        | PragmaBuildingError.UnresolvedVariableInPragma (node, variableName) ->
            AstResult.errStringFMsg
                (AstMessageType.InternalError(AstMessageType.UnresolvedVariable))
                "Unresolved variable in pragma: '%s'"
                variableName
                node
        | PragmaBuildingError.IllegalPragmaArgumentType argumentNode ->
            AstResult.internalTypeMismatchMsg (Some "pragma value") "String, Int, or Float" argumentNode
        | PragmaBuildingError.UndeclaredCapability (capability, legalCapas, node) ->
            let legalCapas =
                legalCapas
                |> Set.toList
                |> List.sort
                |> String.concat ", "

            let msg =
                sprintf "Undeclared capability: %s.  Declared capabilities are %s" capability legalCapas

            AstResult.errStringMsg AstMessageType.PragmaError msg node

        | PragmaBuildingError.PragmaIsUsedInWrongScope (pragmaName, allowedScope, usedInScope, node) ->
            let msg =
                sprintf "#%s is used at %s, but is restricted to %s." pragmaName usedInScope allowedScope

            AstResult.errStringMsg AstMessageType.PragmaError msg node
        | PragmaBuildingError.EmptyPragmaScope node ->
            AstResult.errStringMsg
                (AstMessageType.InternalError(AstMessageType.PragmaError))
                "Pragma scope context is empty."
                node
        | PragmaBuildingError.PragmaBuilder (builderError, node) ->
            match builderError with
            | PragmaFactoryError.MissingDefinition name ->
                let message =
                    sprintf "Unknown or invalid pragma: '#%s'" name

                AstMessage.createErrorWithStackTrace AstMessageType.PragmaError message node
            | PragmaFactoryError.PragmaCreation pragmaArgumentError ->
                pragmaArgumentError
                |> PragmaArgumentError.toAstMessage node

        | PragmaBuildingError.PragmaDeprecated (depreciation, node) ->
            AstMessage.create None AstMessageType.DeprecationWarning depreciation.WarningMessage node

module GetPragmaError =
    open GslCore.Ast.Process.ParsePart

    let toAstMessage: GetPragmaError -> AstMessage =
        function
        | GetPragmaError.UnBuiltPragma (parsePragma, node) ->
            AstResult.unbuiltPragmaError None parsePragma.Value.Name node
        | GetPragmaError.UnknownPragmaNode node -> AstResult.internalTypeMismatchMsg None "Pragma" node

module MergePragmaError =
    open GslCore.Ast.Process.ParsePart

    let toAstMessage: MergePragmaError -> AstMessage =
        function
        | MergePragmaError.GetPragmaError innerError -> innerError |> GetPragmaError.toAstMessage
        | MergePragmaError.PragmaCollision (collidingPragmas, node) ->
            let msg =
                sprintf "Pragma collision(s): %s" (collidingPragmas |> String.concat ", ")

            AstResult.errStringMsg AstMessageType.PragmaError msg node

module PragmaInversionError =
    let toAstMessage (node: AstNode): PragmaInversionError -> AstMessage =
        function
        | PragmaInversionError.IncompatibleTarget (source, target) ->
            let message =
                sprintf "Pragma %s inverts to %s but they have differing argShapes." source.Name target.Name

            AstResult.errStringMsg AstMessageType.PragmaError message node
        | PragmaInversionError.UnknownTarget (source, targetName) ->
            let message =
                sprintf "Pragma %s inverts to an unknown pragma %s" source.Name targetName

            AstResult.errStringMsg AstMessageType.PragmaError message node

module InvertPragmaError =
    open GslCore.Ast.Process.AssemblyFlattening

    let toAstMessage (node: AstNode): InvertPragmaError -> AstMessage =
        function
        | InvertPragmaError.Inversion inner -> inner |> PragmaInversionError.toAstMessage node
        | InvertPragmaError.GetPragma inner -> inner |> GetPragmaError.toAstMessage

module ShiftFuseError =
    open GslCore.Ast.Process.AssemblyFlattening

    let toAstMessage (node: AstNode): ShiftFuseError -> AstMessage =
        function
        | ShiftFuseError.GetPragma inner -> inner |> GetPragmaError.toAstMessage
        | ShiftFuseError.PragmaArgument (inner) -> inner |> PragmaArgumentError.toAstMessage node

module AssemblyExplosionError =
    open GslCore.Ast.Process.AssemblyFlattening

    let toAstMessage (node: AstNode): AssemblyExplosionError -> AstMessage =
        function
        | AssemblyExplosionError.GetPragma inner -> inner |> GetPragmaError.toAstMessage
        | AssemblyExplosionError.InvertPragma inner -> inner |> InvertPragmaError.toAstMessage node
        | AssemblyExplosionError.MergePragma inner -> inner |> MergePragmaError.toAstMessage
        | AssemblyExplosionError.ShiftFuse inner -> inner |> ShiftFuseError.toAstMessage node
        | AssemblyExplosionError.FlippingTrailingFuse part ->
            AstResult.errStringMsg
                AstMessageType.PragmaError
                "Found a trailing #fuse in an assembly that needs to flip."
                (AstNode.Part part)

module RecursivePartCollapseError =
    open GslCore.Ast.Process.AssemblyFlattening

    let toAstMessage: RecursivePartCollapseError -> AstMessage =
        function
        | RecursivePartCollapseError.GetPragma inner -> inner |> GetPragmaError.toAstMessage
        | RecursivePartCollapseError.MergePragma inner -> inner |> MergePragmaError.toAstMessage

module private AssemblyFlatteningError =
    open GslCore.Ast.Process.AssemblyFlattening

    let toAstMessage: AssemblyFlatteningError -> AstMessage =
        function
        | AssemblyFlatteningError.Explosion (inner, node) -> inner |> AssemblyExplosionError.toAstMessage node
        | AssemblyFlatteningError.RecursivePartCollapse inner -> inner |> RecursivePartCollapseError.toAstMessage

module internal AssemblyStuffingError =
    open GslCore.Ast.Process.AssemblyStuffing

    let toAstMessage: AssemblyStuffingError -> AstMessage =
        function
        | AssemblyStuffingError.PragmaMerge (node, inner) ->
            match inner with
            | PragmaMergeError.PragmaConflict (incoming, existing) ->
                let formatPragma (pragma: Pragma) = pragma.Arguments |> String.concat " "

                let msg =
                    sprintf
                        "The pragma #%s is set in this assembly as well as in the enclosing environment with conflicting values.  Incoming: '%s'.  Existing: '%s'."
                        existing.Name
                        (formatPragma incoming)
                        (formatPragma existing)

                AstResult.errStringMsg AstMessageType.PragmaError msg node
        | AssemblyStuffingError.PragmaArgument (inner, node) -> inner |> PragmaArgumentError.toAstMessage node
        | AssemblyStuffingError.GetPragma inner -> inner |> GetPragmaError.toAstMessage

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
                AstMessageType.ValueError
                "Roughage construct has indeterminate locus: %s"
                (AstNode.decompile node)
                node

module private ParseErrorType =
    open GslCore.Ast.Process.Validation

    let toAstMessage: ParseErrorType -> AstMessage =
        function
        | ParseError (message, node) -> AstMessage.createErrorWithStackTrace AstMessageType.ParserError message node

module private PartBaseValidationError =
    open GslCore.Ast.Process.Validation

    let toAstMessage: PartBaseValidationError -> AstMessage =
        function
        | NotValidBasePart node ->
            AstResult.errStringFMsg
                (AstMessageType.InternalError(AstMessageType.PartError))
                "%s is not a valid base part."
                node.TypeName
                node

module private PartModifierValidationError =
    open GslCore.Ast.Process.Validation

    let toAstMessage: PartModifierValidationError -> AstMessage =
        function
        | NotAValidModifierTarget node ->
            AstResult.errStringFMsg
                AstMessageType.PartError
                "Can only apply part mods to Gene or PartId, not %s"
                node.TypeName
                node

module private RecursiveCallCheckError =
    open GslCore.Ast.Process.Validation

    let toAstMessage: RecursiveCallCheckError -> AstMessage =
        function
        | RecursiveCallFoundError (functionCall, node) ->
            AstResult.errStringFMsg
                AstMessageType.RecursiveFunctionCall
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
                AstMessageType.PragmaError
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
open GslCore.Ast.Algorithms

[<RequireQualifiedAccess>]
type Phase1Error =
    | RelativePositionTranslationMessage of RelativePositionTranslationMessage
    | VariableResolutionError of VariableResolutionError
    | FunctionInliningError of InliningError
    | ExpressionReductionError of ExpressionReductionError
    | PragmaBuildingError of PragmaBuildingError
    | AssemblyFlatteningError of AssemblyFlatteningError
    | AssemblyStuffingError of FoldMapError<AssemblyStuffingError, PragmaEnvironmentError>
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
        | Phase1Error.AssemblyStuffingError msg ->
            match msg with
            | MapError inner -> AssemblyStuffingError.toAstMessage inner
            | StateUpdateError (PragmaEnvironmentError.PragmaArgument (inner, node)) ->
                inner |> PragmaArgumentError.toAstMessage node
        | Phase1Error.PragmaWarningError msg -> PragmaWarningError.toAstMessage msg
        | Phase1Error.RoughageExpansionError msg -> RoughageExpansionError.toAstMessage msg
        | Phase1Error.ParseErrorType msg -> ParseErrorType.toAstMessage msg
        | Phase1Error.PartBaseValidationError msg -> PartBaseValidationError.toAstMessage msg
        | Phase1Error.PartModifierValidationError msg -> PartModifierValidationError.toAstMessage msg
        | Phase1Error.RecursiveCallCheckError msg -> RecursiveCallCheckError.toAstMessage msg
        | Phase1Error.LinterHint msg -> LinterHint.toAstMessage msg
        | Phase1Error.FunctionInliningError msg -> FunctionInliningError.toAstMessage msg
        | Phase1Error.NoError _ -> failwith "IMPOSSIBLE"


module NamingError =
    open GslCore.Ast.Process.Naming

    let toAstMessage: NamingError -> AstMessage =
        function
        | NamingError.AddPragma (inner, node) -> inner |> PragmaArgumentError.toAstMessage node
        | NamingError.GetPragma inner -> inner |> GetPragmaError.toAstMessage

module NameCheckError =
    open GslCore.Ast.Process.Naming

    let toAstMessage: NameCheckError -> AstMessage =
        function
        | NameCheckError.UnknownGene (geneName, basePart) ->
            AstResult.errStringFMsg AstMessageType.PartError "Unknown gene: '%s'." geneName basePart
        | NameCheckError.ReferenceError (message, node) ->
            AstMessage.createErrorWithStackTrace AstMessageType.RefGenomeError message node
        | NameCheckError.GetPragmaError inner -> inner |> GetPragmaError.toAstMessage

open GslCore.Ast.Process.Naming

[<RequireQualifiedAccess>]
type PostPhase1Error =
    | NameCheck of NameCheckError
    | Naming of NamingError