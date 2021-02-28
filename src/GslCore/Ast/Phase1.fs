module GslCore.Ast.Phase1


open GslCore.Ast.Linting
open GslCore.Ast.Process.Validation
open GslCore.GslResult
open GslCore.Ast.Process
open GslCore.Ast.Types
open GslCore.Ast.Algorithms
open GslCore.Pragma
open GslCore.Ast.Process.RelativePositionTranslation
open GslCore.Ast.Process.VariableResolution
open GslCore.Ast.Process.Inlining
open GslCore.Ast.Process.ExpressionReduction
open GslCore.Ast.Process.AssemblyStuffing
open GslCore.Ast.Process.AssemblyFlattening
open GslCore.Ast.Process.RoughageExpansion
open GslCore.Ast.Process.PragmaWarning
open GslCore.Ast.Process.PragmaBuilding
open GslCore.Ast.Process.Naming
open GslCore.Ast.Phase1Message

// ==================
// phase 1 of AST reduction
// everything before bioinformatics gets involved
// ==================

let checkParseError =
    ParseErrorValidation.checkParseError
    >> GslResult.mapError Phase1Error.ParseErrorType

let basePartValidation =
    PartValidation.validBasePart
    >> GslResult.mapError Phase1Error.PartBaseValidationError

let linting =
    Linter.linters
    >> GslResult.mapError Phase1Error.LinterHint

let immediateValidations =
    Validation.validate (checkParseError &&& basePartValidation)

let recursiveCallCheck =
    RecursiveCalls.check
    >> GslResult.mapError Phase1Error.RecursiveCallCheckError

let variableResolution =
    VariableResolution.resolveVariables
    >> GslResult.mapError Phase1Error.VariableResolutionError

let functionInlining =
    Inlining.inlineFunctionCalls
    >> GslResult.mapError Phase1Error.FunctionInliningError

let stripFunctions =
    Cleanup.stripFunctions
    >> GslResult.mapError Phase1Error.NoError

let variableResolutionStrict =
    VariableResolution.resolveVariablesStrict
    >> GslResult.mapError Phase1Error.VariableResolutionError

let stripVariables =
    Cleanup.stripVariables
    >> GslResult.mapError Phase1Error.NoError

let expressionReduction =
    ExpressionReduction.reduceMathExpressions
    >> GslResult.mapError Phase1Error.ExpressionReductionError

let pragmaBuilding parameters =
    PragmaBuilding.buildPragmas parameters
    >> GslResult.mapError Phase1Error.PragmaBuildingError

let pragmaWarningCollection =
    PragmaWarning.collect
    >> GslResult.mapError Phase1Error.PragmaWarningError

let relativePositionTranslation =
    RelativePositionTranslation.compute
    >> GslResult.mapError Phase1Error.RelativePositionTranslationMessage

let roughageExpansion =
    RoughageExpansion.expandRoughageLines
    >> GslResult.mapError Phase1Error.RoughageExpansionError

let assemblyFlattening parameters =
    AssemblyFlattening.flattenAssemblies parameters
    >> GslResult.mapError Phase1Error.AssemblyFlatteningError

let partModifierValidation =
    Validation.validate PartValidation.validateModifiers
    >> GslResult.mapError Phase1Error.PartModifierValidationError

let assemblyStuffing =
    AssemblyStuffing.stuffPragmasIntoAssemblies
    >> GslResult.mapError Phase1Error.AssemblyStuffingError

/// Phase 1 is everything before bioinformatics really gets involved.
let phase1 (parameters: Phase1Parameters): AstTreeHead -> GslResult<AstTreeHead, Phase1Error> =
    linting
    >=> immediateValidations
    >=> recursiveCallCheck
    >=> variableResolution
    >=> functionInlining
    >=> stripFunctions
    >=> variableResolutionStrict
    >=> stripVariables
    >=> expressionReduction
    >=> pragmaBuilding parameters
    >=> pragmaWarningCollection
    >=> relativePositionTranslation
    >=> roughageExpansion // inline roughage expansion is pretty simple so we always do it
    >=> assemblyFlattening parameters
    >=> partModifierValidation
    >=> assemblyStuffing

/// Prep a tree for phase 2, after phase 1 compilation is complete.
let postPhase1 rgs library =
    NameChecking.checkGeneNames rgs library
    >=> NameChecking.nameAssemblies
