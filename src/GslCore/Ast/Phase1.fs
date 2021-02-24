module GslCore.Ast.Phase1


open GslCore.Ast.Linting
open GslCore.Ast.Process.Validation
open GslCore.GslResult
open GslCore.Ast.Process
open GslCore.Ast.Types
open GslCore.Ast
open GslCore.Ast.ErrorHandling
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
open GslCore.Ast.MessageTranslation

// ==================
// phase 1 of AST reduction
// everything before bioinformatics gets involved
// ==================

let checkParseError =
    ParseErrorValidation.checkParseError
    >> GslResult.mapError ParseErrorMessage.toAstMessage

let basePartValidation =
    PartValidation.validBasePart
    >> GslResult.mapError PartBaseValidationMessage.toAstMessage

let linting =
    Linter.linters
    >> GslResult.mapError LinterHintMessage.toAstMessage

let immediateValidations =
    Validation.validate (checkParseError &&& basePartValidation)

let recursiveCallCheck =
    RecursiveCalls.check
    >> GslResult.mapError RecursiveCallCheckMessage.toAstMessage

let variableResolution =
    VariableResolution.resolveVariables
    >> GslResult.mapError VariableResolutionMessage.toAstMessage

let functionInlining =
    Inlining.inlineFunctionCalls
    >> GslResult.mapError FunctionInliningMessage.toAstMessage

let stripFunctions =
    Cleanup.stripFunctions
    >> GslResult.mapError NoMessage.toAstMessage

let variableResolutionStrict =
    VariableResolution.resolveVariablesStrict
    >> GslResult.mapError VariableResolutionMessage.toAstMessage

let stripVariables =
    Cleanup.stripVariables
    >> GslResult.mapError NoMessage.toAstMessage

let expressionReduction =
    ExpressionReduction.reduceMathExpressions
    >> GslResult.mapError ExpressionReductionMessage.toAstMessage

let pragmaBuilding parameters =
    PragmaBuilding.buildPragmas parameters
    >> GslResult.mapError PragmaBuildingMessage.toAstMessage

let pragmaWarningCollection =
    PragmaWarning.collect
    >> GslResult.mapError PragmaWarningMessage.toAstMessage

let relativePositionTranslation =
    RelativePositionTranslation.compute
    >> GslResult.mapError RelativePositionTranslationMessage.toAstMessage

let roughageExpansion =
    RoughageExpansion.expandRoughageLines
    >> GslResult.mapError RoughageExpansionMessage.toAstMessage

let assemblyFlattening parameters =
    AssemblyFlattening.flattenAssemblies parameters
    >> GslResult.mapError AssemblyFlatteningMessage.toAstMessage

let partModifierValidation =
    Validation.validate PartValidation.validateModifiers
    >> GslResult.mapError PartModifierValidationMessage.toAstMessage

let assemblyStuffing =
    AssemblyStuffing.stuffPragmasIntoAssemblies
    >> GslResult.mapError AssemblyStuffingMessage.toAstMessage
/// Phase 1 is everything before bioinformatics really gets involved.
let phase1 (parameters: Phase1Parameters): AstTreeHead -> AstResult<AstTreeHead> =
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
    Naming.checkGeneNames rgs library
    >=> Naming.nameAssemblies
