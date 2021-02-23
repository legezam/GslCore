module GslCore.Ast.Phase1


open GslCore.GslResult
open GslCore.Ast.Process
open GslCore.Ast.Types
open GslCore.Ast
open GslCore.Ast.ErrorHandling
open GslCore.Ast.Algorithms
open GslCore.Pragma
open GslCore.Ast.Process.RelativePositionTranslation
open GslCore.Ast.Process.VariableResolution
open GslCore.Ast.MessageTranslation

// ==================
// phase 1 of AST reduction
// everything before bioinformatics gets involved
// ==================

let immediateValidations =
    Validation.validate
        (Validation.checkParseError
         &&& Validation.validBasePart)

/// Phase 1 is everything before bioinformatics really gets involved.
let phase1 (parameters: Phase1Parameters): AstTreeHead -> AstResult<AstTreeHead> =
    Linting.linters
    >=> immediateValidations
    >=> Validation.checkRecursiveCalls
    >=> (VariableResolution.resolveVariables
         >> GslResult.mapError VariableResolutionMessage.toAstMessage)
    >=> Inlining.inlineFunctionCalls
    >=> Cleanup.stripFunctions
    >=> (VariableResolution.resolveVariablesStrict
         >> GslResult.mapError VariableResolutionMessage.toAstMessage)
    >=> Cleanup.stripVariables
    >=> ExpressionReduction.reduceMathExpressions
    >=> PragmaBuilding.buildPragmas parameters
    >=> PragmaWarning.collect
    >=> (RelativePositionTranslation.compute
         >> GslResult.mapError RelativePositionTranslationMessage.toAstMessage)
    >=> RoughageExpansion.expandRoughageLines // inline roughage expansion is pretty simple so we always do it
    >=> AssemblyFlattening.flattenAssemblies parameters
    >=> (Validation.validate Validation.checkMods)
    >=> AssemblyStuffing.stuffPragmasIntoAssemblies

/// Prep a tree for phase 2, after phase 1 compilation is complete.
let postPhase1 rgs library =
    Naming.checkGeneNames rgs library
    >=> Naming.nameAssemblies
