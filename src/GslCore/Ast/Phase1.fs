module GslCore.Ast.Phase1


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

let immediateValidations =
    Validation.validate
        ((ParseErrorValidation.checkParseError
          >> GslResult.mapError ParseErrorMessage.toAstMessage)
         &&& (PartValidation.validBasePart
              >> GslResult.mapError PartBaseValidationMessage.toAstMessage))

/// Phase 1 is everything before bioinformatics really gets involved.
let phase1 (parameters: Phase1Parameters): AstTreeHead -> AstResult<AstTreeHead> =
    Linting.linters
    >=> immediateValidations
    >=> (RecursiveCalls.check
         >> GslResult.mapError RecursiveCallCheckMessage.toAstMessage)
    >=> (VariableResolution.resolveVariables
         >> GslResult.mapError VariableResolutionMessage.toAstMessage)
    >=> (Inlining.inlineFunctionCalls
         >> GslResult.mapError FunctionInliningMessage.toAstMessage)
    >=> (Cleanup.stripFunctions
         >> GslResult.mapError NoMessage.toAstMessage)
    >=> (VariableResolution.resolveVariablesStrict
         >> GslResult.mapError VariableResolutionMessage.toAstMessage)
    >=> (Cleanup.stripVariables
         >> GslResult.mapError NoMessage.toAstMessage)
    >=> (ExpressionReduction.reduceMathExpressions
         >> GslResult.mapError ExpressionReductionMessage.toAstMessage)
    >=> (PragmaBuilding.buildPragmas parameters
         >> GslResult.mapError PragmaBuildingMessage.toAstMessage)
    >=> (PragmaWarning.collect
         >> GslResult.mapError PragmaWarningMessage.toAstMessage)
    >=> (RelativePositionTranslation.compute
         >> GslResult.mapError RelativePositionTranslationMessage.toAstMessage)
    >=> (RoughageExpansion.expandRoughageLines
         >> GslResult.mapError RoughageExpansionMessage.toAstMessage) // inline roughage expansion is pretty simple so we always do it
    >=> (AssemblyFlattening.flattenAssemblies parameters
         >> GslResult.mapError AssemblyFlatteningMessage.toAstMessage)
    >=> (Validation.validate PartValidation.checkMods
         >> GslResult.mapError PartModifierValidationMessage.toAstMessage)
    >=> (AssemblyStuffing.stuffPragmasIntoAssemblies
         >> GslResult.mapError AssemblyStuffingMessage.toAstMessage)

/// Prep a tree for phase 2, after phase 1 compilation is complete.
let postPhase1 rgs library =
    Naming.checkGeneNames rgs library
    >=> Naming.nameAssemblies
