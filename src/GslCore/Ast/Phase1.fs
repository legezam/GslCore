module GslCore.Ast.Phase1

open GslCore.Ast.Process
open GslCore.Ast.Types
open GslCore.Ast
open GslCore.Ast.ErrorHandling
open GslCore.Ast.Algorithms
open Amyris.ErrorHandling
open GslCore.Pragma

// ==================
// phase 1 of AST reduction
// everything before bioinformatics gets involved
// ==================

let immediateValidations =
    Validation.validate
        (Validation.checkParseError
         &&& Validation.validBasePart)

/// Phase 1 is everything before bioinformatics really gets involved.
let phase1 (parameters: Phase1Parameters): AstTreeHead -> Result<AstTreeHead, AstMessage> =
    Linting.linters
    >=> immediateValidations
    >=> Validation.checkRecursiveCalls
    >=> VariableResolution.resolveVariables
    >=> Inlining.inlineFunctionCalls
    >=> Cleanup.stripFunctions
    >=> VariableResolution.resolveVariablesStrict
    >=> Cleanup.stripVariables
    >=> ExpressionReduction.reduceMathExpressions
    >=> PragmaBuilding.buildPragmas parameters
    >=> PragmaWarning.collect
    >=> RelativePosition.compute
    >=> RoughageExpansion.expandRoughageLines // inline roughage expansion is pretty simple so we always do it
    >=> AssemblyFlattening.flattenAssemblies parameters
    >=> (Validation.validate Validation.checkMods)
    >=> AssemblyStuffing.stuffPragmasIntoAssemblies

/// Prep a tree for phase 2, after phase 1 compilation is complete.
let postPhase1 rgs library =
    Naming.checkGeneNames rgs library
    >=> Naming.nameAssemblies