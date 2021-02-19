namespace GslCore.Core

open GslCore.Ast.Types
open GslCore.Core.PluginTypes
open GslCore.Pragma
open GslCore.Reference

type Phase2Parameters =
    { OneShot: bool
      MaxPasses: int option
      Parallel: bool
      Verbose: bool
      LegalCapas: Capabilities
      PragmaBuilder: PragmaBuilder
      AlleleSwapProviders: AlleleSwapProvider list
      References: GenomeDefinitions
      CodonTableCache: ICodonProvider }

module Phase1Parameters =
    let fromPhase2 (phase2: Phase2Parameters): Phase1Parameters =
        { Phase1Parameters.LegalCapabilities = phase2.LegalCapas
          PragmaBuilder = phase2.PragmaBuilder }
