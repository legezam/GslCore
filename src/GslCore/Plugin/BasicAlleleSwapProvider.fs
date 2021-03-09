﻿module GslCore.Plugin.BasicAlleleSwapProvider

open GslCore.Core.AlleleSwaps
open GslCore.Core.PluginTypes

let basicAlleleSwapPlugin =
    { Name = "classic_allele_swap"
      Description = Some "Allele swap implementation using markers."
      Behaviors =
          [ { Name = None
              Description = None
              Behavior =
                  AlleleSwapAA
                      ({ JobScorer = jobScorerClassicAAMut
                         Provider = classicAAMut }) } ]
      ProvidesPragmas = []
      ProvidesCapas = [] }