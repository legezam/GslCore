module GslCore.DesignParams

open Amyris.Bio.primercore
open Amyris.ErrorHandling
open GslCore.Pragma
open GslCore.Pragma.Domain
open GslCore.Constants
open GslCore.PcrParamParse

///<summary>
/// Given an initial set of PrimerParams, return a new set based on parsing arguments
/// passed to a #pcrparams pragma.
/// </summary>
let revisePP (p: PrimerParams) (arguments: string list) =
    arguments
    |> List.fold (fun pp a -> pp >>= (PcrParameterParser.parseArgUpdatePP a)) (ok p)

type DesignParams =
    { targetTm: float<C>
      seamlessOverlapTm: float<C>
      pp: PrimerParams
      overlapParams: PrimerParams
      overlapMinLen: int }

/// Starting design parameters for construction
let initialDesignParams =
    { pp = defaultParams
      targetTm = Default.RyseLinkerTargetTemp
      seamlessOverlapTm = Default.SeamlessTargetTemp
      overlapParams = defaultParams
      overlapMinLen = Default.MinOverlapLength }

let updateDPFromPragma (pragma: Pragma) (designParams: DesignParams) =
    match pragma.Name, pragma.Arguments with
    | "pcrparams", args ->
        revisePP designParams.pp args
        >>= (fun pp -> ok { designParams with pp = pp })
    | "pcrassemblyparams", args ->
        revisePP designParams.overlapParams args
        >>= (fun overlapParams ->
            ok
                { designParams with
                      overlapParams = overlapParams })
    | "targettm", v :: _ ->
        ok
            { designParams with
                  targetTm = Utils.strToTempC v }
    | "minoverlaplen", v :: _ ->
        ok
            { designParams with
                  overlapMinLen = int v }
    | "seamlessoverlaptm", v :: _ ->
        ok
            { designParams with
                  seamlessOverlapTm = Utils.strToTempC v }
    | "atpenalty", v :: _ ->
        ok
            { designParams with
                  pp =
                      { designParams.pp with
                            ATPenalty = float v * 1.0<C> } }
    | _ -> ok designParams

/// Given a pragma environment, compute assembly physical design parameters from an existing set.
let designParamsFromPragmas (designParams: DesignParams)
                            (pragmaCollection: PragmaCollection)
                            : Result<DesignParams, string> =

    pragmaCollection
    |> PragmaCollection.values
    |> Seq.fold (fun designParams pragma -> designParams >>= (updateDPFromPragma pragma)) (ok designParams)
