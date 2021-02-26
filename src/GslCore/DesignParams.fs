namespace GslCore.DesignParams

open Amyris.Bio.primercore
open GslCore
open GslCore.GslResult
open GslCore.Pragma
open GslCore.Constants
open GslCore.PcrParamParse

type DesignParams =
    { TargetTemp: float<C>
      SeamlessOverlapTemp: float<C>
      PrimerParams: PrimerParams
      OverlapParams: PrimerParams
      OverlapMinLength: int }

type DesignParameterError = PcrParameterRevisionError of PcrParameterParseError

module DesignParams =
    ///<summary>
    /// Given an initial set of PrimerParams, return a new set based on parsing arguments
    /// passed to a #pcrparams pragma.
    /// </summary>
    let revise (designParams: PrimerParams) (arguments: string list): GslResult<PrimerParams, PcrParameterParseError> =
        arguments
        |> List.fold (fun primerParams argument ->
            primerParams
            >>= (PcrParameterParser.parseArgUpdatePP argument)) (GslResult.ok designParams)

    /// Starting design parameters for construction
    let identity =
        { PrimerParams = defaultParams
          TargetTemp = Default.RyseLinkerTargetTemp
          SeamlessOverlapTemp = Default.SeamlessTargetTemp
          OverlapParams = defaultParams
          OverlapMinLength = Default.MinOverlapLength }


    let updateFromPragma (pragma: Pragma) (designParams: DesignParams): GslResult<DesignParams, DesignParameterError> =
        match pragma.Name, pragma.Arguments with
        | BuiltIn.PcrParamsName, args ->
            revise designParams.PrimerParams args
            |> GslResult.map (fun primerParams ->
                { designParams with
                      PrimerParams = primerParams })
            |> GslResult.mapError PcrParameterRevisionError
        | BuiltIn.PcrAssemblyParamsName, args ->
            revise designParams.OverlapParams args
            |> GslResult.map (fun overlapParams ->
                { designParams with
                      OverlapParams = overlapParams })
            |> GslResult.mapError PcrParameterRevisionError
        | BuiltIn.TargetTmName, head :: _ ->
            GslResult.ok
                { designParams with
                      TargetTemp = Utils.strToTempC head }
        | BuiltIn.MinOverlapLenName, head :: _ ->
            GslResult.ok
                { designParams with
                      OverlapMinLength = int head }
        | BuiltIn.SeamlesOverlapTmName, head :: _ ->
            GslResult.ok
                { designParams with
                      SeamlessOverlapTemp = Utils.strToTempC head }
        | BuiltIn.AtPenaltyName, head :: _ ->
            GslResult.ok
                { designParams with
                      PrimerParams =
                          { designParams.PrimerParams with
                                ATPenalty = float head * 1.0<C> } }
        | _ -> GslResult.ok designParams

    /// Given a pragma environment, compute assembly physical design parameters from an existing set.
    let fromPragmas (designParams: DesignParams)
                    (pragmaCollection: PragmaCollection)
                    : GslResult<DesignParams, DesignParameterError> =
        pragmaCollection
        |> PragmaCollection.values
        |> Seq.fold (fun designParams pragma -> designParams >>= (updateFromPragma pragma)) (GslResult.ok designParams)
