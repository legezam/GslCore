/// Parsing for PCR parameters.
namespace GslCore.PcrParamParse

open System
open Amyris.Bio.primercore
open System.Text.RegularExpressions
open Amyris.ErrorHandling

type PcrUnit =
    | UM
    | MM
    | NM

type PcrParamTag =
    | Mon
    | Div
    | Dntp
    | Template
    | Primer

/// Handle parsing of PCR parameters
module PcrParameterParser =
    // e.g.   ERROR: unknown pragma #pcrparams mon=50mM div=m5mM dNTP=0uM template=0uM primer=0.25uM

    let private pcrParamRegex = Regex("([^= ]*)=(\d*\.?\d*)(\w*)", RegexOptions.Compiled ||| RegexOptions.CultureInvariant)

    // ParseRegex parses a regular expression and returns a list of the strings that match each group in
    // the regular expression.
    // List.tail is called to eliminate the first element in the list, which is the full matched expression,
    // since only the matches for each group are wanted.
    let private (|ParsePcrParam|_|) (str: string): string list option =
        let regexMatch = pcrParamRegex.Match(str)

        if regexMatch.Success then
            let values =
                [ for group in regexMatch.Groups -> group.Value ]
                |> List.tail
            Some values
        else
            None

    let private (|ParsedAsFloat|_|) (str: string): float option =
        match Double.TryParse(str) with
        | true, v -> Some v
        | _ -> None

    let private (|ParsedAsUnit|_|): string -> PcrUnit option =
        function
        | "um" -> Some UM
        | "mm" -> Some MM
        | "nm" -> Some NM
        | _ -> None

    let private (|KnownTag|UnknownTag|): string -> Choice<PcrParamTag, string> =
        function
        | "mon" -> KnownTag Mon
        | "div" -> KnownTag Div
        | "dntp" -> KnownTag Dntp
        | "template" -> KnownTag Template
        | "primer" -> KnownTag Primer
        | unknown -> UnknownTag unknown

    let [<Literal>] private MatchErrorMsg = "#pcrparams should match tag=valueunit pattern (no spaces).
    tags: [mon, div, dntp, template, primer]; value: a decimal number; unit: [uM, mM, nM]
    e.g. #pcrparams mon=50mM div=1.5mM dNTP=200uM template=0.01uM primer=0.25uM"

    /// Try to parse a single argument.
    let parseArg (a: string) =
        match a.ToLower() with
        | ParsePcrParam [ tag; ParsedAsFloat v; ParsedAsUnit u ] ->
            let unitVal =
                match u with
                | UM -> v * 1.0<uM> |> Amyris.Bio.primercore.uM2M
                | MM -> v * 1.0<mM> |> Amyris.Bio.primercore.mM2M
                | NM -> v * 1.0<nM> |> Amyris.Bio.primercore.nM2M

            match tag with
            | KnownTag t -> ok (t, unitVal)
            | UnknownTag t ->
                fail (sprintf "unknown pcr parameter '%s', should be one of mon, div, dntp, template or primer" t)
        | _ -> fail (sprintf "Invalid argument: '%s'. %s" a MatchErrorMsg)

    /// Try to parse a single argument, and use it to up
    let private updatePP (primerParams: PrimerParams) (tag: PcrParamTag, unitVal: float<M>) =
        match tag with
        | Mon -> { primerParams with monovalentConc = unitVal }
        | Div -> { primerParams with divalentConc = unitVal }
        | Dntp -> { primerParams with dNTPConc = unitVal }
        | Template -> { primerParams with templateConc = unitVal }
        | Primer -> { primerParams with primerConc = unitVal }

    let parseArgUpdatePP (a: string) (primerParams: PrimerParams) =
        parseArg a
        |> Trial.lift (updatePP primerParams)
