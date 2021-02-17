namespace GslCore.Uri

open Amyris.ErrorHandling
type Uri = string

module Uri =
    // Big TODO: dry out the duplication of these constants between Thumper and GSLc

    [<Literal>]
    let AmyrisUriBase = "http://amyris.com/GBoM"

    [<Literal>]
    let private UriPathDelimiter = "/"

    [<Literal>]
    let private UriTermDelimiter = "/"


    let private forbiddenChars = [| UriPathDelimiter; UriTermDelimiter |]

    /// Check a string for forbidden characters, return Some(s, badChars) if any are found.
    let private checkForbiddenChars (input: string): (string * string list) option =
        let badChars =
            [ for forbiddenChar in forbiddenChars do
                if input.Contains(forbiddenChar) then yield forbiddenChar ]

        match badChars with
        | [] -> None
        | x -> Some(input, x)

    let private checkTermsForIssues (terms: string list): string option =
        let issues = List.choose checkForbiddenChars terms

        if not issues.IsEmpty
        then Some("Found bad chars TODO informative error message.")
        else None


    /// Construct a local URI from a list of namespaces and an instance term.
    let buildUri (namespaces: string list) (term: string): Result<string, string> =
        // TODO: type constraint on stringifyable term?
        match checkTermsForIssues (term :: namespaces) with
        | Some errorMessage -> fail errorMessage
        | None ->
            let ub = System.Text.StringBuilder()
            ub.Append(AmyrisUriBase) |> ignore

            for ns in namespaces do
                ub.Append(UriPathDelimiter + ns) |> ignore

            ub.Append(UriTermDelimiter) |> ignore
            ub.Append(term) |> ignore
            ok (ub.ToString())

    /// Construct a URI namespace extension.
    let addNamespaces (baseNamespace: string) (namespaces: string list): Result<string, string> =
        match checkTermsForIssues namespaces with
        | Some errorMessage -> fail errorMessage
        | None ->
            let ub = System.Text.StringBuilder()
            ub.Append(baseNamespace) |> ignore

            for ns in namespaces do
                ub.Append(UriPathDelimiter + ns) |> ignore

            ok (ub.ToString())

    /// Add a term entry into a namespace.
    let addTermToNamespace (baseNamespace: string) (term: string): Result<string, string> =
        match checkTermsForIssues [ term ] with
        | Some errorMessage -> fail errorMessage
        | None -> ok (baseNamespace + UriTermDelimiter + term)

    // TODO: possibly move these definitions into the appropriate module
    let private linkerBase =
        addNamespaces AmyrisUriBase [ "Component"; "Linker" ]
        |> returnOrFail

    /// Construct a RYSE linker URI from a link code.
    /// Since this is entirely programmatic we expect it should never fail at
    /// runtime; thus, raises an exception on error.
    let linkerUri (linkCode: string): Result<string, string> = addTermToNamespace linkerBase linkCode

    let private gslcTempUriBase =
        addNamespaces AmyrisUriBase [ "GSLC"; "TEMP" ]
        |> returnOrFail

    // heap-allocated counter
    let private globalUriCounter: int ref = ref 0

    /// Construct a locally-unique temporary URI.
    let createTempUri (): string =
        let value = !globalUriCounter
        globalUriCounter := value + 1

        addTermToNamespace gslcTempUriBase (sprintf "%d" value)
        |> returnOrFail
