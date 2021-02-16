namespace GslCore.Pragma

///<summary>
/// Formal declaration of a pragma.
/// A pragma is fully specified by its name and the shape of the arguments it accepts.
/// A validation function may be optionally provided to fail fast during parsing
/// rather than when the pragma is used.
///</summary>
[<CustomEquality; NoComparison>]
type PragmaDefinition =
    { Name: string
      Shape: PragmaArgShape
      Description: string
      Scope: PragmaScope
      InvertsTo: string option
      Validate: (string list -> PragmaValidationResult) }
    /// Since we always check for duplicate pragma defs, comparing names is sufficient for equality.
    override this.Equals(other) =
        match other with
        | :? PragmaDefinition as y -> this.Name = y.Name
        | _ -> false
    /// Hash pragma defs just by their name.
    override this.GetHashCode() = hash this.Name
    

module PragmaDefinition =

    /// Format a pragma definition.
    let format (p: PragmaDefinition): string =
        let argDescFormat v = sprintf "<a%d>" v

        let makeArgDesc (n: int) =
            [ 0 .. n - 1 ]
            |> Seq.map argDescFormat
            |> String.concat " "

        let argDesc =
            match p.Shape with
            | Zero -> ""
            | One -> makeArgDesc 1
            | Exactly (n) -> makeArgDesc n
            | AtLeast (n) -> (makeArgDesc n) + " ..."
            | Range (n, m) ->
                (makeArgDesc n)
                + " (..."
                + (argDescFormat (m - 1))
                + ")"
            | ExactlySet (v) -> sprintf " <arg shapes: %A>" v

        let firstLine = sprintf "#%s %s" p.Name argDesc

        let descLines =
            p.Description.Split [| '\n' |]
            |> List.ofArray
            |> List.map (fun d -> "    " + d)

        let scopeLine =
            sprintf "    Scoping: %s" p.Scope.ToString

        (firstLine :: scopeLine :: descLines)
        |> String.concat "\n"    