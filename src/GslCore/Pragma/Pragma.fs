namespace GslCore.Pragma

open System

open FsToolkit.ErrorHandling.Operator.Result
/// Instance of a pragma directive.
[<CustomEquality; CustomComparison>]
type Pragma =
    { Definition: PragmaDefinition
      Arguments: string list }

    member this.Name: string = this.Definition.Name

    /// Only consider pragma name and args in comparisons.
    override this.Equals(obj) =
        match obj with
        | :? Pragma as other ->
            (this.Name = other.Name
             && this.Arguments = other.Arguments)
        | _ -> false
    /// Hash a Pragma as a combination of pName and args.
    override this.GetHashCode() = hash (this.Name, this.Arguments)

    interface IComparable with
        member this.CompareTo obj =
            match obj with
            | :? Pragma as other -> compare (this.Name, this.Arguments) (other.Name, other.Arguments)
            | _ -> invalidArg "obj" "cannot compare values of different types"

    override this.ToString() =
        sprintf "#%s %s" this.Name (String.concat " " this.Arguments)
        

module Pragma =
    let getName (this: Pragma): string = this.Definition.Name

    let isTransient (this: Pragma): bool =
        match this.Definition.Scope with
        | BlockOnly (Persistent)
        | BlockOrPart (Persistent)
        | BlockOnly (PersistentCumulative)
        | BlockOrPart (PersistentCumulative) -> false
        | _ -> true
    /// Does this pragma announce the availability of an extension capability?
    let setsCapability (this: Pragma): string option =
        if this.Definition = BuiltIn.capaPragmaDef then Some(this.Arguments.[0].ToLower()) else None

    /// Is this pragma a warning message?
    let isWarning (this: Pragma): bool =
        this.Definition = BuiltIn.warningPragmaDef
    /// Is this pragma a flag to deactivate a warning?
    let ignoresWarning (this: Pragma): string option =
        if this.Definition = BuiltIn.warnoffPragmaDef
        then Some(this.Arguments.[0])
        else None
    /// Is this pragma a #capa directive?
    let isCapa (this: Pragma): bool = this.Definition = BuiltIn.capaPragmaDef
    /// Helper function to check the list of args for a particular value.
    let hasVal (value: string) (this: Pragma): bool = List.contains value this.Arguments


    /// Validated pragma construction during parsing
    let fromDefinition (values: string list) (pDef: PragmaDefinition): Result<Pragma, string> =
        let name = pDef.Name

        // check that the right number of arguments were supplied
        let nArg = values.Length

        let checkNArgs n =
            if nArg <> n
            then Error (sprintf "Pragma #%s expected %d argument(s) but got %d: %A" name n nArg values)
            else Ok ()

        let checkMinArgs min =
            if nArg < min
            then Error (sprintf "Pragma #%s expected at least %d argument(s) but got %d: %A" name min nArg values)
            else Ok ()

        let checkMaxArgs max _ =
            if nArg > max
            then Error (sprintf "Pragma #%s expected at most %d argument(s) but got %d: %A" name max nArg values)
            else Ok ()

        let checkArgShape (): Result<unit, string> =
            match pDef.Shape with
            | Zero -> checkNArgs 0
            | One -> checkNArgs 1
            | Exactly (n) -> checkNArgs n
            | AtLeast (n) -> checkMinArgs n
            | Range (min, max) -> checkMinArgs min >>= checkMaxArgs max
            | ExactlySet (vals) ->
                if not (vals |> List.contains nArg) then
                    Error
                        (sprintf
                            "Pragma %s expected any number of arguments in the set %A but got %d: %A"
                             name
                             vals
                             nArg
                             values)
                else
                    Ok ()

        let validateArgs (): PragmaValidationResult = pDef.Validate values

        // validation pipeline
        checkArgShape ()
        >>= validateArgs
        >>= (fun () ->
            Ok
                { Definition = pDef
                  Arguments = values })

       