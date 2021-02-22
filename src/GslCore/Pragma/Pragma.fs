namespace GslCore.Pragma

open System
open GslCore.GslResult

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
    let fromDefinition (arguments: string list) (pragmaDefinition: PragmaDefinition): GslResult<Pragma, string> =
        let name = pragmaDefinition.Name

        // check that the right number of arguments were supplied
        let nArg = arguments.Length

        let checkNArgs n =
            if nArg <> n
            then GslResult.err (sprintf "Pragma #%s expected %d argument(s) but got %d: %A" name n nArg arguments)
            else GslResult.ok ()

        let checkMinArgs min =
            if nArg < min
            then GslResult.err
                     (sprintf "Pragma #%s expected at least %d argument(s) but got %d: %A" name min nArg arguments)
            else GslResult.ok ()

        let checkMaxArgs max _ =
            if nArg > max
            then GslResult.err
                     (sprintf "Pragma #%s expected at most %d argument(s) but got %d: %A" name max nArg arguments)
            else GslResult.ok ()

        let checkArgShape (): GslResult<unit, string> =
            match pragmaDefinition.Shape with
            | Zero -> checkNArgs 0
            | One -> checkNArgs 1
            | Exactly n -> checkNArgs n
            | AtLeast n -> checkMinArgs n
            | Range (min, max) -> checkMinArgs min >>= checkMaxArgs max
            | ExactlySet vals ->
                if not (vals |> List.contains nArg) then
                    GslResult.err
                        (sprintf
                            "Pragma %s expected any number of arguments in the set %A but got %d: %A"
                             name
                             vals
                             nArg
                             arguments)
                else
                    GslResult.ok ()

        let validateArgs (): PragmaValidationResult = pragmaDefinition.Validate arguments

        checkArgShape ()
        >>= validateArgs
        |> GslResult.map (fun () ->
            { Pragma.Definition = pragmaDefinition
              Arguments = arguments })
