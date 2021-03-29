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

[<RequireQualifiedAccess>]
type ArgumentShapeError =
    | ArgumentNumberMismatch of pragmaName: string * expected: int * actual: int * arguments: string list
    | TooFewArguments of pragmaName: string * expected: int * actual: int * arguments: string list
    | TooManyArguments of pragmaName: string * expected: int * actual: int * arguments: string list
    | SetMismatch of pragmaName: string * expectedSet: int list * actual: int * arguments: string list

[<RequireQualifiedAccess>]
type PragmaArgumentError =
    | Shape of ArgumentShapeError
    | Validation of string
    

module Pragma =
    let getName (this: Pragma): string = this.Definition.Name

    let isTransient (this: Pragma): bool =
        match this.Definition.Scope with
        | BlockOnly Persistent
        | BlockOnly PersistentCumulative
        | BlockOrPart Persistent
        | BlockOrPart PersistentCumulative -> false
        | BlockOnly Transient
        | BlockOnly TransientCumulative
        | BlockOrPart Transient
        | BlockOrPart TransientCumulative
        | PartOnly -> true
    /// Does this pragma announce the availability of an extension capability?
    let setsCapability (this: Pragma): string option =
        if this.Definition = BuiltIn.capaPragmaDef then Some(this.Arguments.[0].ToLower()) else None

    /// Is this pragma a warning message?
    let isWarning (this: Pragma): bool =
        this.Definition = BuiltIn.warningPragmaDef
    /// Is this pragma a flag to deactivate a warning?
    let ignoresWarning (this: Pragma): string option =
        if this.Definition = BuiltIn.warnoffPragmaDef then
            Some(this.Arguments.[0])
        else
            None
    /// Is this pragma a #capa directive?
    let isCapa (this: Pragma): bool = this.Definition = BuiltIn.capaPragmaDef
    /// Helper function to check the list of args for a particular value.
    let hasVal (value: string) (this: Pragma): bool = List.contains value this.Arguments

    /// Validated pragma construction during parsing
    let fromDefinition (arguments: string list)
                       (pragmaDefinition: PragmaDefinition)
                       : GslResult<Pragma, PragmaArgumentError> =
        let name = pragmaDefinition.Name

        // check that the right number of arguments were supplied
        let numOfArguments = arguments.Length

        let checkNArgs expected =
            if numOfArguments <> expected then
                GslResult.err (ArgumentShapeError.ArgumentNumberMismatch(name, expected, numOfArguments, arguments))
            else
                GslResult.ok ()

        let checkMinArgs min =
            if numOfArguments < min then
                GslResult.err (ArgumentShapeError.TooFewArguments(name, min, numOfArguments, arguments))
            else
                GslResult.ok ()

        let checkMaxArgs max _ =
            if numOfArguments > max then
                GslResult.err (ArgumentShapeError.TooManyArguments(name, max, numOfArguments, arguments))
            else
                GslResult.ok ()

        let checkArgShape (): GslResult<unit, ArgumentShapeError> =
            match pragmaDefinition.Shape with
            | Zero -> checkNArgs 0
            | One -> checkNArgs 1
            | Exactly n -> checkNArgs n
            | AtLeast n -> checkMinArgs n
            | Range (min, max) -> checkMinArgs min >>= checkMaxArgs max
            | ExactlySet vals ->
                if not (vals |> List.contains numOfArguments) then
                    GslResult.err (ArgumentShapeError.SetMismatch(name, vals, numOfArguments, arguments))
                else
                    GslResult.ok ()

        let validateArgs _: GslResult<unit, string> = pragmaDefinition.Validate arguments

        checkArgShape ()
        |> GslResult.mapError PragmaArgumentError.Shape
        >>= (validateArgs
             >> GslResult.mapError PragmaArgumentError.Validation)
        |> GslResult.map (fun _ ->
            { Pragma.Definition = pragmaDefinition
              Arguments = arguments })
