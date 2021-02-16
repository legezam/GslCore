namespace GslCore.Pragma

open System
open Amyris.ErrorHandling
open GslCore

/// Accumulate named capabilities from #capa pragmas
type Capabilities = Set<string>

type PragmaArgShape =
    /// require zero args
    | Zero
    /// Require one arg
    | One
    /// Require exactly N args
    | Exactly of int
    /// Require at least N args
    | AtLeast of int
    /// Range is inclusive on both sides, so Range(1,5) accepts one to five parameters.
    | Range of int * int
    /// For the uncommon case where a pragma might accept on a set of exact
    /// numbers of arguments, possibly to dictate behavior.
    | ExactlySet of int list

type PragmaValidationResult = Result<unit, string>

type PragmaPersistence =
    | Persistent
    | PersistentCumulative
    | Transient
    | TransientCumulative

///<summary>
/// Pragmas are scoped within a GSL document.  Some pragmas
/// are somewhat "scope-polymorphic" and have different
/// behavior depending on which scope they appear in.
/// This type indicates whether a pragma is allowed at the block level.
/// If it is, is specifies if the pragma is persistent or transient (only applies
/// to the next assembly).
/// It also specifies if the pragma is allowed in a part.
/// Part-level pragmas are intrinsically transient.
///</summary>
type PragmaScope =
    | PartOnly
    | BlockOnly of PragmaPersistence
    | BlockOrPart of PragmaPersistence
    member this.ToString =
        match this with
        | BlockOrPart (b) -> sprintf "Block (%s); Part" (Utils.getUnionCaseName b)
        | BlockOnly (b) -> sprintf "Block (%s)" (Utils.getUnionCaseName b)
        | PartOnly -> "Part"

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

type PragmaCache(pragmas: Map<string, PragmaDefinition>) =
    member this.Pragmas = pragmas

// ===========================
// PragmaCollection domain type
// ===========================

///<summary>
/// A PragmaCollection is a mapping between pragma name and the actual value
/// set for that pragma.  This is the main data structure in which pragmas
/// are passed around.  It is a helpful and safe wrapping of an immutable map.
/// It should be impossible to add invalid pragmas to this structure without
/// doing it manually through the underlying map.</summary>
type PragmaCollection =
    { Pragmas: Map<string, Pragma>
      Cache: PragmaCache }

    /// Pretty-print a collection of pragmas.
    override x.ToString() =
        let ordered =
            x.Pragmas
            |> Map.toList
            |> List.sortBy fst
            |> List.map snd

        let entries =
            String.concat " " (ordered |> Seq.map (fun p -> p.ToString()))

        sprintf "PragmaCollection: %s" entries

// ======================
// pragma deprecations and deprecation machinery
// ======================

type PragmaDeprecation =
    { Name: string
      Replacement: string
      Replace: Pragma -> Pragma
      ExtraMessage: string option }
    member this.WarningMessage =
        let msg =
            sprintf "The pragma #%s is deprecated; please use #%s instead." this.Name this.Replacement

        match this.ExtraMessage with
        | Some extraMessage -> sprintf "%s\n%s" msg extraMessage
        | None -> msg
