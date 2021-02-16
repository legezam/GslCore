namespace GslCore.Pragma

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


