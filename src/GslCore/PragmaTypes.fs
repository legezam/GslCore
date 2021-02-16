module GslCore.PragmaTypes

open System
open Amyris.ErrorHandling
open GslCore
open GslCore.PcrParamParse
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

// Helper functions for generic validation of simple parameters.
let parseNumber parseFunc kind (args: string list) =
    match args with
    | [ i ] ->
        match parseFunc i with
        | true, _ -> ok ()
        | false, _ -> fail (sprintf "Could not parse %s as an %s." i kind)
    // We shouldn't ever hit this clause as we should have already blown up with
    // a different error.
    | x -> fail (sprintf "parse number expected one argument but got %d" x.Length)

let parseInt = parseNumber (Int64.TryParse) "int"
let parseDouble = parseNumber (Double.TryParse) "float"

let validatePcrParams (args: string list) =
    args
    |> Seq.map PcrParameterParser.parseArg
    |> Seq.map (lift ignore)
    |> collectValidations

type Platform =
    | Stitch
    | Megastitch

let parsePlatform: string list -> Result<Platform, string> =
    function
    | [ singleItem ] ->
        match singleItem with
        | "stitch" -> ok Stitch
        | "megastitch" -> ok Megastitch
        | _ -> fail (sprintf "Invalid platform '%s'.  Options are 'stitch' and 'megastitch'." singleItem)
    // We shouldn't ever hit this clause as we should have already blown up with
    // a different error.
    | x -> fail (sprintf "platform expected one argument but got %d" x.Length)

/// Represents topology information about contructs
type Topology =
    | Linear
    | Circular

module Topology =
    [<Literal>]
    let LinearValue = "linear"

    [<Literal>]
    let CircularValue = "circular"

    [<Literal>]
    let PragmaName = "topology"

    let parse: string list -> Result<Topology, string> =
        function
        | [ arg ] ->
            match arg with
            | LinearValue -> ok Linear
            | CircularValue -> ok Circular
            | _ -> fail (sprintf "Invalid topology '%s'.  Options are 'linear' and 'circular'." arg)
        | x -> fail (sprintf "topology expected one argument but got %d" x.Length)

/// Pass-through placeholder validator.
let noValidate _ = ok ()

// Pragma defs that are used internally by the compiler.

let warningPragmaDef =
    { Name = "warn"
      Shape = AtLeast(1)
      Scope = BlockOrPart(Transient)
      Description = "Print a warning message."
      InvertsTo = None
      Validate = noValidate }

let warnoffPragmaDef =
    { Name = "warnoff"
      Shape = One
      Scope = BlockOnly(Persistent)
      Description = "Turn off specific warnings."
      InvertsTo = None
      Validate = noValidate }

let capaPragmaDef =
    { Name = "capa"
      Shape = One
      Scope = BlockOnly(Persistent)
      Description = "Enables particular compiler capabilities."
      InvertsTo = None
      Validate = noValidate }

let platformPragmaDef =
    { Name = "platform"
      Shape = One
      Scope = BlockOnly(Persistent)
      Description = "Specify an assembly platform to target, current options: 'stitch', 'megastitch'."
      InvertsTo = None
      Validate = parsePlatform >> (lift ignore) }

let markersetPragmaDef =
    { Name = "markerset"
      Shape = One
      Scope = BlockOnly(Persistent)
      Description = "Set the default marker set for a ### part."
      InvertsTo = None
      Validate = noValidate }

let namePragmaDef =
    { Name = "name"
      Shape = One
      Scope = BlockOrPart(Transient)
      Description = "Override name for a assembly or part."
      InvertsTo = None
      Validate = noValidate }

let fusePragmaDef =
    { Name = "fuse"
      Shape = Zero
      Scope = PartOnly
      Description = "Create a seamless junction with the next part."
      InvertsTo = None
      Validate = noValidate }

let ampPragmaDef =
    { Name = "amp"
      Shape = Zero
      Scope = PartOnly
      Description = "Create a seamless junction with the next part."
      InvertsTo = None
      Validate = noValidate }

let topologyPragmaDef =
    { Name = Topology.PragmaName
      Shape = One
      Scope = BlockOnly(Persistent)
      Description = "The design has either linear or circular topology"
      InvertsTo = None
      Validate = Topology.parse >> (lift ignore) }

/// Base set of hard coded pragmas.  Plugins might augment this list
let pragmaDefsStatic: PragmaDefinition list =
    [ warningPragmaDef
      warnoffPragmaDef
      capaPragmaDef
      platformPragmaDef
      markersetPragmaDef
      namePragmaDef
      fusePragmaDef
      ampPragmaDef
      { Name = "linkers"
        Shape = AtLeast(1)
        Scope = BlockOnly(Persistent)
        Description = "Override the default set of RYSE linkers."
        InvertsTo = None
        Validate = noValidate }
      { Name = "refgenome"
        Shape = One
        Scope = BlockOrPart(Persistent)
        Description = "Specify a reference genome."
        InvertsTo = None
        Validate = noValidate }
      { Name = "dnasrc"
        Shape = One
        Scope = PartOnly
        Description = "Specify a DNA source for a part."
        InvertsTo = None
        Validate = noValidate }
      { Name = "stitch"
        Shape = Zero
        Scope = BlockOnly(Persistent)
        Description = "TODO description"
        InvertsTo = None
        Validate = noValidate }
      { Name = "megastitch"
        Shape = Zero
        Scope = BlockOnly(Persistent)
        Description = "TODO description"
        InvertsTo = None
        Validate = noValidate }
      { Name = "rabitstart"
        Shape = Zero
        Scope = PartOnly
        Description = "Designate part as the start of a RYSE rabit."
        InvertsTo = Some("rabitend")
        Validate = noValidate }
      { Name = "rabitend"
        Shape = Zero
        Scope = PartOnly
        Description = "Designate part as the end of a RYSE rabit."
        InvertsTo = Some("rabitstart")
        Validate = noValidate }
      { Name = "primerpos"
        Shape = Exactly(2)
        Scope = PartOnly
        Description =
            "Dictate forward FWD or reverse REV primer position relative to first base of a short inline slice"
        InvertsTo = None
        Validate = noValidate }
      { Name = "primermax"
        Shape = One
        Scope = BlockOnly(Persistent)
        Description = "Max length of primer that can be designed."
        InvertsTo = None
        Validate = parseInt }
      { Name = "primermin"
        Shape = One
        Scope = BlockOnly(Persistent)
        Description = "Max length of primer that can be designed."
        InvertsTo = None
        Validate = parseInt }
      { Name = "pcrparams"
        Shape = Range(1, 5)
        Scope = BlockOnly(Persistent)
        Description = "Set various parts of PCR conditions."
        InvertsTo = None
        Validate = validatePcrParams }
      { Name = "targettm"
        Shape = One
        Scope = BlockOnly(Persistent)
        Description = "Set target melting temperature for pcr designs."
        InvertsTo = None
        Validate = parseDouble }
      { Name = "seamlesstm"
        Shape = One
        Scope = BlockOnly(Persistent)
        Description =
            "Set target melting temperature for seamless designs (body of primer that amplifies the two pieces adjacent to the junction)."
        InvertsTo = None
        Validate = parseDouble }
      { Name = "seamlessoverlaptm"
        Shape = One
        Scope = BlockOnly(Persistent)
        Description =
            "Set target melting temperature for the tail of the seamless primers that overlap in the middle to form the junction."
        InvertsTo = None
        Validate = parseDouble }
      { Name = "atpenalty"
        Shape = One
        Scope = BlockOnly(Persistent)
        Description = "Set degree of tm flexibility to get a terminal G or C and unstable 3' end of an oligo."
        InvertsTo = None
        Validate = parseDouble }
      { Name = "pcrassemblyparams"
        Shape = Range(1, 5)
        Scope = BlockOnly(Persistent)
        Description = "Set melting conditions for the overlap junction in a seamless design."
        InvertsTo = None
        Validate = validatePcrParams }
      { Name = "minoverlaplen"
        Shape = One
        Scope = BlockOnly(Persistent)
        Description = "Sets the minimum overlap length for junction in a seamless design."
        InvertsTo = None
        Validate = parseInt }
      { Name = "len"
        Shape = One
        Scope = PartOnly
        Description = "Set specific heterology block length for a ~ part."
        InvertsTo = None
        Validate = parseInt }
      { Name = "user"
        Shape = One
        Scope = BlockOnly(Persistent)
        Description = "Set owner for any output fields with a creator field"
        InvertsTo = None
        Validate = noValidate }
      { Name = "style"
        Shape = One
        Scope = PartOnly
        Description = "Set allele swap style."
        InvertsTo = None
        Validate = noValidate }
      { Name = "breed"
        Shape = One
        Scope = PartOnly
        Description = "Specify RYSE breed for a specific rabit, overriding any breed inference."
        InvertsTo = None
        Validate = noValidate }
      { Name = "inline"
        Shape = Zero
        Scope = PartOnly
        Description =
            "Force long inline sequences to be created inline as part of a 2 piece rabit regardless of length."
        InvertsTo = None
        Validate = noValidate }
      { Name = "seed"
        Shape = One
        Scope = BlockOrPart(Persistent)
        Description = "Sets the seed for the random number generator for things like codon optimization."
        InvertsTo = None
        Validate = parseInt }
      { Name = "codonopt"
        Shape = AtLeast 1
        Scope = BlockOnly(Persistent)
        Description = "Set codon optimization parameters."
        InvertsTo = None
        Validate = noValidate }
      { Name = "uri"
        Shape = One
        Scope = BlockOrPart(Transient)
        Description = "Tag a part or assembly with a URI."
        InvertsTo = None
        Validate = noValidate }
      { Name = "swapend"
        Shape = One
        Scope = PartOnly
        Description = "State an end preference for an allele swap. Arg should be '3' or '5'."
        InvertsTo = None
        Validate = parseInt }
      { Name = "promlen"
        Shape = One
        Scope = BlockOnly(Persistent)
        Description = "preferred promoter length - overrides genome or system default."
        InvertsTo = None
        Validate = parseInt }
      { Name = "termlen"
        Shape = One
        Scope = BlockOnly(Persistent)
        Description = "preferred terminator length - overrides genome or system default."
        InvertsTo = None
        Validate = parseInt }
      { Name = "termlenmrna"
        Shape = One
        Scope = BlockOnly(Persistent)
        Description = "preferred terminator region length when part of mRNA part- overrides genome or system default."
        InvertsTo = None
        Validate = parseInt }
      topologyPragmaDef ]




type PragmaCache(pragmas: Map<string, PragmaDefinition>) =
    member this.Pragmas with get() = pragmas
    
module PragmaCache =
    /// Check that a pragma inverts to a legal pragma.  Returns the pragma it inverts to or raises an exception.
    let inverts  (pragmaDefinition: PragmaDefinition) (cache: PragmaCache): PragmaDefinition option =
        match pragmaDefinition.InvertsTo with
        | None -> None
        | Some name ->
            match cache.Pragmas.TryFind name with
            | None -> failwithf "Pragma %s inverts to an unknown pragma %s" (pragmaDefinition.Name) name
            | Some result -> // inverts to a known pragma, make sure they have the same shape
                if pragmaDefinition.Shape <> result.Shape
                then failwithf "Pragma %s inverts to %s but they have differing argShapes." (pragmaDefinition.Name) (result.Name)

                Some result    
    
    let create (pragmas: PragmaDefinition list): PragmaCache =
        let pragmaDefinitions =
            pragmas
            |> List.distinctBy LanguagePrimitives.PhysicalHash

        let pragsByName =
            pragmaDefinitions
            |> List.map (fun pragma -> pragma.Name, pragma)
            |> Map.ofList

        // Idiot check that we don't have any duplicate pragmas.
        if pragsByName.Count <> pragmaDefinitions.Length then
            failwithf
                "%d pragmas were defined but size of legalPragmas map is only %d. Name aliases?"
                (pragmaDefinitions.Length)
                (pragsByName.Count)

        let cache = PragmaCache(pragsByName)
        // Make sure any pragmas that invert do it sensibly.
        // Raises an exception if any one doesn't validate.
        for pragmaDefinition in pragmaDefinitions do
            cache |> inverts pragmaDefinition |> ignore
        cache
    
    let createWithBuiltinPragmas (pragmas: PragmaDefinition list) =
        pragmas @ pragmaDefsStatic
        |> create
        
    let builtin =
        pragmaDefsStatic
        |> create        
    
/// Instance of a pragma directive.
[<CustomEquality; CustomComparison>]
type Pragma =
    { Definition: PragmaDefinition
      Arguments: string list }
    member this.name = this.Definition.Name

    member this.IsTransient =
        match this.Definition.Scope with
        | BlockOnly (Persistent)
        | BlockOrPart (Persistent)
        | BlockOnly (PersistentCumulative)
        | BlockOrPart (PersistentCumulative) -> false
        | _ -> true
    /// Does this pragma announce the availability of an extension capability?
    member this.SetsCapability =
        if this.Definition = capaPragmaDef then Some(this.Arguments.[0].ToLower()) else None
    /// Is this pragma a warning message?
    member this.IsWarning = this.Definition = warningPragmaDef
    /// Is this pragma a flag to deactivate a warning?
    member this.IgnoresWarning =
        if this.Definition = warnoffPragmaDef then Some(this.Arguments.[0]) else None
    /// Is this pragma a #capa directive?
    member this.IsCapa = this.Definition = capaPragmaDef
    /// Helper function to check the list of args for a particular value.
    member this.HasVal(value: string) = List.contains value this.Arguments
    /// Only consider pragma name and args in comparions.
    override this.Equals(obj) =
        match obj with
        | :? Pragma as other -> (this.name = other.name && this.Arguments = other.Arguments)
        | _ -> false
    /// Hash a Pragma as a combination of pName and args.
    override this.GetHashCode() = hash (this.name, this.Arguments)

    interface IComparable with
        member this.CompareTo obj =
            match obj with
            | :? Pragma as other -> compare (this.name, this.Arguments) (other.name, other.Arguments)
            | _ -> invalidArg "obj" "cannot compare values of different types"

    override this.ToString() =
        sprintf "#%s %s" this.name (String.concat " " this.Arguments)


/// Format a pragma definition.
let formatPragma p =
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

/// Print all available pragmas.
let pragmaUsage (cache: PragmaCache) =
    let orderedPragmas =
        cache.Pragmas
        |> Map.toList
        |> List.sortBy fst // sort the pairs by name
        |> List.map snd // pull out just the sorted pragmas

    for p in orderedPragmas do
        printfn "%s" (formatPragma p)

/// Raise an exception if pName is not among the registered pragmas.
let validatePragmaName  (pragmaName: string) (cache: PragmaCache) =
    if cache.Pragmas |> Map.containsKey pragmaName |> not
    then failwithf "Requested unknown pragma '#%s'." pragmaName

/// Validated pragma construction during parsing
let buildPragmaFromDef (pDef: PragmaDefinition) (values: string list) =
    let name = pDef.Name

    // check that the right number of arguments were supplied
    let nArg = values.Length

    let checkNArgs n =
        if nArg <> n
        then fail (sprintf "Pragma #%s expected %d argument(s) but got %d: %A" name n nArg values)
        else ok ()

    let checkMinArgs min =
        if nArg < min
        then fail (sprintf "Pragma #%s expected at least %d argument(s) but got %d: %A" name min nArg values)
        else ok ()

    let checkMaxArgs max _ =
        if nArg > max
        then fail (sprintf "Pragma #%s expected at most %d argument(s) but got %d: %A" name max nArg values)
        else ok ()

    let checkArgShape () =
        match pDef.Shape with
        | Zero -> checkNArgs 0
        | One -> checkNArgs 1
        | Exactly (n) -> checkNArgs n
        | AtLeast (n) -> checkMinArgs n
        | Range (min, max) -> checkMinArgs min >>= checkMaxArgs max
        | ExactlySet (vals) ->
            if not (vals |> List.contains nArg) then
                fail
                    (sprintf
                        "Pragma %s expected any number of arguments in the set %A but got %d: %A"
                         name
                         vals
                         nArg
                         values)
            else
                ok ()

    let validateArgs () = pDef.Validate values

    // validation pipeline
    checkArgShape ()
    >>= validateArgs
    >>= (fun () ->
        ok
            { Definition = pDef
              Arguments = values })

/// Try to build a pragma from a name and values.
let buildPragma (name: string) (values: string list) (cache: PragmaCache): Result<Pragma, string> =
    // try to get the pragma defintion
    match cache.Pragmas |> Map.tryFind name with
    | Some pDef -> buildPragmaFromDef pDef values
    | None -> fail (sprintf "Unknown or invalid pragma: '#%s'" name)


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
    /// Add a Pragma to this collection.
    member this.Add(pragma: Pragma) =
        let pragmas =
            (match pragma.Definition.Scope with
             | BlockOnly (PersistentCumulative)
             | BlockOrPart (PersistentCumulative)
             | BlockOnly (TransientCumulative)
             | BlockOrPart (TransientCumulative) ->
                 match this.Pragmas.TryFind(pragma.name) with
                 | None -> this.Pragmas.Add(pragma.name, pragma)
                 | Some (existing) ->
                     let newArgs = existing.Arguments @ pragma.Arguments // new args go on the end

                     match buildPragmaFromDef existing.Definition newArgs with
                     | Ok (newPragma, _messages) -> this.Pragmas.Add(pragma.name, newPragma)
                     | Bad messages -> failwithf "%s" (String.Join(";", messages))
             | _ -> this.Pragmas.Add(pragma.name, pragma))
        { this with Pragmas = pragmas }
    /// Add a pragma to this collection using string name.
    member x.Add(pName: string) = x.Add(pName, [])
    /// Add a pragma to this collection using string name and single value.
    member x.Add(pName: string, value: string) = x.Add(pName, [ value ])
    /// Add a pragma to this collection using string name and values.
    member x.Add(pName: string, values: string list) =
        x.Cache |> buildPragma pName values
        >>= (fun p -> ok (x.Add(p)))
    /// Remove a pragma from this collection.
    member this.Remove(name: string) =
        { this with Pragmas = this.Pragmas |> Map.remove name }
    /// Remove a pragma from this collection.
    member x.Remove(pDef: PragmaDefinition) = x.Remove(pDef.Name)
    /// Remove a pragma from this collection.
    member x.Remove(p: Pragma) = x.Remove(p.name)
    /// Merge a list of Pragmas into this collection.
    /// The incoming pragmas will clobber any pragmas set in this collection.
    member this.MergeIn(incoming: Pragma list) =
        let newPragmas =
            incoming
            |> List.fold (fun (pc: Map<string, Pragma>) prag -> pc.Add(prag.name, prag)) this.Pragmas
        { this with Pragmas = newPragmas }
    /// Merge another PragmaCollection into this one.
    /// The incoming pragmas will clobber any pragmas set in this collection.
    member this.MergeIn(incoming: PragmaCollection) =
        let newPragmas =
            incoming.Pragmas
            |> Map.fold (fun (pc: Map<string, Pragma>) name prag -> pc.Add(name, prag)) this.Pragmas
        { this with Pragmas = newPragmas }
    /// Has a pragma been set?
    /// Raises an exception if pName is not a registered pragma.
    member this.ContainsKey(pName: string) =
        this.Cache |> validatePragmaName pName
        this.Pragmas |> Map.containsKey pName
    /// Has a pragma been set?
    member this.ContainsKey(pDef: PragmaDefinition) = this.Pragmas |> Map.containsKey pDef.Name
    /// Has a pragma been set?
    member x.ContainsKey(p: Pragma) = x.Pragmas |> Map.containsKey p.name
    /// Get the values associated with a pragma.
    /// Raises an exception is pName is not a registered pragma.
    member this.TryGetValues(pName: string) =
        this.Cache |> validatePragmaName pName

        match this.Pragmas |> Map.tryFind pName with
        | Some (p) -> Some(p.Arguments)
        | None -> None
    /// Get a single value associated with a pragma, ignoring any extras.
    /// Raises an exception is pName is not a registered pragma.
    member this.TryGetOne(pName: string) =
        this.Cache |> validatePragmaName pName

        match this.TryGetValues pName with
        | Some (v :: _) -> Some(v)
        | None
        | Some ([]) -> None
    /// Get a pragma.
    /// Raises an exception is pName is not a registered pragma.
    member this.TryFind(pName: string) =
        this.Cache |> validatePragmaName pName
        this.Pragmas.TryFind pName
    /// Get a pragma by definition.
    member x.TryFind(pDef: PragmaDefinition) = x.Pragmas.TryFind pDef.Name

    member x.Names =
        x.Pragmas |> Map.toSeq |> Seq.map fst |> Set.ofSeq

    member x.Values = x.Pragmas |> Map.toSeq |> Seq.map snd
    member x.IsEmpty = x.Pragmas.IsEmpty
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

let createPragmaCollection (pragmas: seq<Pragma>) (cache: PragmaCache) =
    let pragmas =
        pragmas
        |> Seq.map (fun p -> p.name, p)
        |> Map.ofSeq
    { PragmaCollection.Cache = cache
      Pragmas = pragmas }
    

let EmptyPragmas = { PragmaCollection.Pragmas = Map.empty; Cache = PragmaCache.builtin }

/// Determine the current assembly mode from pragma collection.
let assemblyMode (pc: PragmaCollection) =
    match pc.TryFind("platform") with
    | Some (p) -> parsePlatform p.Arguments |> returnOrFail
    | None -> Megastitch

// ======================
// pragma deprecations and deprecation machinery
// ======================

type PragmaDeprecation =
    { name: string
      replacement: string
      replace: Pragma -> Pragma
      extraMessage: string option }
    member x.WarningMessage =
        let msg =
            sprintf "The pragma #%s is deprecated; please use #%s instead." x.name x.replacement

        match x.extraMessage with
        | Some (m) -> sprintf "%s\n%s" msg m
        | None -> msg

let private replaceStaticPlatformPragma which _ =
    { Definition = platformPragmaDef
      Arguments = [ which ] }

let private stitchPragmaDeprecation =
    { name = "stitch"
      replacement = "platform"
      replace = replaceStaticPlatformPragma "stitch"
      extraMessage = Some("This pragma will be interpreted as '#platform stitch'.") }

let private megastitchPragmaDeprecation =
    { name = "megastitch"
      replacement = "platform"
      replace = replaceStaticPlatformPragma "megastitch"
      extraMessage = Some("This pragma will be interpreted as '#platform megastitch'.") }

let DeprecatedPragmas =
    [ stitchPragmaDeprecation
      megastitchPragmaDeprecation ]
    |> Seq.map (fun pd -> pd.name, pd)
    |> Map.ofSeq
