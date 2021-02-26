namespace GslCore.Pragma

open System
open FsToolkit.ErrorHandling
open GslCore.GslResult
open GslCore.PcrParamParse


module Parse =
    // Helper functions for generic validation of simple parameters.
    let parseNumber parseFunc kind (args: string list): GslResult<unit, string> =
        match args with
        | [ i ] ->
            match parseFunc i with
            | true, _ -> GslResult.ok ()
            | false, _ -> GslResult.err (sprintf "Could not parse %s as an %s." i kind)
        // We shouldn't ever hit this clause as we should have already blown up with
        // a different error.
        | x -> GslResult.err (sprintf "parse number expected one argument but got %d" x.Length)

    let parseInt = parseNumber (Int64.TryParse) "int"
    let parseDouble = parseNumber (Double.TryParse) "float"

module Validation =
    /// Pass-through placeholder validator.
    let noValidate _ = GslResult.ok ()

    let validatePcrParams (args: string list) =
        args
        |> List.map PcrParameterParser.parseArg
        |> GslResult.collectA
        |> GslResult.ignore
        |> GslResult.mapError PcrParameterParseError.toString


type Platform =
    | Stitch
    | Megastitch

module Platform =
    let parsePlatform: string list -> GslResult<Platform, string> =
        function
        | [ singleItem ] ->
            match singleItem with
            | "stitch" -> GslResult.ok Stitch
            | "megastitch" -> GslResult.ok Megastitch
            | _ -> GslResult.err (sprintf "Invalid platform '%s'.  Options are 'stitch' and 'megastitch'." singleItem)
        // We shouldn't ever hit this clause as we should have already blown up with
        // a different error.
        | x -> GslResult.err (sprintf "platform expected one argument but got %d" x.Length)

/// Represents topology information about constructs
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

    let parse: string list -> GslResult<Topology, string> =
        function
        | [ arg ] ->
            match arg with
            | LinearValue -> GslResult.ok Linear
            | CircularValue -> GslResult.ok Circular
            | _ -> GslResult.err (sprintf "Invalid topology '%s'.  Options are 'linear' and 'circular'." arg)
        | x -> GslResult.err (sprintf "topology expected one argument but got %d" x.Length)

    let toString: Topology -> string =
        function
        | Linear -> "linear"
        | Circular -> "circular"

// Pragma defs that are used internally by the compiler.
module BuiltIn =
    let warningPragmaDef =
        { PragmaDefinition.Name = "warn"
          Shape = AtLeast(1)
          Scope = BlockOrPart(Transient)
          Description = "Print a warning message."
          InvertsTo = None
          Validate = Validation.noValidate }

    let warnoffPragmaDef =
        { PragmaDefinition.Name = "warnoff"
          Shape = One
          Scope = BlockOnly(Persistent)
          Description = "Turn off specific warnings."
          InvertsTo = None
          Validate = Validation.noValidate }

    let capaPragmaDef =
        { PragmaDefinition.Name = "capa"
          Shape = One
          Scope = BlockOnly(Persistent)
          Description = "Enables particular compiler capabilities."
          InvertsTo = None
          Validate = Validation.noValidate }

    let platformPragmaDef =
        { PragmaDefinition.Name = "platform"
          Shape = One
          Scope = BlockOnly(Persistent)
          Description = "Specify an assembly platform to target, current options: 'stitch', 'megastitch'."
          InvertsTo = None
          Validate = Platform.parsePlatform >> GslResult.ignore }

    let markersetPragmaDef =
        { PragmaDefinition.Name = "markerset"
          Shape = One
          Scope = BlockOnly(Persistent)
          Description = "Set the default marker set for a ### part."
          InvertsTo = None
          Validate = Validation.noValidate }

    let namePragmaDef =
        { PragmaDefinition.Name = "name"
          Shape = One
          Scope = BlockOrPart(Transient)
          Description = "Override name for a assembly or part."
          InvertsTo = None
          Validate = Validation.noValidate }

    let fusePragmaDef =
        { PragmaDefinition.Name = "fuse"
          Shape = Zero
          Scope = PartOnly
          Description = "Create a seamless junction with the next part."
          InvertsTo = None
          Validate = Validation.noValidate }

    let ampPragmaDef =
        { PragmaDefinition.Name = "amp"
          Shape = Zero
          Scope = PartOnly
          Description = "Create a seamless junction with the next part."
          InvertsTo = None
          Validate = Validation.noValidate }

    let topologyPragmaDef =
        { PragmaDefinition.Name = Topology.PragmaName
          Shape = One
          Scope = BlockOnly(Persistent)
          Description = "The design has either linear or circular topology"
          InvertsTo = None
          Validate = Topology.parse >> GslResult.ignore }

    let linkersPragmaDef =
        { PragmaDefinition.Name = "linkers"
          Shape = AtLeast(1)
          Scope = BlockOnly(Persistent)
          Description = "Override the default set of RYSE linkers."
          InvertsTo = None
          Validate = Validation.noValidate }

    let refGenomePragmaDef =
        { PragmaDefinition.Name = "refgenome"
          Shape = One
          Scope = BlockOrPart(Persistent)
          Description = "Specify a reference genome."
          InvertsTo = None
          Validate = Validation.noValidate }

    let dnaSrcPragmaDef =
        { PragmaDefinition.Name = "dnasrc"
          Shape = One
          Scope = PartOnly
          Description = "Specify a DNA source for a part."
          InvertsTo = None
          Validate = Validation.noValidate }

    [<Literal>]
    let StitchName = "stitch"

    let stitchPragmaDef =
        { PragmaDefinition.Name = StitchName
          Shape = Zero
          Scope = BlockOnly(Persistent)
          Description = "TODO description"
          InvertsTo = None
          Validate = Validation.noValidate }

    [<Literal>]
    let MegaStitchName = "megastitch"

    let megastitchPragmaDef =
        { PragmaDefinition.Name = MegaStitchName
          Shape = Zero
          Scope = BlockOnly(Persistent)
          Description = "TODO description"
          InvertsTo = None
          Validate = Validation.noValidate }

    let rabitEndPragmaDef =
        { PragmaDefinition.Name = "rabitend"
          Shape = Zero
          Scope = PartOnly
          Description = "Designate part as the end of a RYSE rabit."
          InvertsTo = Some("rabitstart")
          Validate = Validation.noValidate }

    let rabitStartPragmaDef =
        { PragmaDefinition.Name = "rabitstart"
          Shape = Zero
          Scope = PartOnly
          Description = "Designate part as the start of a RYSE rabit."
          InvertsTo = Some("rabitend")
          Validate = Validation.noValidate }

    let primerPosPragmaDef =
        { PragmaDefinition.Name = "primerpos"
          Shape = Exactly(2)
          Scope = PartOnly
          Description =
              "Dictate forward FWD or reverse REV primer position relative to first base of a short inline slice"
          InvertsTo = None
          Validate = Validation.noValidate }

    let primerMaxPragmaDef =
        { PragmaDefinition.Name = "primermax"
          Shape = One
          Scope = BlockOnly(Persistent)
          Description = "Max length of primer that can be designed."
          InvertsTo = None
          Validate = Parse.parseInt >> GslResult.ignore }

    let primerMinPragmaDef =
        { PragmaDefinition.Name = "primermin"
          Shape = One
          Scope = BlockOnly(Persistent)
          Description = "Max length of primer that can be designed."
          InvertsTo = None
          Validate = Parse.parseInt }

    [<Literal>]
    let PcrParamsName = "pcrparams"

    let pcrParamsPragmaDef =
        { PragmaDefinition.Name = PcrParamsName
          Shape = Range(1, 5)
          Scope = BlockOnly(Persistent)
          Description = "Set various parts of PCR conditions."
          InvertsTo = None
          Validate = Validation.validatePcrParams }

    [<Literal>]
    let TargetTmName = "targettm"

    let targetTmPragmaDef =
        { PragmaDefinition.Name = TargetTmName
          Shape = One
          Scope = BlockOnly(Persistent)
          Description = "Set target melting temperature for pcr designs."
          InvertsTo = None
          Validate = Parse.parseDouble }

    let seamlessTmPragmaDef =
        { PragmaDefinition.Name = "seamlesstm"
          Shape = One
          Scope = BlockOnly(Persistent)
          Description =
              "Set target melting temperature for seamless designs (body of primer that amplifies the two pieces adjacent to the junction)."
          InvertsTo = None
          Validate = Parse.parseDouble }

    [<Literal>]
    let SeamlesOverlapTmName = "seamlessoverlaptm"

    let seamlessOverlapTmPragmaDef =
        { PragmaDefinition.Name = SeamlesOverlapTmName
          Shape = One
          Scope = BlockOnly(Persistent)
          Description =
              "Set target melting temperature for the tail of the seamless primers that overlap in the middle to form the junction."
          InvertsTo = None
          Validate = Parse.parseDouble }

    [<Literal>]
    let AtPenaltyName = "atpenalty"

    let atPenaltyPragmaDef =
        { PragmaDefinition.Name = AtPenaltyName
          Shape = One
          Scope = BlockOnly(Persistent)
          Description = "Set degree of tm flexibility to get a terminal G or C and unstable 3' end of an oligo."
          InvertsTo = None
          Validate = Parse.parseDouble }

    [<Literal>]
    let PcrAssemblyParamsName = "pcrassemblyparams"

    let pcrAssemblyParamsPragmaDef =
        { PragmaDefinition.Name = PcrAssemblyParamsName
          Shape = Range(1, 5)
          Scope = BlockOnly(Persistent)
          Description = "Set melting conditions for the overlap junction in a seamless design."
          InvertsTo = None
          Validate = Validation.validatePcrParams }

    [<Literal>]
    let MinOverlapLenName = "minoverlaplen"

    let minOverlapLenPragmaDef =
        { PragmaDefinition.Name = MinOverlapLenName
          Shape = One
          Scope = BlockOnly(Persistent)
          Description = "Sets the minimum overlap length for junction in a seamless design."
          InvertsTo = None
          Validate = Parse.parseInt }

    let lenPragmaDef =
        { PragmaDefinition.Name = "len"
          Shape = One
          Scope = PartOnly
          Description = "Set specific heterology block length for a ~ part."
          InvertsTo = None
          Validate = Parse.parseInt }

    let userPragmaDef =
        { PragmaDefinition.Name = "user"
          Shape = One
          Scope = BlockOnly(Persistent)
          Description = "Set owner for any output fields with a creator field"
          InvertsTo = None
          Validate = Validation.noValidate }

    let stylePragmaDef =
        { PragmaDefinition.Name = "style"
          Shape = One
          Scope = PartOnly
          Description = "Set allele swap style."
          InvertsTo = None
          Validate = Validation.noValidate }

    let breedPragmaDef =
        { PragmaDefinition.Name = "breed"
          Shape = One
          Scope = PartOnly
          Description = "Specify RYSE breed for a specific rabit, overriding any breed inference."
          InvertsTo = None
          Validate = Validation.noValidate }

    let inlinePragmaDef =
        { PragmaDefinition.Name = "inline"
          Shape = Zero
          Scope = PartOnly
          Description =
              "Force long inline sequences to be created inline as part of a 2 piece rabit regardless of length."
          InvertsTo = None
          Validate = Validation.noValidate }

    let seedPragmaDef =
        { PragmaDefinition.Name = "seed"
          Shape = One
          Scope = BlockOrPart(Persistent)
          Description = "Sets the seed for the random number generator for things like codon optimization."
          InvertsTo = None
          Validate = Parse.parseInt }

    let codonOptPragmaDef =
        { PragmaDefinition.Name = "codonopt"
          Shape = AtLeast 1
          Scope = BlockOnly(Persistent)
          Description = "Set codon optimization parameters."
          InvertsTo = None
          Validate = Validation.noValidate }

    let uriPragmaDef =
        { PragmaDefinition.Name = "uri"
          Shape = One
          Scope = BlockOrPart(Transient)
          Description = "Tag a part or assembly with a URI."
          InvertsTo = None
          Validate = Validation.noValidate }

    let swapEndPragmaDef =
        { PragmaDefinition.Name = "swapend"
          Shape = One
          Scope = PartOnly
          Description = "State an end preference for an allele swap. Arg should be '3' or '5'."
          InvertsTo = None
          Validate = Parse.parseInt }

    let promLenPragmaDef =
        { PragmaDefinition.Name = "promlen"
          Shape = One
          Scope = BlockOnly(Persistent)
          Description = "preferred promoter length - overrides genome or system default."
          InvertsTo = None
          Validate = Parse.parseInt }

    let termLenPragmaDef =
        { PragmaDefinition.Name = "termlen"
          Shape = One
          Scope = BlockOnly(Persistent)
          Description = "preferred terminator length - overrides genome or system default."
          InvertsTo = None
          Validate = Parse.parseInt }

    let termLenMrnaPragmaDef =
        { PragmaDefinition.Name = "termlenmrna"
          Shape = One
          Scope = BlockOnly(Persistent)
          Description = "preferred terminator region length when part of mRNA part- overrides genome or system default."
          InvertsTo = None
          Validate = Parse.parseInt }

    /// Base set of hard coded pragmas.  Plugins might augment this list
    let all: PragmaDefinition list =
        [ warningPragmaDef
          warnoffPragmaDef
          capaPragmaDef
          platformPragmaDef
          markersetPragmaDef
          namePragmaDef
          fusePragmaDef
          ampPragmaDef
          linkersPragmaDef
          refGenomePragmaDef
          dnaSrcPragmaDef
          stitchPragmaDef
          megastitchPragmaDef
          rabitStartPragmaDef
          rabitEndPragmaDef
          primerPosPragmaDef
          primerMaxPragmaDef
          primerMinPragmaDef
          pcrParamsPragmaDef
          targetTmPragmaDef
          seamlessTmPragmaDef
          seamlessOverlapTmPragmaDef
          atPenaltyPragmaDef
          pcrAssemblyParamsPragmaDef
          minOverlapLenPragmaDef
          lenPragmaDef
          userPragmaDef
          stylePragmaDef
          breedPragmaDef
          inlinePragmaDef
          seedPragmaDef
          codonOptPragmaDef
          uriPragmaDef
          swapEndPragmaDef
          promLenPragmaDef
          termLenPragmaDef
          termLenMrnaPragmaDef
          topologyPragmaDef ]
