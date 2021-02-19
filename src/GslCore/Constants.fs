/// Numeric and string constants, as well as some broadly-needed domain types.
namespace GslCore.Constants

open Amyris.Bio.primercore

module Default =
    [<Literal>]
    let RyseLinkerTargetTemp = 60.0<C> // melting temp for linker overhang melting temp

    [<Literal>]
    let SeamlessTargetTemp = 68.0<C> // higher temp for seamless junctions

    [<Literal>]
    let SeamlessOverlapTargetTemp = 68.0<C> // Temperature for internal overlap region of a seamless junction

    [<Literal>]
    let MinOverlapLength = 24

    [<Literal>]
    let ApproxMargin = 50

    [<Literal>]
    let PrimerMaxLength = 60

    [<Literal>]
    let PrimerMinLength = 20

    [<Literal>]
    let NameMaxLength = 600 // see also thumper restriction in thumper.fs

    [<Literal>]
    let ThumperMaxOligoLength = 80

    [<Literal>]
    let FlankLength = 500
    /// Terminator length default value (can be overriden in genome def)
    [<Literal>]
    let TerminatorLength = 500

    /// Terminator length default value when part of an 'm' mRNA part (can be overriden in genome def)
    [<Literal>]
    let MRNATerminatorLength = 200

    /// Promoter length default value (can be overriden in genome def)
    [<Literal>]
    let PromoterLength = 500

    /// Classic allele swap codon replacement min frequency
    [<Literal>]
    let MinHBCodonUsage = 0.05

    [<Literal>]
    let RefGenome = "cenpk"

    /// Amount of extra dna adjacent to the ORF to include
    [<Literal>]
    let OrfPlusMargin = 100
    
    let maxPhase2Passes = Some(10)

    let ValidAminoAcids: Set<char> = "ACDEFGHIKLMNPQRSTVWY*" |> Set.ofSeq

    /// List of approved linker abbreviations.
    let ValidLinkers: Set<string> =
        [ '0' .. '9' ] @ [ 'A' .. 'E' ]
        |> List.map (sprintf "%c")
        |> Set.ofSeq

[<Measure>]
type OneOffset

[<Measure>]
type ZeroOffset

module ZeroOffset =
    /// Drop the units from a ZeroOffset int.
    let toInt (input: int<ZeroOffset>): int = input / 1<ZeroOffset>

    /// Convert a ZeroOffset int into a OneOffset int.
    let toOne (input: int<ZeroOffset>): int<OneOffset> =
        let unitless = toInt input

        if unitless >= 0 then unitless + 1 else unitless
        * 1<OneOffset>

module OneOffset =
    /// Convert a OneOffset int into a ZeroOffset int.
    let toZero (input: int<OneOffset>): int<ZeroOffset> =
        match input / 1<OneOffset> with
        | 0 -> 0
        | x when x > 0 -> (x - 1)
        | x -> x
        * 1<ZeroOffset>

type GeneEnd =
    | FivePrime
    | ThreePrime

type RelPos =
    { Position: int<OneOffset>
      RelativeTo: GeneEnd }


[<Measure>]
type PluginScore

/// Domain wrapper type to indicate that a string represents literal GSL source.
type GslSourceCode =
    | GslSourceCode of string
    member this.String =
        let (GslSourceCode (s)) = this
        s

    override this.ToString() = this.String
