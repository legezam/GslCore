namespace GslCore.Core.Types

open System
open FsToolkit.ErrorHandling
open GslCore.Pragma
open GslCore.Constants
open GslCore.DesignParams
open GslCore.Uri
open GslCore.Legacy.Types
open GslCore.Legacy
open Amyris.Dna
open GslCore.Ast.Types

type SequenceLibrary = Map<string, Dna>

/// Instructions gleaned from command line
type ParsedOptions =
    { Quiet: bool
      RefStrain: string
      LibDir: string
      Iter: bool
      OnlyPhase1: bool
      DoParallel: bool
      Verbose: bool
      NoPrimers: bool
      LexOnly: bool
      RefList: bool
      RefDump: string option
      ListPlugins: bool
      DoHelpPragmas: bool
      IsDeterministic: bool }

type DNAIntervalType =
    | Anneal
    | RyseLinker
    | AMP
    | Sandwich

type DNAInterval =
    { Left: int
      Right: int
      Type: DNAIntervalType }

[<RequireQualifiedAccess>]
type Breed =
    | Promoter
    | Terminator
    | Marker
    | FusableOrf
    | Upstream
    | Downstream
    | GST
    | GS
    | Inline
    | X
    | Virtual
    | Linker

/// Used in the grammar of GSL to pick a standard part of a gene (p, t, o etc)
[<RequireQualifiedAccess>]
type StandardSlice =
    | Gene
    | Promoter
    | Terminator
    | Orf
    | FusableOrf
    | Upstream
    | Downstream
    | MRNA (* ORF + term *)

module StandardSlice =
    let sliceTypeChars =
        [ 'p', 'u', 't', 'd', 'o', 'f', 'g', 'm' ]

    let charToSliceType c =
        match Char.ToLower c with
        | 'p' -> Some(StandardSlice.Promoter)
        | 'u' -> Some(StandardSlice.Upstream)
        | 't' -> Some(StandardSlice.Terminator)
        | 'd' -> Some(StandardSlice.Downstream)
        | 'o' -> Some(StandardSlice.Orf)
        | 'f' -> Some(StandardSlice.FusableOrf)
        | 'g' -> Some(StandardSlice.Gene)
        | 'm' -> Some(StandardSlice.MRNA)
        | _ -> None

[<RequireQualifiedAccess>]
type SliceType =
    | Regular
    | Marker
    | Linker
    | Inline
    | Fusion

module SliceType =
    let toString: SliceType -> string =
        function
        | SliceType.Regular -> "REG"
        | SliceType.Marker -> "MARKER"
        | SliceType.Linker -> "LINKER"
        | SliceType.Inline -> "INLINE"
        | SliceType.Fusion -> "FUSION"

/// Due to slices or other considerations, Orfs may not exactly align with the codon sequence.
/// Indicate which position in the first codon is represented by the first base in the orf.
type OrfOffset =
    | Zero
    | One
    | Two

module OrfOffset =
    /// Given a slice offset from the start of a gene's ORF, determine what offset the first allele
    /// in the resulting sequence will have.
    let orfOffsetFromAlleleOffset (offset: int<ZeroOffset>) =
        match (offset / 1<ZeroOffset>) % 3 with
        | 0 -> Zero
        | 1
        | -2 -> One
        | 2
        | -1 -> Two
        | _ -> Zero // this case is unreachable

/// Slice annotation for indicating the presence of an ORF in a slice.
type OrfAnnotation =
    /// The leftmost base pair of this ORF.
    { Left: int<ZeroOffset>
      /// The rightmost base pair of this ORF, inclusive.
      Right: int<ZeroOffset>
      /// Is the first base of this ORF offset into a codon?
      /// This field should be interpreted in the context of direction,
      /// as it applies to the leftmost base in a fwd Orf vs. the rightmost base in a rev Orf.
      FrameOffset: OrfOffset
      /// Is this ORF on the fwd or reverse direction relative to this slice?
      IsForward: bool }
    /// Return a sequence of the starting indices of every complete codon described by this annotation.
    /// If the Orf is fwd, these will be in increasing order; if rev, decreasing order.
    member x.CompleteCodonIndices() =
        let left, right =
            ZeroOffset.toInt x.Left, ZeroOffset.toInt x.Right
        // the number of bases we need to move inwards from the edge to find the start of the first codon
        let alleleOffset =
            match x.FrameOffset with
            | Zero -> 0
            | One -> 2
            | Two -> 1
        // compute the number of complete codons based on the size of the ORF and the offset
        let nCodons =
            let nBases = right - left + 1 - alleleOffset
            nBases / 3

        let codonOffsets = seq { 0 .. 3 .. (nCodons - 1) * 3 }

        if x.IsForward then
            let firstCodon = left + alleleOffset

            codonOffsets
            |> Seq.map (fun offset -> (firstCodon + offset) * 1<ZeroOffset>)
        else
            let firstCodon = right - alleleOffset

            codonOffsets
            |> Seq.map (fun offset -> (firstCodon - offset) * 1<ZeroOffset>)

module OrfAnnotation =
    /// Create an ORF annotation from a slice on gene-relative coordiantes.
    let orfAnnotationFromSlice (slice: Slice) (orfLen: int) fwd context =
        let sliceStart, sliceEnd =
            
            LegacySliceContext.getBoundsFromSlice slice orfLen context
            |> Result.valueOr failwith
            |> (fun (l, r) -> OneOffset.toZero l, OneOffset.toZero r)

        // compute the actual length of the slice
        let sliceLen =
            ZeroOffset.toInt (sliceEnd - sliceStart) + 1

        // based on the gene-relative slice coordinates, determine the frame offset of this ORF.
        let frameOffset =
            OrfOffset.orfOffsetFromAlleleOffset sliceStart

        // if left is less than 0, then the ORF starts at a positive offset
        // these coordinates are now relative to the materialized DNA slice
        let orfStart = -1 * sliceStart // slice coordinates are relative to ORF start at 5' end

        let orfEnd =
            orfStart + orfLen * 1<ZeroOffset> - 1<ZeroOffset>

        // if the part is reversed, the start and end need to be flipped to be relative to
        // the opposite ends
        let left, right =
            if fwd then
                orfStart, orfEnd
            else
                (orfLen * 1<ZeroOffset>) - orfEnd - 1<ZeroOffset>,
                (orfLen * 1<ZeroOffset>)
                - orfStart
                - 1<ZeroOffset>

        // the slice may be smaller than the ORF in either direction, so make sure left and right
        // are constrained to the interval defined by the length of the slice
        let constrain index =
            index
            |> max 0<ZeroOffset>
            |> min (sliceLen * 1<ZeroOffset> - 1<ZeroOffset>)

        { OrfAnnotation.Left = constrain left
          Right = constrain right
          FrameOffset = frameOffset
          IsForward = fwd }

/// Extensible type to add useful annotations to slices.
type SliceAnnotation = Orf of OrfAnnotation

/// Represents one piece of DNA for assembly, capturing its origins and relevant details
type DNASlice =
    { Id: int option
      ExternalId: string option
      Dna: Dna
      SourceChromosome: string
      SourceFrom: int<ZeroOffset>
      SourceTo: int<ZeroOffset>
      SourceForward: bool
      SourceFromApprox: bool
      SourceToApprox: bool
      DestinationFrom: int<ZeroOffset>
      DestinationTo: int<ZeroOffset>
      DestinationForward: bool
      /// is this slice created by PCR
      IsAmplified: bool
      Template: Dna option
      SliceName: string
      Uri: Uri option
      Description: string
      Type: SliceType
      Pragmas: PragmaCollection
      DnaSource: string
      Breed: Breed
      /// Keep track of the part this slice was materialized from.
      MaterializedFrom: PartPlusPragma option
      Annotations: SliceAnnotation list }
    override this.ToString() =
        sprintf "{ %s - %A - %A }" this.Description this.Type this.Breed

module DNASlice =
    /// Recalculate the offsets of pieces in a list of pieces after new pieces are added in
    let recalculatOffset (pieces: DNASlice list) =
        let lengths =
            pieces
            |> List.map (fun p -> p.Dna.Length * 1<ZeroOffset>)

        let _, offsets' =
            lengths
            |> List.fold (fun (o, r) l -> (o + l, o :: r)) (0 * 1<ZeroOffset>, [])

        let offsets = List.rev offsets'

        List.zip pieces offsets
        |> List.map (fun (p, o) ->
            { p with
                  DestinationFrom = o
                  DestinationTo = o + (p.Dna.Length - 1) * 1<ZeroOffset> })

type DnaAssembly =
    { Id: int option
      DnaParts: DNASlice list
      Name: string
      Uri: Uri option
      LinkerHint: string
      Pragmas: PragmaCollection
      DesignParams: DesignParams
      DocStrings: string list
      MaterializedFrom: Assembly
      Tags: Set<AssemblyTag>
      Topology: Topology }
    member x.Sequence() =
        x.DnaParts
        |> Seq.map (fun p -> p.Dna)
        |> DnaOps.concat

    interface ISourcePosition with
        member x.OptionalSourcePosition = x.MaterializedFrom.SourcePosition

module DnaAssembly =
    /// DNASlice default for a fusion slidetype
    let fusionSliceConstant =
        { Id = None
          ExternalId = None
          SliceName = "fusion"
          Uri = None // TODO: uri for fusion parts?
          Dna = Dna("")
          SourceChromosome = ""
          SourceFrom = 0<ZeroOffset>
          SourceTo = 0<ZeroOffset>
          SourceForward = true
          Template = None
          IsAmplified = false
          SourceFromApprox = false
          SourceToApprox = false
          DestinationFrom = 0<ZeroOffset>
          DestinationTo = 0<ZeroOffset>
          DestinationForward = true
          Description = "::"
          DnaSource = ""
          Type = SliceType.Fusion
          Pragmas = PragmaCollection.empty
          Breed = Breed.Virtual
          MaterializedFrom = None // TODO: should we mark this as associated with this ppp?
          Annotations = [] }

/// Model a primer which diverges and has body/tail parts.
/// The body part anneals to the intended amplification target and the tail
/// hangs out and anneals for stitching purposes
type Primer =
    { Tail: Dna
      Body: Dna
      Annotation: DNAInterval list }
    member x.Primer = DnaOps.append x.Tail x.Body

    member x.lenLE(maxOligo) =
        x.Tail.Length + x.Body.Length <= maxOligo

    /// Try to find an interval of type iType, returns Some/None
    member x.Interval(iType: DNAIntervalType) =
        x.Annotation
        |> List.tryFind (fun i -> i.Type = iType)

/// Divergend pair of Primers
type PrimerPair =
    { Forward: Primer
      Reverse: Primer
      Name: string }

type DivergedPrimerPair =
    | DivergedPrimerPair of PrimerPair
    | SandwichGap
    | Gap

type RYSELinker = { Name: string; Dna: Dna }
