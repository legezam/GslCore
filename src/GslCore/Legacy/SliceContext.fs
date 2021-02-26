module GslCore.Legacy.LegacySliceContext

open GslCore.Constants
open GslCore.Legacy.Types


/// Return a tuple of OneOffset left/right slice bounds from a slice record.
/// These bounds are both relative to the FivePrime end.
/// Requires the length of the feature being sliced to be interpreted correctly.
let getBoundsFromSlice (slice: Slice)
                       (featureLength: int)
                       (context: SliceContext)
                       : Result<int<OneOffset> * int<OneOffset>, string> =
    let left =
        match slice.Left.RelativeTo with
        | FivePrime -> slice.Left.Position
        | ThreePrime ->
            (featureLength + 1) * 1<OneOffset>
            + slice.Left.Position

    let right =
        match slice.Right.RelativeTo with
        | FivePrime -> slice.Right.Position
        | ThreePrime ->
            (featureLength + 1) * 1<OneOffset>
            + slice.Right.Position

    match context with
    | Genomic ->
        // no validation necessary
        Ok(left, right)
    | Library partId ->
        // the slice bounds are not allowed to fall outside the feature as we don't have
        // data on flanking regions in this context
        if left < 1<OneOffset>
           || right <= left
           || right > (featureLength * 1<OneOffset>) then
            Result.Error(sprintf "Illegal slice (%A) outside core gene range for library item %s." slice partId)
        else
            Ok(left, right)