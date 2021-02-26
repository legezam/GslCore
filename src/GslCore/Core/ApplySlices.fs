module GslCore.Core.ApplySlices

open GslCore.Legacy.Types
open GslCore.Constants

/// Given a gene end and two one based offsets, calculate a new one based offset
let addOneOffset (ge: GeneEnd) (a: int<OneOffset>) (b: int<OneOffset>) =
    match ge with
    | FivePrime -> if b > 0<OneOffset> then a + b - 1<OneOffset> else a + b
    | ThreePrime -> if b < 0<OneOffset> then a + b + 1<OneOffset> else a + b

/// What does it mean to apply further slice notations to an existing piece?
let rec applySlices verbose (modifiers: Modifier list) (slice: Slice): Slice =
    match modifiers with
    | [] -> slice
    | Modifier.Slice headSlice :: tl ->
        // Concatenate slices
        let s' =
            { // subsequent slices could be relative to either end of the existing slice
              Slice.LeftApprox = (if headSlice.LeftApprox then true else slice.LeftApprox)
              RightApprox = (if headSlice.RightApprox then true else slice.RightApprox)
              Left =
                  match headSlice.Left.RelativeTo with
                  | FivePrime ->
                      { Position = addOneOffset FivePrime headSlice.Left.Position slice.Left.Position
                        RelativeTo = slice.Left.RelativeTo }
                  | ThreePrime ->
                      { Position = addOneOffset ThreePrime headSlice.Left.Position slice.Right.Position
                        RelativeTo = slice.Right.RelativeTo }
              Right =
                  match headSlice.Right.RelativeTo with
                  | FivePrime ->
                      { Position = addOneOffset FivePrime headSlice.Right.Position slice.Left.Position
                        RelativeTo = slice.Left.RelativeTo }
                  | ThreePrime ->
                      { Position = addOneOffset ThreePrime headSlice.Right.Position slice.Right.Position
                        RelativeTo = slice.Right.RelativeTo } }

        applySlices verbose tl s'
    | x :: tl ->
        if verbose
        then printf "WARNING: ignoring unimplemented mod %A\n" x

        applySlices verbose tl slice
