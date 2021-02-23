module GslCore.Core.ApplySlices

open GslCore.Ast.LegacyParseTypes
open GslCore.Constants

/// Given a gene end and two one based offsets, calculate a new one based offset
let addOneOffset (ge: GeneEnd) (a: int<OneOffset>) (b: int<OneOffset>) =
    match ge with
    | FivePrime -> if b > 0<OneOffset> then a + b - 1<OneOffset> else a + b
    | ThreePrime -> if b < 0<OneOffset> then a + b + 1<OneOffset> else a + b

/// What does it mean to apply further slice notations to an existing piece?
let rec applySlices verbose (modifiers: Mod list) (slice: Slice): Slice =
    match modifiers with
    | [] -> slice
    | SLICE headSlice :: tl ->
        // Concatenate slices
        let s' =
            { // subsequent slices could be relative to either end of the existing slice
              Slice.lApprox = (if headSlice.lApprox then true else slice.lApprox)
              rApprox = (if headSlice.rApprox then true else slice.rApprox)
              left =
                  match headSlice.left.RelativeTo with
                  | FivePrime ->
                      { Position = addOneOffset FivePrime headSlice.left.Position slice.left.Position
                        RelativeTo = slice.left.RelativeTo }
                  | ThreePrime ->
                      { Position = addOneOffset ThreePrime headSlice.left.Position slice.right.Position
                        RelativeTo = slice.right.RelativeTo }
              right =
                  match headSlice.right.RelativeTo with
                  | FivePrime ->
                      { Position = addOneOffset FivePrime headSlice.right.Position slice.left.Position
                        RelativeTo = slice.left.RelativeTo }
                  | ThreePrime ->
                      { Position = addOneOffset ThreePrime headSlice.right.Position slice.right.Position
                        RelativeTo = slice.right.RelativeTo } }

        applySlices verbose tl s'
    | x :: tl ->
        if verbose
        then printf "WARNING: ignoring unimplemented mod %A\n" x

        applySlices verbose tl slice
