module GslCore.ApplySlices

open GslCore.Ast.LegacyParseTypes
open GslCore.Constants

/// Given a gene end and two one based offsets, calculate a new one based offset
let addOneOffset (ge: GeneEnd) (a: int<OneOffset>) (b: int<OneOffset>) =
    match ge with
    | FivePrime -> if b > 0<OneOffset> then a + b - 1<OneOffset> else a + b
    | ThreePrime -> if b < 0<OneOffset> then a + b + 1<OneOffset> else a + b

/// What does it mean to apply further slice notations to an existing piece?
let rec applySlices verbose (mods: Mod list) (s: Slice) =
    match mods with
    | [] -> s
    | SLICE (sl) :: tl ->
        // Concatenate slices
        let s' =
            { // subsequent slices could be relative to either end of the existing slice
              lApprox = (if sl.lApprox then true else s.lApprox)
              rApprox = (if sl.rApprox then true else s.rApprox)
              left =
                  match sl.left.RelativeTo with
                  | FivePrime ->
                      { Position = addOneOffset FivePrime sl.left.Position s.left.Position
                        RelativeTo = s.left.RelativeTo }
                  | ThreePrime ->
                      { Position = addOneOffset ThreePrime sl.left.Position s.right.Position
                        RelativeTo = s.right.RelativeTo }
              right =
                  match sl.right.RelativeTo with
                  | FivePrime ->
                      { Position = addOneOffset FivePrime sl.right.Position s.left.Position
                        RelativeTo = s.left.RelativeTo }
                  | ThreePrime ->
                      { Position = addOneOffset ThreePrime sl.right.Position s.right.Position
                        RelativeTo = s.right.RelativeTo } }

        applySlices verbose tl s'
    | x :: tl ->
        if verbose
        then printf "WARNING: ignoring unimplemented mod %A\n" x

        applySlices verbose tl s
