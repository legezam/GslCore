module GslCore.Legacy.LegacyPrettyPrint

open GslCore.Constants
open System
open GslCore.Pragma
open GslCore.Ast.Types
open GslCore.Legacy.Types


// ========================
// pretty-printing legacy assemblies as GSL source code
// ========================

/// Pretty print a RelPos
let relativePosition (position: RelativePosition): string =
    sprintf
        "%A/%s"
        position.Position
        (match position.RelativeTo with
         | FivePrime -> "S"
         | ThreePrime -> "E")

let slice (slice: Slice): string =
    sprintf
        "[%s%A%s:%s%A%s]"
        (if slice.LeftApprox then "~" else "")
        slice.Left.Position
        (match slice.Left.RelativeTo with
         | FivePrime -> "S"
         | ThreePrime -> "E")
        (if slice.RightApprox then "~" else "")
        slice.Right.Position
        (match slice.Right.RelativeTo with
         | FivePrime -> "S"
         | ThreePrime -> "E")

let expandModifiers (modifiers: Modifier list): string =
    seq {
        for modifier in modifiers do
            match modifier with
            | Modifier.Mutation mutation ->
                yield
                    sprintf
                        "%c%c%d%c"
                        (match mutation.Type with
                         | AA -> '$'
                         | NT -> '*')
                        mutation.From
                        mutation.Location
                        mutation.To
            | Modifier.Slice slicee -> yield slice slicee
            | Modifier.Dot dot -> yield sprintf ".%s" dot
    }
    |> fun x -> String.Join("", x)

let rec partPlusPragma (partPlusPragma: PartPlusPragma): string =
    let partOut =
        match partPlusPragma.Part with
        | Part.HeterologyBlock -> "~ " // don't do anything at this level
        | Part.InlineDna (s) -> sprintf "/%O/ " s // inline DNA sequence
        | Part.InlineProtein (s) -> sprintf "/$%s/ " s // inline protein sequence
        | Part.MarkerPart -> "### "
        | Part.PartId (p) -> sprintf "@%s" p.Id + (expandModifiers p.Modifiers)
        | Part.SourceCode (s) -> s.String // Part that was already expanded into a string
        | Part.GenePart (gp) ->
            let lOut =
                match gp.Linker with
                | None -> ""
                | Some (l) -> sprintf "%s-%s-%s-" l.Linker1 l.Linker2 l.Orient // Emit linker

            let p = gp.Part

            let gOut = p.Gene
            let modOut = expandModifiers p.Modifiers

            lOut + gOut + modOut // String.Join("",Array.ofSeq modOut)
    // Now add in any inline pragma part with braces, ; separated etc
    let prOut =
        if partPlusPragma.Pragma.Pragmas.Count = 0 then
            ""
        else
            partPlusPragma.Pragma.Pragmas
            |> Seq.map (fun pv -> sprintf "#%s %s" pv.Key (pv.Value.Arguments |> String.concat " "))
            |> fun ss -> String.Join(";", ss)
            |> sprintf "{%s}"

    (if partPlusPragma.IsForward then "" else "!")
    + partOut
    + prOut

/// Pretty print a built GSL assembly
let assembly (assembly: Assembly): GslSourceCode =
    [ for ppp in assembly.Parts -> partPlusPragma ppp ]
    |> String.concat ";"
    |> GslSourceCode
