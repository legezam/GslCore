/// IO routine for emitting simple primer details
module GslCore.Plugin.CoreOutput.PrimerDump

open System.IO
open GslCore.Core.Types
open System
open GslCore

/// Dump out all the primers/primerparts to define the construct
let simplePrimerDump (file: string) (primers: DivergedPrimerPair list list) (assemblies: DnaAssembly list) =
    // User wants primers now
    if file <> "-" then printfn "Writing primers to %s" file

    let outF =
        if file = "-" then None else Some(new StreamWriter(file))
    // use outF = new StreamWriter(file)

    let w (s: string) =
        match outF with
        | None -> stdout.WriteLine(s)
        | Some (x) -> x.WriteLine(s)

    let dumpOne i (primerList: DivergedPrimerPair list, assembly: DnaAssembly) =
        //name the primer based on the part in binds to
        let primerInfo isFwd (primer: Primer) =
            //find the AMP part of the primer that binds to the template.
            let ampBody =
                match primer.Interval DNAIntervalType.AMP with
                | Some (i) -> primer.Primer.[i.Left..i.Right]
                | None -> primer.Body
            //search using either the ampBody or reverse complement of it depending on direction of primer
            let searchBody =
                if isFwd then ampBody else ampBody.RevComp()

            let containsPrimer (part: DNASlice) = part.Dna.Contains searchBody

            let bindingPart =
                List.tryFind containsPrimer assembly.DnaParts

            let name =
                match bindingPart with
                | Some value ->
                    if value.SliceName <> "" then value.SliceName
                    else if value.Description <> "" then value.Description
                    else (Utils.ambId value.Id)
                | None -> ""

            if primer.Primer.Length > 0 then
                let cols =
                    seq {
                        yield (sprintf "\"%s_%s\"" name (if isFwd then "fwd" else "rev"))
                        yield (if isFwd then "fwd" else "rev")
                        yield (primer.Primer.str)
                        yield (primer.Tail.str)
                        yield (primer.Body.str)
                        //calculate tm for given primer parts
                        let tm (a: char array) =
                            Amyris.Bio.primercore.temp assembly.DesignParams.PrimerParams a a.Length
                            |> fun t -> sprintf "%3.1f" (t * 1.0 / (1.0<Amyris.Bio.primercore.C>))
                        //emit individual primer parts and tm
                        yield
                            (match primer.Interval DNAIntervalType.Anneal with
                             | Some (i) -> primer.Primer.[i.Left..i.Right].str
                             | None -> "")

                        yield
                            (match primer.Interval DNAIntervalType.Sandwich with
                             | Some (i) -> primer.Primer.[i.Left..i.Right].str
                             | None -> "")

                        yield
                            (match primer.Interval DNAIntervalType.AMP with
                             | Some (i) -> primer.Primer.[i.Left..i.Right].str
                             | None -> "")

                        yield
                            (match primer.Interval DNAIntervalType.Anneal with
                             | Some (i) -> primer.Primer.[i.Left..i.Right].arr |> tm
                             | None -> "")

                        yield
                            (match primer.Interval DNAIntervalType.AMP with
                             | Some (i) -> primer.Primer.[i.Left..i.Right].arr |> tm
                             | None -> "")
                    }

                String.Join("\t", cols) |> w

        for dpp in primerList do
            match dpp with
            | Gap
            | SandwichGap -> () // Don't need to emit these
            | DivergedPrimerPair (dpp) ->
                primerInfo true dpp.Forward
                primerInfo false dpp.Reverse

    w
        (String.Join
            ("\t",
             seq {
                 yield "name"
                 yield "direction"
                 yield "sequence"
                 yield "tail"
                 yield "body"
                 yield "anneal"
                 yield "sandwich"
                 yield "amp"
                 yield "annealTm"
                 yield "ampTm"
             }))

    List.zip primers assemblies
    |> List.iteri (dumpOne)

    match outF with
    | Some (outF) -> outF.Close()
    | None -> ()
