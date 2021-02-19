module GslCore.Primer.PrimerValidation

open GslCore.Core.Types
open Amyris.Bio.utils
open Amyris.Dna

let checkAnnotation (p: Primer) errorDesc =
    for a in p.Annotation do
        if a.Left < 0
        then failwithf "primer annotation il (%d) < 0 p=%s %s" a.Left (p.Primer.str) errorDesc

        if a.Right >= p.Tail.Length + p.Body.Length
        then failwithf "primer annotation ir (%d) off end len=%d %s" a.Right (p.Tail.Length + p.Body.Length) errorDesc

        if a.Left > a.Right && p.Tail.Length > 0
        then failwithf "primer annotation il(%d) > ir(%d) %s" a.Left a.Right errorDesc

        match a.Type with
        | Anneal when a.Right - a.Left + 1 < 12 ->
            failwithf "annealing region of a primer is less than 12 bases il=%d ir=%d %s" a.Left a.Right errorDesc
        | AMP when a.Right - a.Left + 1 < 12 ->
            failwithf "amplification region of a primer is less than 12 bases il=%d ir=%d %s" a.Left a.Right errorDesc
        | _ -> () // fine

let checkPrimers (primers: DivergedPrimerPair list list) =
    for pList in primers do
        for primer in pList do
            match primer with
            | DivergedPrimerPair (dpp) ->
                checkAnnotation dpp.Forward dpp.Name
                checkAnnotation dpp.Reverse dpp.Name

                match dpp.Forward.Interval DNAIntervalType.Anneal, dpp.Reverse.Interval DNAIntervalType.Anneal with
                | Some (f), Some (r) ->
                    // Annealing primers fwd and reverse
                    if f.Right - f.Left <> r.Right - r.Left then
                        failwithf
                            "primer annotation issue annealing regions different lengths il1=%d ir1=%d il2=%d ir2=%d"
                            f.Left
                            f.Right
                            r.Left
                            r.Right

                    let s1 = dpp.Forward.Primer.[f.Left..f.Right]
                    let s2 = dpp.Reverse.Primer.[r.Left..r.Right]
                    let s2' = s2.RevComp()

                    if s1 <> s2' then
                        failwithf "primer annotation anneal region fails antiparallel test\nfwd  =%O\nrev  =%O\nrcrev=%O\nname=%s\n"
                            s1 s2 s2' dpp.Name
                | None, Some (x) ->
                    failwithf "primer annotation single anneal region rev %d-%d %O" x.Left x.Right dpp.Reverse.Primer
                | Some (x), None ->
                    failwithf "primer annotation single anneal region fwd %d-%d %O" x.Left x.Right dpp.Forward.Primer
                | None, None -> () // fine
            | Gap
            | SandwichGap -> ()

let checkPrimersVAssembly (pa: (DivergedPrimerPair list * DnaAssembly) list) =
    for pList, assembly in pa do

        let assemblySeq = assembly.Sequence()

        for primer in pList do
            match primer with
            | DivergedPrimerPair (dpp) ->
                // Ensure assembly contains primer
                let fwd = dpp.Forward.Primer

                if not (assemblySeq.Contains(fwd)) then
                    failwithf "fwd primer validation failure.  Primer %O\ntail=%O\nhead=%O\n does not occur in assembly %s\n%s"
                        fwd dpp.Forward.Tail dpp.Forward.Body assembly.Name (assemblySeq.arr |> format60)

                let rev = dpp.Reverse.Primer.RevComp()

                if not (assemblySeq.Contains(rev)) then
                    failwithf "rev primer validation failure.  Primer %O\ntail=%O\nbody=%O\n does not occur in assembly %s\n%s"
                        rev dpp.Reverse.Tail dpp.Reverse.Body assembly.Name (assemblySeq.arr |> format60)

                ()
            | Gap
            | SandwichGap -> ()

        let lastN N (c: Dna) =
            c.[c.Length - 1 - N |> max 0..c.Length - 1]

        /// More stringent check that some reasonable primer tail binds to the template DNA sequences
        let templateSeq =
            assembly.DnaParts
            |> List.map (fun slice ->
                match slice.Template with
                | None when slice.Type = SliceType.Linker -> [| 'n'; 'n' |]
                | None -> [| 'N'; 'N' |]
                | Some (x) ->
                    Array.concat [ [| 'N' |]
                                   x.arr
                                   [| 'N' |] ])
            |> Array.concat
            |> fun s -> Dna(s, false, AllowAmbiguousBases)

        let templateSeqRC = templateSeq.RevComp()

        for primer in pList do
            match primer with
            | DivergedPrimerPair (dpp) ->
                let fwd = dpp.Forward.Body |> lastN 10

                let rev =
                    dpp.Reverse.Body |> lastN 10 |> fun d -> d.RevComp()

                let ff = templateSeq.Contains fwd
                let fr = templateSeqRC.Contains fwd
                let rf = templateSeq.Contains rev
                let rr = templateSeqRC.Contains rev

                // Ensure assembly contains primer
                if not (ff || fr) then
                    failwithf "fwd XXX primer validation failure.  Primerlast10 %O\ntail=%O\nbody=%O\n does not occur fwd or rc in template %s\n>template\n%s\n>assembly\n%s"
                        fwd dpp.Forward.Tail dpp.Forward.Body assembly.Name (templateSeq.arr |> format60)
                        (assemblySeq.arr |> format60)

                if not (rf || rr) then
                    failwithf "rev primer validation failure.  Primer %O\ntail=%O\nbody=%O\n does not occur fwd or rc in template %s\n>template\n%s\n>assembly\n%s"
                        rev dpp.Reverse.Tail dpp.Reverse.Body assembly.Name (templateSeq.arr |> format60)
                        (assemblySeq.arr |> format60)
            | Gap
            | SandwichGap -> ()
