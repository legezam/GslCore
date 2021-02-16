namespace GslCore.Pragma

// ======================
// pragma deprecations and deprecation machinery
// ======================

type PragmaDeprecation =
    { Name: string
      Replacement: string
      Replace: Pragma -> Pragma
      ExtraMessage: string option }
    member this.WarningMessage =
        let msg =
            sprintf "The pragma #%s is deprecated; please use #%s instead." this.Name this.Replacement

        match this.ExtraMessage with
        | Some extraMessage -> sprintf "%s\n%s" msg extraMessage
        | None -> msg

module PragmaDeprecation =
    let private replaceStaticPlatformPragma (which: string) (_target: Pragma): Pragma =
        { Pragma.Definition = BuiltIn.platformPragmaDef
          Arguments = [ which ] }

    let private stitchPragmaDeprecation =
        { PragmaDeprecation.Name = "stitch"
          Replacement = "platform"
          Replace = replaceStaticPlatformPragma "stitch"
          ExtraMessage = Some("This pragma will be interpreted as '#platform stitch'.") }

    let private megastitchPragmaDeprecation =
        { PragmaDeprecation.Name = "megastitch"
          Replacement = "platform"
          Replace = replaceStaticPlatformPragma "megastitch"
          ExtraMessage = Some("This pragma will be interpreted as '#platform megastitch'.") }

    let deprecatedPragmas: Map<string, PragmaDeprecation> =
        [ stitchPragmaDeprecation
          megastitchPragmaDeprecation ]
        |> Seq.map (fun depreciation -> depreciation.Name, depreciation)
        |> Map.ofSeq
