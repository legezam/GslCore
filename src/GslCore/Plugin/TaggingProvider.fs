/// Assembly transforming plugin that implements seamless part assembly.
module GslCore.Plugin.TaggingPlugin

open System
open FsToolkit.ErrorHandling
open GslCore.Ast.LegacyParseTypes
open GslCore.Core.Types
open GslCore.Core.CommandConfig
open GslCore.GslResult
open GslCore.Pragma
open GslCore.Core.PluginTypes

let taggingArg =
    { Name = "tag"
      Parameters = [ "namespace:value" ]
      Aliases = []
      Description = "Add default tag to every assembly." }

let parseTag (single: string): GslResult<AssemblyTag, string> =
    match single.IndexOf(":") with
    | -1 -> GslResult.err (sprintf "--tag value %s missing expected colon" single)
    | colonPosition ->
        GslResult.ok
            { AssemblyTag.nameSpace = single.[..colonPosition - 1].Trim()
              tag = single.[colonPosition + 1..].Trim() }

let parseTags (args: string list) =
    args |> List.map parseTag |> GslResult.collectA

/// do a trial parse and return ok unit if successful
let validateTag (args: string list): GslResult<unit, string> = parseTags args |> GslResult.ignore

let tagPragmaDef =
    { Name = "tag"
      Shape = AtLeast 1
      Scope = BlockOnly(TransientCumulative)
      Description = "tag assemblies with terms from a namespace."
      InvertsTo = None
      Validate = validateTag }

let gTagPragmaDef =
    { Name = "gtag"
      Shape = AtLeast 1
      Scope = BlockOnly(PersistentCumulative)
      Description = "global tag assemblies with tags from a namespace."
      InvertsTo = None
      Validate = validateTag }

/// Take previous #tag namespace:tagvalue  lines and fold into the assembly structure
let foldInTags (cmdlineTags: AssemblyTag list) (_at: ATContext) (a: DnaAssembly) =
    // gtag is global tag, tag is dna assembly tag
    match List.collect (fun pragma -> pragma.Arguments)
              ([ a.Pragmas |> PragmaCollection.tryFind tagPragmaDef
                 a.Pragmas
                 |> PragmaCollection.tryFind gTagPragmaDef ]
               |> List.choose id) with
    | [] ->
        let newTags =
            cmdlineTags |> Set.ofList |> Set.union a.Tags

        GslResult.ok { a with Tags = newTags }
    | args ->
        parseTags args
        |> GslResult.map (fun newTags ->
            let newTags =
                (cmdlineTags @ newTags)
                |> Set.ofList
                |> Set.union a.Tags

            { a with Tags = newTags })
        |> GslResult.mapErrors (fun parseErrors ->
            { AssemblyTransformationMessage.Message = String.Join(";", parseErrors)
              Kind = ATError
              Assembly = a
              StackTrace = None
              FromException = None }
            |> List.singleton)

type TaggingProvider =
    { cmdlineTags: AssemblyTag list
      /// Optionally attach a function to this plugin behavior to permit its operation to be
      /// configured by command line arguments injected by other plugins.  This is necessary because
      /// seamless assembly can alter a lot of expectations of downstream processing steps.
      processExtraArgs: ParsedCmdLineArg -> TaggingProvider -> TaggingProvider }
    interface IAssemblyTransform with
        member __.ProvidedArgs() = [ taggingArg ]

        member this.Configure(arg) =
            if arg.Specification = taggingArg then
                parseTags arg.Values
                |> GslResult.map (fun parsed ->
                    { this with
                          cmdlineTags = parsed @ this.cmdlineTags })
                |> GslResult.valueOr (fun messages -> failwithf "%s" (String.Join("; ", messages)))
            else
                this
            |> this.processExtraArgs arg :> IAssemblyTransform

        member this.ConfigureFromOptions(_opts) = this :> IAssemblyTransform

        member this.TransformAssembly context assembly =
            foldInTags this.cmdlineTags context assembly

/// Produce an instance of the seamless assembly plugin with the provided extra argument processor.
let createTaggingPlugin extraArgProcessor =
    { Name = "assembly tagging support"
      Description = Some "Allow tagging of assemblies with #tag namespace:tag"
      Behaviors =
          [ { Name = None
              Description = None
              Behavior =
                  AssemblyTransform
                      ({ cmdlineTags = []
                         processExtraArgs = extraArgProcessor }) } ]
      ProvidesPragmas = [ tagPragmaDef; gTagPragmaDef ]
      ProvidesCapas = [] }

let taggingPlugin = createTaggingPlugin (fun _ -> id)
