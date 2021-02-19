namespace GslCore

open GslCore.Plugin
open NUnit.Framework

open GslCore.Core.CommandConfig
open GslCore.ProcessCmdLineArgs
open GslCore.Core.PluginTypes

[<AutoOpen>]
module fixtures =

    let testArg name alias desc =
        { name = name
          param = [ "justOneArg" ]
          alias = [ alias ]
          desc = desc }

    type TestOutputFormat =
        { data: string option
          argName: string
          argAlias: string
          desc: string }
        member x.TestArg = testArg x.argName x.argAlias x.desc

        interface IOutputFormat with
            member x.ProvidedArgs() = [ x.TestArg ]

            member x.Configure(parsedArg) =
                let revised =
                    if parsedArg.spec = x.TestArg then
                        { x with
                              data = Some(parsedArg.values.[0]) }
                    else
                        x
                revised :> IOutputFormat

            member x.ConfigureFromOptions(_) = x :> IOutputFormat

            member x.ProduceOutput(_) =
                match x.data with
                | Some (data) -> ()
                | None -> failwith "Test output provider tried to produce output without data."

    let testOutputPlugin argName argAlias desc =
        { Name = "test plugin"
          Description = None
          Behaviors =
              [ { Name = None
                  Description = None
                  Behavior =
                      OutputFormat
                          ({ data = None
                             argName = argName
                             argAlias = argAlias
                             desc = desc }) } ]
          ProvidesPragmas = []
          ProvidesCapas = [] }

    let getOutputProvider plugin =
        match plugin.Behaviors with
        | [ { Behavior = OutputFormat (op) } ] -> op :?> TestOutputFormat
        | x -> failwithf "Illegal: %A" x

[<TestFixture>]
type TestPluginSystem() =

    [<Test>]
    member x.TestBuiltinConflict() =
        let top =
            testOutputPlugin "lextest" "lextest" "test"

        Assert.Throws(fun () -> collectCommandLineArgs [ top ] |> ignore)
        |> (fun e -> StringAssert.Contains("whose definition conflicts with a built-in", e.Message))

    [<Test>]
    member x.TestOtherPluginConflict() =
        let top1 =
            testOutputPlugin "testArg" "testArg" "test1"

        let top2 =
            testOutputPlugin "testArg" "testArg" "test2"

        Assert.Throws(fun () -> collectCommandLineArgs [ top1; top2 ] |> ignore)
        |> (fun e -> StringAssert.Contains("whose definition conflicts with that from another plugin", e.Message))

    [<Test>]
    member x.TestConfigurePlugin() =
        let top =
            testOutputPlugin "testArg" "testArgAlias" "I'm a test argument."

        let plugins =
            [ top
              BasicCodonProvider.basicCodonProviderPlugin ]

        let argSpecs = collectCommandLineArgs plugins
        let fakeArgs = [ "--testArg"; "foo" ]

        let s =
            configure false argSpecs plugins fakeArgs

        Assert.That(s.Files.IsEmpty)
        Assert.AreEqual(2, s.Plugins.Length)
        let updatedBehavior = getOutputProvider s.Plugins.[0]
        let originalBehavior = getOutputProvider top

        Assert.AreEqual
            ({ originalBehavior with
                   data = Some("foo") },
             updatedBehavior)
