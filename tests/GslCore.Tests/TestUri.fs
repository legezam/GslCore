namespace GslCore

open GslCore.GslResult
open NUnit.Framework
open GslCore.Uri

[<TestFixture>]
[<Category("Integration")>]
type TestUri() =

    [<Test>]
    member x.TestUriConstruction() =

        let uri =
            Uri.buildUri [] "test"
            |> GslResult.valueOr (failwithf "%A")

        Assert.AreEqual("http://amyris.com/GBoM/test", uri)

        Assert.AreEqual
            (Uri.linkerUri "0"
             |> GslResult.valueOr (failwithf "%A"),
             "http://amyris.com/GBoM/Component/Linker/0")

        Assert.AreEqual
            (Uri.linkerUri "A"
             |> GslResult.valueOr (failwithf "%A"),
             "http://amyris.com/GBoM/Component/Linker/A")
