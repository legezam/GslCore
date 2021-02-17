namespace GslCore

open NUnit.Framework
open GslCore.Uri
open Amyris.ErrorHandling

[<TestFixture>]
type TestUri() = 

    [<Test>]
    member x.TestUriConstruction() =

        let uri = Uri.buildUri [] "test" |> returnOrFail
        Assert.AreEqual("http://amyris.com/GBoM/test", uri)
        Assert.AreEqual(Uri.linkerUri "0" |> returnOrFail, "http://amyris.com/GBoM/Component/Linker/0")
        Assert.AreEqual(Uri.linkerUri "A" |> returnOrFail, "http://amyris.com/GBoM/Component/Linker/A")
