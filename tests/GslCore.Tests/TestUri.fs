namespace GslCore

open NUnit.Framework
open Uri

[<TestFixture>]
type TestUri() = 

    [<Test>]
    member x.TestUriConstruction() =

        match Uri.buildUri [] "test" with
        | Ok(u) -> Assert.AreEqual("http://amyris.com/GBoM/test", u)
        | Err(e) -> failwith e

        Assert.AreEqual(Uri.linkerUri "0", "http://amyris.com/GBoM/Component/Linker/0")
        Assert.AreEqual(Uri.linkerUri "A", "http://amyris.com/GBoM/Component/Linker/A")
