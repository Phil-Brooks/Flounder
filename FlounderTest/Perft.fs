namespace FlounderTest
open NUnit.Framework
open FsUnit
open FlounderLib
module Perft =

    [<SetUp>]
    let Setup () =
        ()

    [<Test>]
    let Depth1() =
        let (ans1,ans2) = Perft.Depth1()
        ans1 |> should equal ans2

    [<Test>]
    let Depth2() =
        let (ans1,ans2) = Perft.Depth2()
        ans1 |> should equal ans2

    [<Test>]
    let Depth3() =
        let (ans1,ans2) = Perft.Depth3()
        ans1 |> should equal ans2

    [<Test>]
    let Depth4() =
        let (ans1,ans2) = Perft.Depth4()
        ans1 |> should equal ans2

    [<Test>]
    let Depth5() =
        let (ans1,ans2) = Perft.Depth5()
        ans1 |> should equal ans2

    [<Test>]
    let Depth6() =
        let (ans1,ans2) = Perft.Depth6()
        ans1 |> should equal ans2

    //[<Test>]
    //let Depth7() =
    //    let (ans1,ans2) = Perft.Depth7()
    //    ans1 |> should equal ans2


