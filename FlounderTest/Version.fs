namespace FlounderTest
open NUnit.Framework
open FsUnit
open FlounderLib

module Vestion =

    [<SetUp>]
    let Setup () =
        ()

    [<Test>]
    let Current() =
        let ans = VersionNo
        ans |> should equal "0.5.0.2"
