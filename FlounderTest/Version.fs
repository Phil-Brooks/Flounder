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
        let ans = Version.Current
        ans |> should equal "0.2.7.6"