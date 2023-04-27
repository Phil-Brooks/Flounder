namespace FlounderTest
open NUnit.Framework
open FsUnit
open FlounderLib

module Zobrist =

    let mutable Map = Board.Default()
    
    [<SetUp>]
    let Setup () =
        ()

    [<Test>]
    let Hash() =
        Zobrist.Hash(Map) |> should equal 6845746135957128455UL
