namespace FlounderTest
open NUnit.Framework
open FsUnit
open FlounderLib

module Zobrist =

    let mutable Map = BitBoardMap("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR", "w", "KQkq", "-")
    
    [<SetUp>]
    let Setup () =
        ()

    [<Test>]
    let Hash() =
        BitBoardMap.Hash(Map) |> should equal 15338111103076795062UL
