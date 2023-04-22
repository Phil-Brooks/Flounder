namespace FlounderTest
open NUnit.Framework
open FsUnit
open System
open FlounderLib
module OrderedMoveList =

    [<SetUp>]
    let Setup () =
        ()

    [<Test>]
    let NormalSearcha() =
        let plyFromRoot = 1
        Hist.Clear()
        KillMv.Clear()
        EngBoard.Default()
        let movearr = Array.zeroCreate OrdMoves.SIZE
        let moveList = OrdMoves.Create(movearr, plyFromRoot)
        let moveCount = OrdMoves.NormalMoveGeneration(moveList,OrdMove.Default)
        moveCount |> should equal 20
        let ans = OrdMoves.Get(moveList, 19)
        ans.From |> should equal G1
        ans.Score |> should equal 0
