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
        let HistTbl = HistoryTable.Default
        let KillerMvTbl = KillerMoveTable.Default
        let mutable board = Board.Default()
        let moveSpanarr = Array.zeroCreate<OrderedMoveEntry>(OrderedMoveList.SIZE)//stackalloc OrderedMoveEntry[OrderedMoveList.SIZE];
        let mutable moveSpan = new Span<OrderedMoveEntry>(moveSpanarr)
        let moveList = OrderedMoveList(moveSpan, plyFromRoot, KillerMvTbl, HistTbl)
        let moveCount = moveList.NormalMoveGeneration(&board, OrderedMoveEntry.Default)
        moveCount |> should equal 20
        let ans = moveList.[19]
        ans.From |> should equal G1
        ans.Score |> should equal 0
