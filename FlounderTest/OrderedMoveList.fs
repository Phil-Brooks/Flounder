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
        let HistoryTable = HistoryTable()
        let KillerMoveTable = KillerMoveTable()
        let board = Board1.Default()
        let moveSpanarr = Array.zeroCreate<OrderedMoveEntry>(OrderedMoveList.SIZE)//stackalloc OrderedMoveEntry[OrderedMoveList.SIZE];
        let mutable moveSpan = new Span<OrderedMoveEntry>(moveSpanarr)
        let moveList = OrderedMoveList(moveSpan, plyFromRoot, KillerMoveTable, HistoryTable)
        let moveCount = moveList.NormalMoveGeneration(board, SearchedMove.Default)
        moveCount |> should equal 20
        let ans = moveList.[19]
        ans.From |> should equal Square.G1
        ans.Score |> should equal 0