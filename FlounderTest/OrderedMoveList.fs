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
        EngBoard.Default()
        let moveSpanarr = Array.zeroCreate<OrdMoveEntryRec>(OrderedMoveList.SIZE)//stackalloc OrdMoveEntryRec[OrderedMoveList.SIZE];
        let mutable moveSpan = new Span<OrdMoveEntryRec>(moveSpanarr)
        let moveList = OrderedMoveList(moveSpan, plyFromRoot, KillerMvTbl, HistTbl)
        let moveCount = moveList.NormalMoveGeneration(OrdMove.Default)
        moveCount |> should equal 20
        let ans = moveList.[19]
        ans.From |> should equal G1
        ans.Score |> should equal 0
