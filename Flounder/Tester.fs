namespace Flounder
open System
open System.IO
open FlounderLib
open System.Diagnostics

module Tester =
    let MaxDepth = 30
    let TesterFen = 
        [|
            "2rr3k/pp3pp1/1nnqbN1p/3pN3/2pP4/2P3Q1/PPB4P/R4RK1 w - - bm g3g6"
            "8/7p/5k2/5p2/p1p2P2/Pr1pPK2/1P1R3P/8 b - - bm b3b2"
            "5rk1/1ppb3p/p1pb4/6q1/3P1p1r/2P1R2P/PP1BQ1P1/5RKN w - - bm e3g3"
            "r1bq2rk/pp3pbp/2p1p1pQ/7P/3P4/2PB1N2/PP3PPR/2KR4 w - - bm h6h7"
            "5k2/6pp/p1qN4/1p1p4/3P4/2PKP2Q/PP3r2/3R4 b - - bm c6c4"
            "7k/p7/1R5K/6r1/6p1/6P1/8/8 w - - bm b6b7"
            "rnbqkb1r/pppp1ppp/8/4P3/6n1/7P/PPPNPPP1/R1BQKBNR b KQkq - bm g4e3"
            "r4q1k/p2bR1rp/2p2Q1N/5p2/5p2/2P5/PP3PPP/R5K1 w - - bm e7f7"
            "3q1rk1/p4pp1/2pb3p/3p4/6Pr/1PNQ4/P1PB1PP1/4RRK1 b - - bm d6h2"
            "2br2k1/2q3rn/p2NppQ1/2p1P3/Pp5R/4P3/1P3PPP/3R2K1 w - - bm h4h7"
            "r1b1kb1r/3q1ppp/pBp1pn2/8/Np3P2/5B2/PPP3PP/R2Q1RK1 w kq - bm f3c6"
            "4k1r1/2p3r1/1pR1p3/3pP2p/3P2qP/P4N2/1PQ4P/5R1K b - - bm g4f3"
            "5rk1/pp4p1/2n1p2p/2Npq3/2p5/6P1/P3P1BP/R4Q1K w - - bm f1f8"
            "r2rb1k1/pp1q1p1p/2n1p1p1/2bp4/5P2/PP1BPR1Q/1BPN2PP/R5K1 w - - bm h3h7"
            "1R6/1brk2p1/4p2p/p1P1Pp2/P7/6P1/1P4P1/2R3K1 w - - bm b8b7"
            "r4rk1/ppp2ppp/2n5/2bqp3/8/P2PB3/1PP1NPPP/R2Q1RK1 w - - bm e2c3"
            "1k5r/pppbn1pp/4q1r1/1P3p2/2NPp3/1QP5/P4PPP/R1B1R1K1 w - - bm c4e5"
            "R7/P4k2/8/8/8/8/r7/6K1 w - - bm a8h8"
            "r1b2rk1/ppbn1ppp/4p3/1QP4q/3P4/N4N2/5PPP/R1B2RK1 w - - bm c5c6"
            "r2qkb1r/1ppb1ppp/p7/4p3/P1Q1P3/2P5/5PPP/R1B2KNR b kq - bm d7b5"
        |]
    let Test (argv:string array) =
        let table = MoveTranspositionTable.GenerateTable(16)
        let timeControl = TimeControl(9999999)
        let search = MoveSearch(EngineBoard.Default(), table, timeControl)
        let stopwatch = new Stopwatch()
        stopwatch.Start()
        let fens =
            if argv.Length > 1 then 
                File.ReadAllLines(argv.[1])
            else TesterFen
        for i = 0 to fens.Length-1 do
            let bits = fens.[i].Split("bm") 
            let fen = bits.[0]
            let ebm = bits.[1].Trim()
            Console.WriteLine("Position (" + (i + 1).ToString() + "/" + fens.Length.ToString() + "): " + fen)
            let board = EngineBoard.FromFen(fen)
            search.Reset(board, timeControl)
            let bestMove = search.DoTest(MaxDepth,ebm)
            let bm = OrderedMoveEntry.ToStr(bestMove)
            Console.WriteLine("bestmove " + bm)
            if bm <> ebm then failwith("Test " + (i + 1).ToString() + " failed with wrong best move: " + bm)
        Console.WriteLine(fens.Length.ToString() + " run successfully.")
        stopwatch.Stop()
        let elap = stopwatch.ElapsedMilliseconds
        Console.WriteLine(elap.ToString() + "ms taken.")
        let expElap = 100000L
        let pc = (100L * elap)/expElap - 100L
        if pc>0 then
            Console.WriteLine("BAD: " + pc.ToString() + "% extra time taken.")
        else
            Console.WriteLine("GOOD: " + (-pc).ToString() + "% less time taken.")
        