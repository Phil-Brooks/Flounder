namespace FlounderTest
open NUnit.Framework
open FsUnit
open FlounderLib

module NNUE =
    //let board = Board.Default()
    let epfen = "rnbqkbnr/pp2pppp/8/2ppP3/8/8/PPPP1PPP/RNBQKBNR w KQkq d6 0 3"
    //let epboard = Board.FromFen(epfen)
    //let prmfen = "rnb1kb1r/ppP1pppp/5n2/2p5/8/8/PPPP1PPP/RNBQKBNR w KQkq - 0 5"
    //let prmboard = Board.FromFen(prmfen)
    let bugfen = "5rk1/pp4p1/2n1p2p/2Npq3/2p5/6P1/P3P1BP/R4Q1K w - -"
    //let bugboard = Board.FromFen(bugfen)

    [<SetUp>]
    let Setup () =
        ()

    [<Test>]
    let RefreshBasicAccumulator() =
        let ans = NNUE.RefreshAccumulator()
        ans |> should equal ()

    [<Test>]
    let Eua() =
        let ans1 = NNUE.EfficientlyUpdateAccumulator(false, Pawn, White, E2)
        ans1 |> should equal ()
        let ans2 = NNUE.EfficientlyUpdateAccumulator(true, Pawn, White, E2)
        ans2 |> should equal ()

    [<Test>]
    let EvaluateBasic() =
        EngBoard.Default()
        NNUE.RefreshAccumulator()
        let ans = NNUE.Evaluate(Brd.Stm)
        ans |> should equal 68

    [<Test>]
    let EvaluateEP() =
        EngBoard.FromFen(epfen)
        NNUE.RefreshAccumulator()
        let ans = NNUE.Evaluate(Brd.Stm)
        ans |> should equal -21

    [<Test>]
    let EvaluateUpdate() =
        EngBoard.Default()
        NNUE.ResetAccumulator()
        NNUE.RefreshAccumulator()
        NNUE.EfficientlyUpdateAccumulator(false, Pawn, White, E2)
        NNUE.EfficientlyUpdateAccumulator(true, Pawn, White, E4)
        let ans = NNUE.Evaluate(Brd.Xstm)
        ans |> should equal -44

    [<Test>]
    let Feature() =
        EngBoard.Default()
        NNUE.ResetAccumulator()
        NNUE.RefreshAccumulator()
        let farln = NNUE.NNUEin.FeatureWeight.Length
        let ffarln = NNUE.NNUEin.FlippedFeatureWeight.Length
        farln|>should equal ffarln
 
    [<Test>]
    let EvaluateBug() =
        EngBoard.FromFen(bugfen)
        NNUE.ResetAccumulator()
        NNUE.RefreshAccumulator()
        let ans = NNUE.Evaluate(Brd.Stm)
        ans |> should equal 9
        NNUE.PushAccumulator()
        let mutable om = OrdMove.Create(F1, F8, PromNone)
        let mutable rv = EngBoard.Move(&om)
        let ans = NNUE.Evaluate(Brd.Stm)
        ans |> should equal -684
        Board.UndoMove(&rv)
        NNUE.PullAccumulator()
        let ans = NNUE.Evaluate(Brd.Stm)
        ans |> should equal 9

