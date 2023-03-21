namespace FlounderTest
open NUnit.Framework
open FsUnit
open FlounderLib

module NNUE =
    let board = Board.Default()
    let epfen = "rnbqkbnr/pp2pppp/8/2ppP3/8/8/PPPP1PPP/RNBQKBNR w KQkq d6 0 3"
    let epboard = Board.FromFen(epfen)
    let prmfen = "rnb1kb1r/ppP1pppp/5n2/2p5/8/8/PPPP1PPP/RNBQKBNR w KQkq - 0 5"
    let prmboard = Board.FromFen(prmfen)

    [<SetUp>]
    let Setup () =
        ()

    [<Test>]
    let RefreshBasicAccumulator() =
        let ans = NNUE.RefreshAccumulator(board.Map)
        ans |> should equal ()

    [<Test>]
    let Eua() =
        let ans1 = NNUE.EfficientlyUpdateAccumulator(false, Piece.Pawn, PieceColor.White, Square.E2)
        ans1 |> should equal ()
        let ans2 = NNUE.EfficientlyUpdateAccumulator(true, Piece.Pawn, PieceColor.White, Square.E2)
        ans2 |> should equal ()

    [<Test>]
    let EvaluateBasic() =
        NNUE.RefreshAccumulator(board.Map)
        let ans = NNUE.Evaluate(board.ColorToMove)
        ans |> should equal 68

    [<Test>]
    let EvaluateEP() =
        NNUE.RefreshAccumulator(epboard.Map)
        let ans = NNUE.Evaluate(epboard.ColorToMove)
        ans |> should equal -21

    [<Test>]
    let EvaluateUpdate() =
        NNUE.ResetAccumulator()
        NNUE.RefreshAccumulator(board.Map)
        NNUE.EfficientlyUpdateAccumulator(false, Piece.Pawn, PieceColor.White, Square.E2)
        NNUE.EfficientlyUpdateAccumulator(true, Piece.Pawn, PieceColor.White, Square.E4)
        let ans = NNUE.Evaluate(PieceColor.OppositeColor(board.ColorToMove))
        ans |> should equal -44

    [<Test>]
    let Feature() =
        NNUE.ResetAccumulator()
        NNUE.RefreshAccumulator(board.Map)
        let farln = NNUE.NNUEin.FeatureWeight.Length
        let ffarln = NNUE.NNUEin.FlippedFeatureWeight.Length
        farln|>should equal ffarln
        