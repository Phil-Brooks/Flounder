namespace FlounderTest
open NUnit.Framework
open FsUnit
open FlounderLib
open System
open System.IO

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
        let ans = RefreshAccumulator(board.Map)
        ans |> should equal ()

    [<Test>]
    let Eua() =
        let ans1 = EfficientlyUpdateAccumulator(false, Piece.Pawn, PieceColor.White, Square.E2)
        ans1 |> should equal ()
        let ans2 = EfficientlyUpdateAccumulator(true, Piece.Pawn, PieceColor.White, Square.E2)
        ans2 |> should equal ()

    [<Test>]
    let EvaluateBasic() =
        RefreshAccumulator(board.Map)
        let ans = Evaluate(board.ColorToMove)
        ans |> should equal 68

    [<Test>]
    let EvaluateEP() =
        RefreshAccumulator(epboard.Map)
        let ans = Evaluate(epboard.ColorToMove)
        ans |> should equal -21

    [<Test>]
    let EvaluateUpdate() =
        ResetAccumulator()
        RefreshAccumulator(board.Map)
        EfficientlyUpdateAccumulator(false, Piece.Pawn, PieceColor.White, Square.E2)
        EfficientlyUpdateAccumulator(true, Piece.Pawn, PieceColor.White, Square.E4)
        let ans = Evaluate(PieceColor.OppositeColor(board.ColorToMove))
        ans |> should equal -44

    [<Test>]
    let Feature() =
        ResetAccumulator()
        RefreshAccumulator(board.Map)
        let farln = NNUEin.FeatureWeight.Length
        let ffarln = NNUEin.FlippedFeatureWeight.Length
        farln|>should equal ffarln
        