namespace FlounderTest
open NUnit.Framework
open FsUnit
open FlounderLib
open System.IO
open Newtonsoft.Json

module NNUE =
    let Board = Board1.Default()
    let epfen = "rnbqkbnr/pp2pppp/8/2ppP3/8/8/PPPP1PPP/RNBQKBNR w KQkq d6 0 3"
    let epboard = Board1.FromFen(epfen)
    let prmfen = "rnb1kb1r/ppP1pppp/5n2/2p5/8/8/PPPP1PPP/RNBQKBNR w KQkq - 0 5"
    let prmboard = Board1.FromFen(prmfen)

    let path = @"D:\Github\Flounder\FlounderLib"
    let NNUE_FILE = "BasicNNUE"
    let HASH = "0334adf934"
    let inf = Path.Combine(path, NNUE_FILE + "f-" + HASH + ".nnue.json")
    let txt = File.ReadAllText(inf)
    let basicNNUE = JsonConvert.DeserializeObject<BasicNNUE>(txt)

    [<SetUp>]
    let Setup () =
        ()

    [<Test>]
    let RefreshBasicAccumulator() =
        let ans = basicNNUE.RefreshAccumulator(Board)
        ans |> should equal ()

    [<Test>]
    let Eua() =
        let ans1 = basicNNUE.EfficientlyUpdateAccumulator(AccumulatorOperation.Deactivate, Piece.Pawn, PieceColor.White, Square.E2)
        ans1 |> should equal ()
        let ans2 = basicNNUE.EfficientlyUpdateAccumulator(AccumulatorOperation.Activate, Piece.Pawn, PieceColor.White, Square.E2)
        ans2 |> should equal ()

    [<Test>]
    let EvaluateBasic() =
        let ans = basicNNUE.Evaluate(Board.ColorToMove)
        ans |> should equal 3

    [<Test>]
    let EvaluateEP() =
        basicNNUE.RefreshAccumulator(epboard)
        let ans = basicNNUE.Evaluate(epboard.ColorToMove)
        ans |> should equal -21

    [<Test>]
    let EvaluateUpdate() =
        basicNNUE.ResetAccumulator()
        basicNNUE.RefreshAccumulator(Board)
        basicNNUE.EfficientlyUpdateAccumulator(AccumulatorOperation.Deactivate, Piece.Pawn, PieceColor.White, Square.E2)
        basicNNUE.EfficientlyUpdateAccumulator(AccumulatorOperation.Activate, Piece.Pawn, PieceColor.White, Square.E4)
        let ans = basicNNUE.Evaluate(PieceColor.OppositeColor(Board.ColorToMove))
        ans |> should equal -44
