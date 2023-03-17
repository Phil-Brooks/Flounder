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

    let basicNNUE = 
        let path = @"D:\Github\Flounder\FlounderLib"
        let NNUE_FILE = "BasicNNUEf"
        let HASH = "0334adf934"
        let loadtxt str =
            let pref = NNUE_FILE + "-" + HASH + "-"
            let fl= Path.Combine(path,pref + str) //e.g "FeatureBias.txt"
            let txt = File.ReadAllText(fl)
            let nl = Environment.NewLine
            txt.Split(nl.ToString(), StringSplitOptions.RemoveEmptyEntries)
            |>Array.map(fun s -> int16(s))
        let fb = loadtxt "FeatureBias.txt" 
        let fw = loadtxt "FeatureWeight.txt" 
        let ffw = loadtxt "FlippedFeatureWeight.txt" 
        let ob = loadtxt "OutBias.txt" 
        let ow = loadtxt "OutWeight.txt" 
        let nnue =
            let acca = 
                let ans = Array.zeroCreate 128
                ans|>Array.map (fun a -> Array.zeroCreate 256)
            let accb = 
                let ans = Array.zeroCreate 128
                ans|>Array.map (fun a -> Array.zeroCreate 256)
            {
                Output = [| -1072 |]
                CurrentAccumulator = 0
                FeatureWeight = fw
                FlippedFeatureWeight = ffw
                FeatureBias = fb
                OutWeight = ow
                OutBias = ob
                WhitePOV = Array.zeroCreate 768 
                BlackPOV = Array.zeroCreate 768
                AccumulatorA = acca
                AccumulatorB = accb
                Flatten = Array.zeroCreate 512
            }
        nnue

    [<SetUp>]
    let Setup () =
        ()

    [<Test>]
    let RefreshBasicAccumulator() =
        let ans = basicNNUE.RefreshAccumulator(board.Map)
        ans |> should equal ()

    [<Test>]
    let Eua() =
        let ans1 = basicNNUE.EfficientlyUpdateAccumulator(false, Piece.Pawn, PieceColor.White, Square.E2)
        ans1 |> should equal ()
        let ans2 = basicNNUE.EfficientlyUpdateAccumulator(true, Piece.Pawn, PieceColor.White, Square.E2)
        ans2 |> should equal ()

    [<Test>]
    let EvaluateBasic() =
        let ans = basicNNUE.Evaluate(board.ColorToMove)
        ans |> should equal 3

    [<Test>]
    let EvaluateEP() =
        basicNNUE.RefreshAccumulator(epboard.Map)
        let ans = basicNNUE.Evaluate(epboard.ColorToMove)
        ans |> should equal -21

    [<Test>]
    let EvaluateUpdate() =
        basicNNUE.ResetAccumulator()
        basicNNUE.RefreshAccumulator(board.Map)
        basicNNUE.EfficientlyUpdateAccumulator(false, Piece.Pawn, PieceColor.White, Square.E2)
        basicNNUE.EfficientlyUpdateAccumulator(true, Piece.Pawn, PieceColor.White, Square.E4)
        let ans = basicNNUE.Evaluate(PieceColor.OppositeColor(board.ColorToMove))
        ans |> should equal -44

    [<Test>]
    let Feature() =
        basicNNUE.ResetAccumulator()
        basicNNUE.RefreshAccumulator(board.Map)
        let farln = basicNNUE.FeatureWeight.Length
        let ffarln = basicNNUE.FlippedFeatureWeight.Length
        farln|>should equal ffarln
        