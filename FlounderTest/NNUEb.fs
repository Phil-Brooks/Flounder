namespace FlounderTest
open NUnit.Framework
open FsUnit
open FlounderLib

module NNUEb =
    let board = Board.Default()
    let epfen = "rnbqkbnr/pp2pppp/8/2ppP3/8/8/PPPP1PPP/RNBQKBNR w KQkq d6 0 3"
    let epboard = Board.FromFen(epfen)
    let prmfen = "rnb1kb1r/ppP1pppp/5n2/2p5/8/8/PPPP1PPP/RNBQKBNR w KQkq - 0 5"
    let prmboard = Board.FromFen(prmfen)

    [<SetUp>]
    let Setup () =
        ()

    [<Test>]
    let LoadInputWeights() =
        let m0 = NNUEb.NNUEin.InputWeights.[0]
        m0 |> should equal -1s
        let m100 = NNUEb.NNUEin.InputWeights.[100]
        m100 |> should equal 2s
        let last = NNUEb.NNUEin.InputWeights.Length - 1
        last |> should equal 9437183
        let mlast = NNUEb.NNUEin.InputWeights.[last]
        mlast |> should equal 83s

    [<Test>]
    let LoadInputBiases() =
        let m0 = NNUEb.NNUEin.InputBiases.[0]
        m0 |> should equal -391s
        let m100 = NNUEb.NNUEin.InputBiases.[100]
        m100 |> should equal -307s
        let last = NNUEb.NNUEin.InputBiases.Length - 1
        last |> should equal 767
        let mlast = NNUEb.NNUEin.InputBiases.[last]
        mlast |> should equal -477s

    [<Test>]
    let LoadOutputWeights() =
        let m0 = NNUEb.NNUEin.OutputWeights.[0]
        m0 |> should equal -711s
        let m100 = NNUEb.NNUEin.OutputWeights.[100]
        m100 |> should equal 6203s
        let last = NNUEb.NNUEin.OutputWeights.Length - 1
        last |> should equal 1535
        let mlast = NNUEb.NNUEin.OutputWeights.[last]
        mlast |> should equal -614s

    [<Test>]
    let LoadOutputBiases() =
        let ob = NNUEb.NNUEin.OutputBias
        ob |> should equal 56469

    [<Test>]
    let ResetRefreshTable() =
        NNUEb.ResetRefreshTable()
        let m0 = NNUEb.RefreshTable.[0].AccKsValues.[0]
        m0 |> should equal -391s
        NNUEb.RefreshTable.[0].Pcs.[0].Internal |> should equal 0UL
        let m100 = NNUEb.RefreshTable.[0].AccKsValues.[100]
        m100 |> should equal -307s
        let last = NNUEb.RefreshTable.[0].AccKsValues.Length - 1
        last |> should equal 767
        let mLast = NNUEb.RefreshTable.[0].AccKsValues.[last]
        mLast |> should equal -477s
    
    [<Test>]
    let ResetAccumulatorWhite() =
        NNUEb.ResetAccumulator(board.Map,PieceColor.White)
        let m0 = NNUEb.Accumulators.[NNUEb.AccIndex].AccValues.[0].[0]
        m0 |> should equal -110s
        let m100 = NNUEb.Accumulators.[NNUEb.AccIndex].AccValues.[0].[100]
        m100 |> should equal -1031s
        let last = NNUEb.Accumulators.[NNUEb.AccIndex].AccValues.[0].Length - 1
        last |> should equal 767
        let mLast = NNUEb.Accumulators.[NNUEb.AccIndex].AccValues.[0].[last]
        mLast |> should equal 16s

    [<Test>]
    let ResetAccumulatorBlack() =
        NNUEb.ResetAccumulator(board.Map,PieceColor.Black)
        let m0 = NNUEb.Accumulators.[NNUEb.AccIndex].AccValues.[1].[0]
        m0 |> should equal -110s
        let m100 = NNUEb.Accumulators.[NNUEb.AccIndex].AccValues.[1].[100]
        m100 |> should equal -1031s
        let last = NNUEb.Accumulators.[NNUEb.AccIndex].AccValues.[1].Length - 1
        last |> should equal 767
        let mLast = NNUEb.Accumulators.[NNUEb.AccIndex].AccValues.[1].[last]
        mLast |> should equal 16s

    [<Test>]
    let RefreshAccumulator() =
        let fen = "r1bqkbnr/pppp1ppp/2n5/4p3/Q1P5/8/PP1PPPPP/RNB1KBNR w KQkq - 0 3"
        let board = Board.FromFen(fen)
        NNUEb.ResetAccumulator(board.Map,PieceColor.White)
        NNUEb.ResetAccumulator(board.Map,PieceColor.Black)
        let m0 = NNUEb.Accumulators.[NNUEb.AccIndex].AccValues.[0].[0]
        m0 |> should equal -495s
        let m0 = NNUEb.Accumulators.[NNUEb.AccIndex].AccValues.[1].[0]
        m0 |> should equal -165s
        let m100 = NNUEb.Accumulators.[NNUEb.AccIndex].AccValues.[0].[100]
        m100 |> should equal -767s
        let m100 = NNUEb.Accumulators.[NNUEb.AccIndex].AccValues.[1].[100]
        m100 |> should equal -978s
        let last = NNUEb.Accumulators.[NNUEb.AccIndex].AccValues.[1].Length - 1
        last |> should equal 767
        let mLast = NNUEb.Accumulators.[NNUEb.AccIndex].AccValues.[0].[last]
        mLast |> should equal -41s
        let mLast = NNUEb.Accumulators.[NNUEb.AccIndex].AccValues.[1].[last]
        mLast |> should equal 39s
        let rv = board.Move(Square.E1,Square.D1)
        NNUEb.AccIndex<-NNUEb.AccIndex+1
        NNUEb.RefreshAccumulator(board.Map,PieceColor.White)
        let x0 = NNUEb.Accumulators.[NNUEb.AccIndex].AccValues.[0].[0]
        m0 |> should equal -165s