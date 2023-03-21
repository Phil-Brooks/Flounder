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
    let ResetAccumulator() =
        NNUEb.ResetAccumulator(board.Map,PieceColor.White)