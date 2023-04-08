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
        NNUEb.ResetAccumulator(board.Map,0)
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
        NNUEb.ResetAccumulator(board.Map,1)
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
        NNUEb.ResetAccumulator(board.Map,0)
        NNUEb.ResetAccumulator(board.Map,1)
        NNUEb.ResetRefreshTable()
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
        let rv = board.Move(E1,D1)
        NNUEb.AccIndex<-NNUEb.AccIndex+1
        NNUEb.RefreshAccumulator(board.Map,0)
        let m0 = NNUEb.Accumulators.[NNUEb.AccIndex].AccValues.[0].[0]
        m0 |> should equal -443s
        let m100 = NNUEb.Accumulators.[NNUEb.AccIndex].AccValues.[0].[100]
        m100 |> should equal -1007s
        let mLast = NNUEb.Accumulators.[NNUEb.AccIndex].AccValues.[0].[767]
        mLast |> should equal -433s

    [<Test>]
    let MoveRequiresRefresh() =
        let fen = "r1bqkbnr/pppp1ppp/2n5/4p3/Q1P5/8/PP1PPPPP/RNB1KBNR w KQkq - 0 3"
        let board = Board.FromFen(fen)
        NNUEb.ResetAccumulator(board.Map,0)
        NNUEb.ResetAccumulator(board.Map,1)
        NNUEb.ResetRefreshTable()
        let ans = NNUEb.MoveRequiresRefresh(WhiteKing,int(E1),int(D1))
        ans|>should equal true
        let ans = NNUEb.MoveRequiresRefresh(WhitePawn,int(A2),int(A3))
        ans|>should equal false

    [<Test>]
    let ApplyUpdates() =
        let fen = "r1bqkbnr/pppp1ppp/2n5/4p3/Q1P5/8/PP1PPPPP/RNB1KBNR w KQkq - 0 3"
        let board = Board.FromFen(fen)
        NNUEb.ResetAccumulator(board.Map,0)
        NNUEb.ResetAccumulator(board.Map,1)
        NNUEb.ResetRefreshTable()
        let rv = board.Move(E1,D1)
        NNUEb.AccIndex<-NNUEb.AccIndex+1
        NNUEb.ApplyUpdates(board.Map,rv,1)
        let m0 = NNUEb.Accumulators.[NNUEb.AccIndex].AccValues.[1].[0]
        m0 |> should equal -127s
        let m100 = NNUEb.Accumulators.[NNUEb.AccIndex].AccValues.[1].[100]
        m100 |> should equal -921s
        let mLast = NNUEb.Accumulators.[NNUEb.AccIndex].AccValues.[1].[767]
        mLast |> should equal 52s

    [<Test>]
    let ApplyUpdatesCap() =
        let fen = "rnbqkbnr/pppp1ppp/8/4p3/4PP2/8/PPPP2PP/RNBQKBNR b KQkq - 0 2"
        let board = Board.FromFen(fen)
        NNUEb.ResetAccumulator(board.Map,0)
        NNUEb.ResetAccumulator(board.Map,1)
        NNUEb.ResetRefreshTable()
        let rv = board.Move(E5,F4)
        NNUEb.AccIndex<-NNUEb.AccIndex+1
        NNUEb.ApplyUpdates(board.Map,rv,0)
        let m0 = NNUEb.Accumulators.[NNUEb.AccIndex].AccValues.[0].[0]
        m0 |> should equal 14s
        let m100 = NNUEb.Accumulators.[NNUEb.AccIndex].AccValues.[0].[100]
        m100 |> should equal -935s
        let mLast = NNUEb.Accumulators.[NNUEb.AccIndex].AccValues.[0].[767]
        mLast |> should equal -87s

    [<Test>]
    let ApplyUpdatesCas() =
        let fen = "r1bqk1nr/pppp1ppp/2n5/2b1p3/2B1P3/5N2/PPPP1PPP/RNBQK2R w KQkq - 0 4"
        let board = Board.FromFen(fen)
        NNUEb.ResetAccumulator(board.Map,0)
        NNUEb.ResetAccumulator(board.Map,1)
        NNUEb.ResetRefreshTable()
        let rv = board.Move(E1,G1)
        NNUEb.AccIndex<-NNUEb.AccIndex+1
        NNUEb.ApplyUpdates(board.Map,rv,1)
        let m0 = NNUEb.Accumulators.[NNUEb.AccIndex].AccValues.[1].[0]
        m0 |> should equal -93s
        let m100 = NNUEb.Accumulators.[NNUEb.AccIndex].AccValues.[1].[100]
        m100 |> should equal -873s
        let mLast = NNUEb.Accumulators.[NNUEb.AccIndex].AccValues.[1].[767]
        mLast |> should equal 26s

    [<Test>]
    let OutputLayer() =
        let board = Board.Default()
        NNUEb.ResetAccumulator(board.Map,0)
        NNUEb.ResetAccumulator(board.Map,1)
        NNUEb.ResetRefreshTable()
        let m0 = NNUEb.Accumulators.[NNUEb.AccIndex].AccValues.[0].[0]
        m0 |> should equal -110s
        let ans = NNUEb.OutputLayer(board.Map.stm)
        ans|>should equal 101        
 
    [<Test>]
    let DoUpdate() =
        let fen = "r1bqkbnr/pppp1ppp/2n5/4p3/Q1P5/8/PP1PPPPP/RNB1KBNR w KQkq - 0 3"
        let board = Board.FromFen(fen)
        NNUEb.ResetAccumulator(board.Map,0)
        NNUEb.ResetAccumulator(board.Map,1)
        NNUEb.ResetRefreshTable()
        let rv = board.Move(E1,D1)
        NNUEb.AccIndex<-NNUEb.AccIndex+1
        NNUEb.DoUpdate(board.Map,rv)
        let m0 = NNUEb.Accumulators.[NNUEb.AccIndex].AccValues.[1].[0]
        m0 |> should equal -127s
        let m100 = NNUEb.Accumulators.[NNUEb.AccIndex].AccValues.[1].[100]
        m100 |> should equal -921s
        let mLast = NNUEb.Accumulators.[NNUEb.AccIndex].AccValues.[1].[767]
        mLast |> should equal 52s

    [<Test>]
    let EvaluateDiff() =
        let fen = "rnbqkbnr/ppp1pppp/8/3p4/4P3/8/PPPPKPPP/RNBQ1BNR b kq -"
        let board = Board.FromFen(fen)
        NNUEb.ResetAccumulator(board.Map,0)
        NNUEb.ResetAccumulator(board.Map,1)
        let ans = NNUEb.OutputLayer(board.Map.stm)
        ans |> should equal 208

    [<Test>]
    let TestBroken() =
    //Issue with King Move
        let fen = "rnb1kbnr/ppp2ppp/8/3qN3/3Pp3/8/PPP2PPP/RNBQKB1R b KQkq d3 0 5"
        let engbrd = EngineBoard.FromFen(fen)
        NNUEb.ResetAccumulator(engbrd.Brd.Map,0)
        NNUEb.ResetAccumulator(engbrd.Brd.Map,1)
        NNUEb.ResetRefreshTable()
        let mutable mv = OrderedMoveEntry(E4, D3, Promotion.None)
        let mutable rv = engbrd.Move(&mv)
        let ans = NNUEb.OutputLayer(engbrd.Brd.Map.stm)
        ans|>should equal -45
