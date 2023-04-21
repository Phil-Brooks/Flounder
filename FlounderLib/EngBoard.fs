namespace FlounderLib
open System

module EngBoard =
    let mutable Brd = Board.Default()
    let FromFen(fen:string) = 
        Brd <- Board.FromFen(fen)
        RepHist.Reset()
        NNUEb.AccIndex<-0
        NNUEb.ResetAccumulator(Brd,White)
        NNUEb.ResetAccumulator(Brd,Black)
        Board.FromFen(fen)
    let Default() = 
        let DEFAULT_FEN = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
        FromFen(DEFAULT_FEN)
    let PieceOnly(brd:BoardRec,sq:int) = brd.Squares.[sq]/2
    let IsRepetition(brd:BoardRec) = 
        RepHist.Count(brd.ZobristHash) > 1
    let GuiMove(brd:byref<BoardRec>,from:int, mto:int, promotion:int) =
        let moveList = MoveList.ForSq(&brd, from)
        if not(Bits.IsSet(moveList.Moves, mto)) then raise (InvalidOperationException("Invalid move provided by GUI."))
        if (promotion <> PromNone && not moveList.Promotion) then
            raise (InvalidOperationException("Invalid move provided by GUI."))
        let rv = Board.Move(&brd, from, mto, promotion)
        RepHist.Append(brd.ZobristHash)
    let NullMove(brd:BoardRec) =
        let rv = brd.EnPassantTarget
        if brd.EnPassantTarget <> Na then Zobrist.HashEp(&brd.ZobristHash, brd.EnPassantTarget)
        brd.EnPassantTarget <- Na
        brd.IsWtm <- not brd.IsWtm
        brd.Stm <- brd.Stm ^^^ 1  
        brd.Xstm <- brd.Xstm ^^^ 1  
        Zobrist.FlipTurnInHash(&brd.ZobristHash)
        rv
    let UndoNullMove(brd:BoardRec,rv:int) =
        if rv <> Na then
            brd.EnPassantTarget <- rv
            Zobrist.HashEp(&brd.ZobristHash, rv)
        brd.IsWtm <- not brd.IsWtm
        brd.Stm <- brd.Stm ^^^ 1  
        brd.Xstm <- brd.Xstm ^^^ 1  
        Zobrist.FlipTurnInHash(&brd.ZobristHash)
    let Move(brd:byref<BoardRec>,move:byref<OrdMoveEntryRec>) =
        let rv:MoveRec =
            NNUEb.AccIndex<-NNUEb.AccIndex+1
            Board.Move(&brd, move.From, move.To, move.Promotion)
        NNUEb.DoUpdate(brd,rv)
        RepHist.Append(brd.ZobristHash)
        rv
    let UndoMove(brd:byref<BoardRec>,rv:byref<MoveRec>) =
        Board.UndoMove(&brd, &rv)
        NNUEb.AccIndex<-NNUEb.AccIndex-1
        RepHist.RemoveLast()
