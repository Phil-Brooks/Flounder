namespace FlounderLib
open System

module EngBoard =
    let FromFen(fen:string) = 
        Brd <- Board.FromFen(fen)
        RepHist.Reset()
        NNUEb.AccIndex<-0
        NNUEb.ResetAccumulator(White)
        NNUEb.ResetAccumulator(Black)
    let Default() = 
        let DEFAULT_FEN = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
        FromFen(DEFAULT_FEN)
    let PieceOnly(sq:int) = Brd.Squares.[sq]/2
    let IsRepetition() = 
        RepHist.Count(Brd.ZobristHash) > 1
    let GuiMove(from:int, mto:int, promotion:int) =
        let moveList = MoveList.ForSq(from)
        if not(Bits.IsSet(moveList.Moves, mto)) then raise (InvalidOperationException("Invalid move provided by GUI."))
        if (promotion <> PromNone && not moveList.Promotion) then
            raise (InvalidOperationException("Invalid move provided by GUI."))
        let rv = Board.Move(from, mto, promotion)
        RepHist.Append(Brd.ZobristHash)
    let NullMove() =
        let rv = Brd.EnPassantTarget
        if Brd.EnPassantTarget <> Na then Zobrist.HashEp(&Brd.ZobristHash, Brd.EnPassantTarget)
        Brd.EnPassantTarget <- Na
        Brd.IsWtm <- not Brd.IsWtm
        Brd.Stm <- Brd.Stm ^^^ 1  
        Brd.Xstm <- Brd.Xstm ^^^ 1  
        Zobrist.FlipTurnInHash(&Brd.ZobristHash)
        rv
    let UndoNullMove(rv:int) =
        if rv <> Na then
            Brd.EnPassantTarget <- rv
            Zobrist.HashEp(&Brd.ZobristHash, rv)
        Brd.IsWtm <- not Brd.IsWtm
        Brd.Stm <- Brd.Stm ^^^ 1  
        Brd.Xstm <- Brd.Xstm ^^^ 1  
        Zobrist.FlipTurnInHash(&Brd.ZobristHash)
    let Move(move:OrdMoveEntryRec) =
        let rv:MoveRec =
            NNUEb.AccIndex<-NNUEb.AccIndex+1
            Board.Move(move.From, move.To, move.Promotion)
        NNUEb.DoUpdate(rv)
        RepHist.Append(Brd.ZobristHash)
        rv
    let UndoMove(rv:MoveRec) =
        Board.UndoMove(rv)
        NNUEb.AccIndex<-NNUEb.AccIndex-1
        RepHist.RemoveLast()
