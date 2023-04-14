namespace FlounderLib
open System

type EngineBoard =
    val mutable RepHist:RepetitionHistory
    val mutable Brd:BoardRec
    new(fen) as this = 
        {
            Brd = Board.FromFen(fen)
            RepHist = RepetitionHistory.Default()
        } then NNUEb.AccIndex<-0;NNUEb.ResetAccumulator(this.Brd,0);NNUEb.ResetAccumulator(this.Brd,1)
        
    new(board:EngineBoard) = 
        {
            Brd = board.Brd
            RepHist = board.RepHist.Clone()
        }
    member this.Clone() = EngineBoard(this)
    member this.PieceOnly(sq:int) = this.Brd.Squares.[sq]/2
    member this.IsRepetition() = 
        this.RepHist.Count(this.Brd.ZobristHash) > 1
    member this.GuiMove(from:int, mto:int, promotion:int) =
        let moveList = MoveList(&this.Brd, from)
        if not(Bits.IsSet(moveList.Moves, mto)) then raise (InvalidOperationException("Invalid move provided by GUI."))
        if (promotion <> PromNone && not moveList.Promotion) then
            raise (InvalidOperationException("Invalid move provided by GUI."))
        let rv = Board.Move(&this.Brd, from, mto, promotion)
        this.RepHist.Append(this.Brd.ZobristHash)
    member this.NullMove() =
        let rv = this.Brd.EnPassantTarget
        if this.Brd.EnPassantTarget <> Na then Zobrist.HashEp(&this.Brd.ZobristHash, this.Brd.EnPassantTarget)
        this.Brd.EnPassantTarget <- Na
        this.Brd.IsWtm <- not this.Brd.IsWtm
        this.Brd.Stm <- this.Brd.Stm ^^^ 1  
        this.Brd.Xstm <- this.Brd.Xstm ^^^ 1  
        Zobrist.FlipTurnInHash(&this.Brd.ZobristHash)
        rv
    member this.UndoNullMove(rv:int) =
        if rv <> Na then
            this.Brd.EnPassantTarget <- rv
            Zobrist.HashEp(&this.Brd.ZobristHash, rv)
        this.Brd.IsWtm <- not this.Brd.IsWtm
        this.Brd.Stm <- this.Brd.Stm ^^^ 1  
        this.Brd.Xstm <- this.Brd.Xstm ^^^ 1  
        Zobrist.FlipTurnInHash(&this.Brd.ZobristHash)
    member this.Move(move:byref<OrderedMoveEntry>) =
        let rv:MoveRec =
            NNUEb.AccIndex<-NNUEb.AccIndex+1
            Board.Move(&this.Brd, move.From, move.To, move.Promotion)
        NNUEb.DoUpdate(this.Brd,rv)
        this.RepHist.Append(this.Brd.ZobristHash)
        rv
    member this.UndoMove(rv:byref<MoveRec>) =
        Board.UndoMove(&this.Brd, &rv)
        NNUEb.AccIndex<-NNUEb.AccIndex-1
        this.RepHist.RemoveLast()
module EngineBoard =
    let FromFen(fen:string) = 
        EngineBoard(fen)
    let Default() = 
        let DEFAULT_FEN = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
        FromFen(DEFAULT_FEN)
