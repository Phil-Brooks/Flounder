namespace FlounderLib
open System

type EngineBoard =
    val mutable RepHist:RepetitionHistory
    val mutable Brd:Board
    new(boardData, turnData, castlingData, enPassantTargetData) as this = 
        {
            Brd = Board(boardData, turnData, castlingData, enPassantTargetData)
            RepHist = RepetitionHistory.Default()
        } then NNUEb.AccIndex<-0;NNUEb.ResetAccumulator(this.Brd.Map,0);NNUEb.ResetAccumulator(this.Brd.Map,1)
        
    new(board:EngineBoard) = 
        {
            Brd = Board(board.Brd.Map)
            RepHist = board.RepHist.Clone()
        }
    member this.Clone() = EngineBoard(this)
    member this.PieceOnly(sq:int) = this.Brd.Map.Squares.[sq]/2
    member this.IsRepetition() = 
        this.RepHist.Count(this.Brd.Map.ZobristHash) > 1
    member this.GuiMove(from:int, mto:int, promotion:int) =
        let moveList = MoveList(this.Brd, from)
        if not(Bits.IsSet(moveList.Moves, mto)) then raise (InvalidOperationException("Invalid move provided by GUI."))
        if (promotion <> PromNone && not moveList.Promotion) then
            raise (InvalidOperationException("Invalid move provided by GUI."))
        let rv = Board.Move(&this.Brd, from, mto, promotion)
        this.RepHist.Append(this.Brd.Map.ZobristHash)
    member this.NullMove() =
        let rv = this.Brd.Map.EnPassantTarget
        if this.Brd.Map.EnPassantTarget <> Na then Zobrist.HashEp(&this.Brd.Map.ZobristHash, this.Brd.Map.EnPassantTarget)
        this.Brd.Map.EnPassantTarget <- Na
        this.Brd.Map.IsWtm <- not this.Brd.Map.IsWtm
        this.Brd.Map.Stm <- this.Brd.Map.Stm ^^^ 1  
        this.Brd.Map.Xstm <- this.Brd.Map.Xstm ^^^ 1  
        Zobrist.FlipTurnInHash(&this.Brd.Map.ZobristHash)
        rv
    member this.UndoNullMove(rv:int) =
        if rv <> Na then
            this.Brd.Map.EnPassantTarget <- rv
            Zobrist.HashEp(&this.Brd.Map.ZobristHash, rv)
        this.Brd.Map.IsWtm <- not this.Brd.Map.IsWtm
        this.Brd.Map.Stm <- this.Brd.Map.Stm ^^^ 1  
        this.Brd.Map.Xstm <- this.Brd.Map.Xstm ^^^ 1  
        Zobrist.FlipTurnInHash(&this.Brd.Map.ZobristHash)
    member this.Move(move:byref<OrderedMoveEntry>) =
        let rv:MoveRec =
            NNUEb.AccIndex<-NNUEb.AccIndex+1
            Board.Move(&this.Brd, move.From, move.To, move.Promotion)
        NNUEb.DoUpdate(this.Brd.Map,rv)
        this.RepHist.Append(this.Brd.Map.ZobristHash)
        rv
    member this.UndoMove(rv:byref<MoveRec>) =
        Board.UndoMove(&this.Brd, &rv)
        NNUEb.AccIndex<-NNUEb.AccIndex-1
        this.RepHist.RemoveLast()
module EngineBoard =
    let FromFen(fen:string) = 
        let parts = fen.Split(" ")
        EngineBoard(parts.[0], parts.[1], parts.[2], parts.[3])
    let Default() = 
        let DEFAULT_FEN = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
        FromFen(DEFAULT_FEN)
