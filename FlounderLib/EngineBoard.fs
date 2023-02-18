namespace FlounderLib
open System

type EngineBoard =
    val mutable Board:Board
    val mutable History:RepetitionHistory
    new(boardData, turnData, castlingData, enPassantTargetData) as this = 
        {
            Board = Board(boardData, turnData, castlingData, enPassantTargetData)
            History = new RepetitionHistory(1024)
        } then NNUE.ResetAccumulator();NNUE.RefreshAccumulator(this.Board.Map)
    new(board:EngineBoard) = 
        {
            Board = board.Board.Clone()
            History = board.History.Clone()
        }
    member this.Clone() = EngineBoard(this)
    member this.PieceOnly(sq:Square) = this.Board.Map.PieceOnly(sq)
    member this.IsRepetition() = 
        this.History.Count(this.Board.ZobristHash) > 1
    member this.GuiMove(from:Square, mto:Square, promotion:Promotion) =
        let moveList = MoveList(this.Board, from)
        if not(moveList.Moves.[mto]) then raise (InvalidOperationException("Invalid move provided by GUI."))
        if (promotion <> Promotion.None && not moveList.Promotion) then
            raise (InvalidOperationException("Invalid move provided by GUI."))
        let rv = this.Board.Move(from, mto, promotion)
        this.History.Append(this.Board.ZobristHash)
    member this.NullMove() =
        let rv = this.Board.Map.EnPassantTarget
        if (this.Board.Map.EnPassantTarget <> Square.Na) then Zobrist.HashEp(&this.Board.Map.ZobristHash, this.Board.Map.EnPassantTarget)
        this.Board.Map.EnPassantTarget <- Square.Na
        this.Board.Map.ColorToMove <- PieceColor.OppositeColor(this.Board.Map.ColorToMove)
        Zobrist.FlipTurnInHash(&this.Board.Map.ZobristHash)
        rv
    member this.UndoNullMove(rv:Square) =
        if (rv <> Square.Na) then
            this.Board.Map.EnPassantTarget <- rv
            Zobrist.HashEp(&this.Board.Map.ZobristHash, rv)
        this.Board.Map.ColorToMove <- PieceColor.OppositeColor(this.Board.Map.ColorToMove)
        Zobrist.FlipTurnInHash(&this.Board.Map.ZobristHash)
    member this.Move(move:byref<OrderedMoveEntry>) =
        let rv:RevertMove =
            NNUE.PushAccumulator()
            this.Board.Move(move.From, move.To, move.Promotion)
        this.History.Append(this.Board.ZobristHash)
        rv
    member this.UndoMove(rv:byref<RevertMove>) =
        this.Board.UndoMove(&rv)
        NNUE.PullAccumulator()
        this.History.RemoveLast()

module EngineBoard =
    let FromFen(fen:string) = 
        let parts = fen.Split(" ")
        EngineBoard(parts.[0], parts.[1], parts.[2], parts.[3])
    let Default =
        let DEFAULT_FEN = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
        FromFen(DEFAULT_FEN)
