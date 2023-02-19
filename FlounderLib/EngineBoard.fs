namespace FlounderLib
open System

type EngineBoard =
    val mutable History:RepetitionHistory
    val mutable IBoard:Board
    new(boardData, turnData, castlingData, enPassantTargetData) as this = 
        {
            IBoard = Board(boardData, turnData, castlingData, enPassantTargetData)
            History = new RepetitionHistory(1024)
        } then NNUE.ResetAccumulator();NNUE.RefreshAccumulator(this.IBoard.Map)
    new(board:EngineBoard) = 
        {
            IBoard = Board(board.IBoard.Map)
            History = board.History.Clone()
        }
    member this.Clone() = EngineBoard(this)
    member this.PieceOnly(sq:Square) = this.IBoard.Map.PieceOnly(sq)
    member this.IsRepetition() = 
        this.History.Count(this.IBoard.ZobristHash) > 1
    member this.GuiMove(from:Square, mto:Square, promotion:Promotion) =
        let moveList = MoveList(this.IBoard, from)
        if not(moveList.Moves.[mto]) then raise (InvalidOperationException("Invalid move provided by GUI."))
        if (promotion <> Promotion.None && not moveList.Promotion) then
            raise (InvalidOperationException("Invalid move provided by GUI."))
        let rv = this.IBoard.Move(from, mto, promotion)
        this.History.Append(this.IBoard.ZobristHash)
    member this.NullMove() =
        let rv = this.IBoard.Map.EnPassantTarget
        if (this.IBoard.Map.EnPassantTarget <> Square.Na) then Zobrist.HashEp(&this.IBoard.Map.ZobristHash, this.IBoard.Map.EnPassantTarget)
        this.IBoard.Map.EnPassantTarget <- Square.Na
        this.IBoard.Map.ColorToMove <- PieceColor.OppositeColor(this.IBoard.Map.ColorToMove)
        Zobrist.FlipTurnInHash(&this.IBoard.Map.ZobristHash)
        rv
    member this.UndoNullMove(rv:Square) =
        if (rv <> Square.Na) then
            this.IBoard.Map.EnPassantTarget <- rv
            Zobrist.HashEp(&this.IBoard.Map.ZobristHash, rv)
        this.IBoard.Map.ColorToMove <- PieceColor.OppositeColor(this.IBoard.Map.ColorToMove)
        Zobrist.FlipTurnInHash(&this.IBoard.Map.ZobristHash)
    member this.Move(move:byref<OrderedMoveEntry>) =
        let rv:RevertMove =
            NNUE.PushAccumulator()
            this.IBoard.Move(move.From, move.To, move.Promotion)
        this.History.Append(this.IBoard.ZobristHash)
        rv
    member this.UndoMove(rv:byref<RevertMove>) =
        this.IBoard.UndoMove(&rv)
        NNUE.PullAccumulator()
        this.History.RemoveLast()
module EngineBoard =
    let FromFen(fen:string) = 
        let parts = fen.Split(" ")
        EngineBoard(parts.[0], parts.[1], parts.[2], parts.[3])
    let Default() = 
        let DEFAULT_FEN = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
        FromFen(DEFAULT_FEN)
