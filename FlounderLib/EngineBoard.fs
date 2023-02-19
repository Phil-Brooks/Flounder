namespace FlounderLib
open System

type EngineBoard =
    val mutable RepHist:RepetitionHistory
    val mutable Brd:Board
    new(boardData, turnData, castlingData, enPassantTargetData) as this = 
        {
            Brd = Board(boardData, turnData, castlingData, enPassantTargetData)
            RepHist = RepetitionHistory.Default()
        } then NNUE.ResetAccumulator();NNUE.RefreshAccumulator(this.Brd.Map)
    new(board:EngineBoard) = 
        {
            Brd = Board(board.Brd.Map)
            RepHist = board.RepHist.Clone()
        }
    member this.Clone() = EngineBoard(this)
    member this.PieceOnly(sq:Square) = this.Brd.Map.PieceOnly(sq)
    member this.IsRepetition() = 
        this.RepHist.Count(this.Brd.ZobristHash) > 1
    member this.GuiMove(from:Square, mto:Square, promotion:Promotion) =
        let moveList = MoveList(this.Brd, from)
        if not(moveList.Moves.[mto]) then raise (InvalidOperationException("Invalid move provided by GUI."))
        if (promotion <> Promotion.None && not moveList.Promotion) then
            raise (InvalidOperationException("Invalid move provided by GUI."))
        let rv = this.Brd.Move(from, mto, promotion)
        this.RepHist.Append(this.Brd.ZobristHash)
    member this.NullMove() =
        let rv = this.Brd.Map.EnPassantTarget
        if (this.Brd.Map.EnPassantTarget <> Square.Na) then Zobrist.HashEp(&this.Brd.Map.ZobristHash, this.Brd.Map.EnPassantTarget)
        this.Brd.Map.EnPassantTarget <- Square.Na
        this.Brd.Map.ColorToMove <- PieceColor.OppositeColor(this.Brd.Map.ColorToMove)
        Zobrist.FlipTurnInHash(&this.Brd.Map.ZobristHash)
        rv
    member this.UndoNullMove(rv:Square) =
        if (rv <> Square.Na) then
            this.Brd.Map.EnPassantTarget <- rv
            Zobrist.HashEp(&this.Brd.Map.ZobristHash, rv)
        this.Brd.Map.ColorToMove <- PieceColor.OppositeColor(this.Brd.Map.ColorToMove)
        Zobrist.FlipTurnInHash(&this.Brd.Map.ZobristHash)
    member this.Move(move:byref<OrderedMoveEntry>) =
        let rv:RevertMove =
            NNUE.PushAccumulator()
            this.Brd.Move(move.From, move.To, move.Promotion)
        this.RepHist.Append(this.Brd.ZobristHash)
        rv
    member this.UndoMove(rv:byref<RevertMove>) =
        this.Brd.UndoMove(&rv)
        NNUE.PullAccumulator()
        this.RepHist.RemoveLast()
module EngineBoard =
    let FromFen(fen:string) = 
        let parts = fen.Split(" ")
        EngineBoard(parts.[0], parts.[1], parts.[2], parts.[3])
    let Default() = 
        let DEFAULT_FEN = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
        FromFen(DEFAULT_FEN)
