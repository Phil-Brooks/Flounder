namespace FlounderLib
open System

type EngineBoard =
    inherit Board
    val History:RepetitionHistory
    new(boardData, turnData, castlingData, enPassantTargetData) as this = 
        {
            inherit Board(boardData, turnData, castlingData, enPassantTargetData)
            History = new RepetitionHistory()
        } then NNUE.ResetAccumulator();NNUE.RefreshAccumulator(this.Map)
    new(board:EngineBoard) = 
        {
            inherit Board(board.Map)
            History = board.History.Clone()
        }
    static member FromFen(fen:string) = 
        let parts = fen.Split(" ")
        EngineBoard(parts.[0], parts.[1], parts.[2], parts.[3])
    static member Default() = 
        let DEFAULT_FEN = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
        EngineBoard.FromFen(DEFAULT_FEN)
    member this.Clone() = EngineBoard(this)
    member this.PieceOnly(sq:Square) = this.Map.PieceOnly(sq)
    member this.IsRepetition() = 
        this.History.Count(this.ZobristHash) > 1
    member this.GuiMove(from:Square, mto:Square, promotion:Promotion) =
        let moveList = MoveList.WithoutProvidedPins(this, from)
        if not(moveList.Moves.[mto]) then raise (InvalidOperationException("Invalid move provided by GUI."))
        if (promotion <> Promotion.None && not moveList.Promotion) then
            raise (InvalidOperationException("Invalid move provided by GUI."))
        let rv = this.Move(from, mto, promotion)
        this.History.Append(this.ZobristHash)
    member this.NullMove() =
        let rv = RevertNullMove.FromBitBoardMap(&this.Map)
        if (this.Map.EnPassantTarget <> Square.Na) then Zobrist.HashEp(&this.Map.ZobristHash, this.Map.EnPassantTarget)
        this.Map.EnPassantTarget <- Square.Na
        this.Map.ColorToMove <- PieceColor.OppositeColor(this.Map.ColorToMove)
        Zobrist.FlipTurnInHash(&this.Map.ZobristHash)
        rv
    member this.UndoNullMove(rv:RevertNullMove) =
        if (rv.EnPassantTarget <> Square.Na) then
            this.Map.EnPassantTarget <- rv.EnPassantTarget
            Zobrist.HashEp(&this.Map.ZobristHash, rv.EnPassantTarget)
        this.Map.ColorToMove <- PieceColor.OppositeColor(this.Map.ColorToMove)
        Zobrist.FlipTurnInHash(&this.Map.ZobristHash)
    member this.Move(move:byref<OrderedMoveEntry>) =
        let rv:RevertMove =
            NNUE.PushAccumulator()
            this.Move(move.From, move.To, move.Promotion)
        this.History.Append(this.ZobristHash)
        rv
    member this.UndoMove(rv:byref<RevertMove>) =
        base.UndoMove(&rv)
        NNUE.PullAccumulator()
        this.History.RemoveLast()

