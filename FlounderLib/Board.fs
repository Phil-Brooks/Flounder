namespace FlounderLib
open System

type Board =
    val mutable Map:BitBoardMap 
    new(boardData, turnData, castlingData, enPassantTargetData) = 
        let map = new BitBoardMap(boardData, turnData, castlingData, enPassantTargetData)
        Board(map)
    new(map:BitBoardMap) = 
        {
            Map = map
        }
    member this.ColorToMove = this.Map.ColorToMove
    member this.EnPassantTarget = this.Map.EnPassantTarget
    member this.ZobristHash = this.Map.ZobristHash
    member this.Clone() = Board(this.Map.Copy())
    // Readonly Properties
    member this.CastlingRight(color) = 
        if color = PieceColor.White then (this.Map.WhiteQCastle, this.Map.WhiteKCastle) else (this.Map.BlackQCastle, this.Map.BlackKCastle)
    member this.At(sq:Square) = this.Map.[sq]
    member this.All() = this.Map.[PieceColor.White] ||| this.Map.[PieceColor.Black]
    member this.All(color:PieceColor) = this.Map.[color]
    member this.All(piece, color) = this.Map.[piece, color]
    member this.KingLoc(color:PieceColor) = this.Map.[Piece.King, color]
    member this.EnptyAt(sq:Square) = 
        let (pc,_) = this.Map.[sq]
        pc = Piece.Empty
    // Move
    member this.Move(from:Square, mto:Square, ?promotion0:Promotion) =
        let promotion = defaultArg promotion0 Promotion.None
        let (pieceF, colorF) = this.Map.[from]
        let (pieceT, colorT) = this.Map.[mto]
        // Generate a revert move before the map has been altered.
        let mutable rv = RevertMove.FromBitBoardMap(&this.Map)
        if (pieceT <> Piece.Empty) then
            // If piece we're moving to isn't an empty one, we will be capturing.
            // Thus, we need to set it in revert move to ensure we can properly revert it.
            rv.CapturedPiece <- pieceT
            rv.CapturedColor <- colorT
            NNUE.EfficientlyUpdateAccumulator(false, pieceT, colorT, mto)
        if (this.EnPassantTarget = mto && pieceF = Piece.Pawn) then
            // If the attack is an EP attack, we must empty the piece affected by EP.
            let epPieceSq = if colorF = PieceColor.White then Square.FromInt(int(this.EnPassantTarget) - 8) else Square.FromInt(int(this.EnPassantTarget) + 8)
            let oppositeColor = PieceColor.OppositeColor(colorF)
            this.Map.Empty(Piece.Pawn, oppositeColor, epPieceSq)
            NNUE.EfficientlyUpdateAccumulator(false, Piece.Pawn, oppositeColor, epPieceSq)
            // Set it in revert move.
            rv.EnPassant <- true
            // We only need to reference the color.
            rv.CapturedColor <- oppositeColor
        // Update Zobrist.
        if (this.EnPassantTarget<> Square.Na) then Zobrist.HashEp(&this.Map.ZobristHash, this.Map.EnPassantTarget)
        if (pieceF = Piece.Pawn && Math.Abs(int(mto) - int(from)) = 16) then
            // If the pawn push is a 2-push, the square behind it will be EP target.
            this.Map.EnPassantTarget <- if colorF = PieceColor.White then Square.FromInt(int(from) + 8) else Square.FromInt(int(from) - 8)
            // Update Zobrist.
            Zobrist.HashEp(&this.Map.ZobristHash, this.Map.EnPassantTarget)
        else this.Map.EnPassantTarget <- Square.Na
        // Make the move.
        this.Map.Move(pieceF, colorF, pieceT, colorT, from, mto)
        NNUE.EfficientlyUpdateAccumulator(pieceF, colorF, from, mto)
        if (promotion <> Promotion.None) then
            this.Map.Empty(pieceF, colorF, mto)
            this.Map.InsertPiece(Piece.FromInt(int(promotion)), colorF, mto)
            rv.Promotion <- true
            NNUE.EfficientlyUpdateAccumulator(false, pieceF, colorF, mto)
            NNUE.EfficientlyUpdateAccumulator(true, Piece.FromInt(int(promotion)), colorF, mto)
        // Update revert move.
        rv.From <- from
        rv.To <- mto
        // Remove castling rights from hash to allow easy update.
        Zobrist.HashCastlingRights(
            &this.Map.ZobristHash, 
            this.Map.WhiteKCastle, this.Map.WhiteQCastle, 
            this.Map.BlackKCastle, this.Map.BlackQCastle
        )
        // If our rook moved, we must update castling rights.
        if pieceF = Piece.Rook then
            if colorF = PieceColor.White then
                if int(from) % 8 = 0 then this.Map.WhiteQCastle <- 0x0
                if int(from) % 8 = 7 then this.Map.WhiteKCastle <- 0x0
            elif colorF = PieceColor.Black then
                if int(from) % 8 = 0 then this.Map.BlackQCastle <- 0x0
                if int(from) % 8 = 7 then this.Map.BlackKCastle <- 0x0
            else
                raise (InvalidOperationException("Rook cannot have no color."))
        // If our king moved, we also must update castling rights.
        if pieceF = Piece.King then
            if colorF = PieceColor.White then
                this.Map.WhiteQCastle <- 0x0
                this.Map.WhiteKCastle <- 0x0
            elif colorF = PieceColor.Black then
                this.Map.BlackQCastle <- 0x0
                this.Map.BlackKCastle <- 0x0
            else
                raise (InvalidOperationException("King cannot have no color."))
            let d = Math.Abs(int(mto) - int(from))
            if (d = 2) then
                // In the case the king moved to castle, we must also move the rook accordingly,
                // making a secondary move. To ensure proper reverting, we must also update our revert move.
                if (mto > from) then // King-side
                    rv.SecondaryFrom <- Square.FromInt(int(mto) + 1)
                    rv.SecondaryTo <- Square.FromInt(int(mto) - 1)
                else // Queen-side
                    rv.SecondaryFrom <- Square.FromInt(int(mto) - 2)
                    rv.SecondaryTo <- Square.FromInt(int(mto) + 1)
                // Make the secondary move.
                this.Map.Move(
                    Piece.Rook, colorF, Piece.Empty, PieceColor.None, 
                    rv.SecondaryFrom, rv.SecondaryTo
                )
                NNUE.EfficientlyUpdateAccumulator(Piece.Rook, colorF, rv.SecondaryFrom, rv.SecondaryTo)
        // If our rook was captured, we must also update castling rights so we don't castle with enemy piece.
        if pieceT = Piece.Rook then
            if colorT = PieceColor.White then
                if (mto = Square.A1) then this.Map.WhiteQCastle <- 0x0
                if (mto = Square.H1) then this.Map.WhiteKCastle <- 0x0
            elif colorT = PieceColor.Black then
                if (mto = Square.A8) then this.Map.BlackQCastle <- 0x0
                if (mto = Square.H8) then this.Map.BlackKCastle <- 0x0
            else
                raise (InvalidOperationException("Rook cannot have no color."))
        // Re-hash castling rights.
        Zobrist.HashCastlingRights(
            &this.Map.ZobristHash, 
            this.Map.WhiteKCastle, this.Map.WhiteQCastle, 
            this.Map.BlackKCastle, this.Map.BlackQCastle
        )
        // Flip the turn.
        this.Map.ColorToMove <- PieceColor.OppositeColor(this.Map.ColorToMove)
        // Update Zobrist.
        Zobrist.FlipTurnInHash(&this.Map.ZobristHash)
        rv
    member this.UndoMove(rv:byref<RevertMove>)=
        // Remove castling rights from hash to allow easy update.
        Zobrist.HashCastlingRights(
            &this.Map.ZobristHash, 
            this.Map.WhiteKCastle, this.Map.WhiteQCastle, 
            this.Map.BlackKCastle, this.Map.BlackQCastle
        )
        // Revert to old castling rights.
        this.Map.WhiteKCastle <- rv.WhiteKCastle
        this.Map.WhiteQCastle <- rv.WhiteQCastle
        this.Map.BlackKCastle <- rv.BlackKCastle
        this.Map.BlackQCastle <- rv.BlackQCastle
        // Re-hash castling rights.
        Zobrist.HashCastlingRights(
            &this.Map.ZobristHash, 
            this.Map.WhiteKCastle, this.Map.WhiteQCastle, 
            this.Map.BlackKCastle, this.Map.BlackQCastle
        )
        // Update Zobrist.
        if (this.Map.EnPassantTarget <> Square.Na) then
            Zobrist.HashEp(&this.Map.ZobristHash, this.Map.EnPassantTarget)
        // Revert to the previous EP target.
        this.Map.EnPassantTarget <- rv.EnPassantTarget
        if (this.Map.EnPassantTarget <> Square.Na) then 
            // If we don't have an empty EP, we should hash it in.
            Zobrist.HashEp(&this.Map.ZobristHash, this.Map.EnPassantTarget)
        // Revert to previous turn.
        this.Map.ColorToMove <- rv.ColorToMove
        Zobrist.FlipTurnInHash(&this.Map.ZobristHash)
        if (rv.Promotion) then
            let (piece, color) = this.Map.[rv.To]
            this.Map.Empty(piece, color, rv.To)
            this.Map.InsertPiece(Piece.Pawn, color, rv.To)
        let (pF, cF) = this.Map.[rv.To]
        let (pT, cT) = this.Map.[rv.From]
        // Undo the move by moving the piece back.
        this.Map.Move(pF, cF, pT, cT, rv.To, rv.From)
        if (rv.EnPassant) then
            // If it was an EP attack, we must insert a pawn at the affected square.
            let insertion = if rv.CapturedColor = PieceColor.White then Square.FromInt(int(rv.To) + 8) else Square.FromInt(int(rv.To) - 8)
            this.Map.InsertPiece(Piece.Pawn, rv.CapturedColor, insertion)
        elif (rv.CapturedPiece <> Piece.Empty) then
            // If a capture happened, we must insert the piece at the relevant square.
            this.Map.InsertPiece(rv.CapturedPiece, rv.CapturedColor, rv.To)
        // If there was a secondary move (castling), revert the secondary move.
        elif (rv.SecondaryFrom <> Square.Na) then this.Map.Move(rv.SecondaryTo, rv.SecondaryFrom)
    // Insert/Remove
    member this.InsertPiece(piece, color, sq) = 
        this.Map.InsertPiece(piece, color, sq)
    member this.RemovePiece(piece, color, sq) =
        this.Map.Empty(piece, color, sq)
    override this.ToString() =
        "FEN: " + this.GenerateFen() + "\nHash: " + $"{this.Map.ZobristHash:X}" + "\n"
    member this.GenerateFen() =
        let boardData = this.Map.GenerateBoardFen()
        let turnData = if this.ColorToMove = PieceColor.White then "w" else "b"
        let mutable castlingRight = ""
        if (this.Map.WhiteKCastle = 0x0 && this.Map.WhiteQCastle = 0x0 && this.Map.BlackKCastle = 0x0 && this.Map.BlackQCastle = 0x0) then
            castlingRight <- "-"
        else    
            if (this.Map.WhiteKCastle <> 0x0) then castlingRight <- castlingRight + "K"
            if (this.Map.WhiteQCastle <> 0x0) then castlingRight <- castlingRight + "Q"
            if (this.Map.BlackKCastle <> 0x0) then castlingRight <- castlingRight + "k"
            if (this.Map.BlackQCastle <> 0x0) then castlingRight <- castlingRight + "q"
        let mutable enPassantTarget = "-"
        if (this.EnPassantTarget <> Square.Na) then
            enPassantTarget <- this.EnPassantTarget.ToString().ToLower()
        let fen = [| boardData; turnData; castlingRight; enPassantTarget |]
        fen|>Array.reduce (fun a b -> a + " " + b)
module Board =
    let FromFen(fen:string) = 
        let parts = fen.Split(" ")
        Board(parts.[0], parts.[1], parts.[2], parts.[3])
    let Default() = 
        let DEFAULT_FEN = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
        FromFen(DEFAULT_FEN)
