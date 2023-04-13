namespace FlounderLib
open System

type Board =
    val mutable Map:BoardRec
    new(boardData:string, turnData, castlingData, enPassantTargetData) = 
        let map = BitBoardMap.FromParts(boardData, turnData, castlingData, enPassantTargetData)
        Board(map)
    new(map:BoardRec) = 
        {
            Map = map
        }
    member this.All(piece, color) = this.Map.Pieces[piece*2 + color]
    member this.KingLoc(color:int) = if color= White then this.Map.Pieces[WhiteKing] else this.Map.Pieces[BlackKing]
    member this.EnptyAt(sq:int) = 
        let pc = this.Map.Squares[sq]
        pc = EmptyColPc
    // Move
    member this.Move(from:int, mto:int, ?promotion0:int) =
        let promotion = defaultArg promotion0 PromNone
        let cpcF = this.Map.Squares[from]
        let cpcT = this.Map.Squares[mto]
        let pieceF, colorF = ColPiece.ToPcCol(cpcF)
        let pieceT, colorT = ColPiece.ToPcCol(cpcT)
        // Generate a revert move before the map has been altered.
        let mutable rv = BitBoardMap.ToMove(&this.Map)
        if pieceT <> EmptyPc then
            // If piece we're moving to isn't an empty one, we will be capturing.
            // Thus, we need to set it in revert move to ensure we can properly revert it.
            rv.CapturedPiece <- cpcT
        if (this.Map.EnPassantTarget = mto && pieceF = Pawn) then
            // If the attack is an EP attack, we must empty the piece affected by EP.
            let epPieceSq = if colorF = 0 then this.Map.EnPassantTarget + 8 else this.Map.EnPassantTarget - 8
            let oppositeColor = colorF^^^1
            let eppc = if colorF=0 then BlackPawn else WhitePawn
            BitBoardMap.Empty(&this.Map, epPieceSq)
            // Set it in revert move.
            rv.EnPassant <- true
            // We only need to reference the color.
            rv.CapturedPiece <- oppositeColor
        // Update Zobrist.
        if this.Map.EnPassantTarget<> Na then Zobrist.HashEp(&this.Map.ZobristHash, this.Map.EnPassantTarget)
        if (pieceF = Pawn && Math.Abs(int(mto) - int(from)) = 16) then
            // If the pawn push is a 2-push, the square behind it will be EP target.
            this.Map.EnPassantTarget <- if colorF = 0 then from - 8 else from + 8
            // Update Zobrist.
            Zobrist.HashEp(&this.Map.ZobristHash, this.Map.EnPassantTarget)
        else this.Map.EnPassantTarget <- Na
        // Make the move.
        BitBoardMap.Move(&this.Map, from, mto)
        if promotion <> PromNone then
            BitBoardMap.Empty(&this.Map, mto)
            let prompc = promotion*2 + colorF
            BitBoardMap.InsertPiece(&this.Map, prompc, mto)
            rv.Promotion <- true
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
        if pieceF = Rook then
            if colorF = 0 then
                if from % 8 = 0 then this.Map.WhiteQCastle <- 0x0
                if from % 8 = 7 then this.Map.WhiteKCastle <- 0x0
            elif colorF = 1 then
                if from % 8 = 0 then this.Map.BlackQCastle <- 0x0
                if from % 8 = 7 then this.Map.BlackKCastle <- 0x0
            else
                raise (InvalidOperationException("Rook cannot have no color."))
        // If our king moved, we also must update castling rights.
        if pieceF = King then
            if colorF = 0 then
                this.Map.WhiteQCastle <- 0x0
                this.Map.WhiteKCastle <- 0x0
            elif colorF = 1 then
                this.Map.BlackQCastle <- 0x0
                this.Map.BlackKCastle <- 0x0
            else
                raise (InvalidOperationException("King cannot have no color."))
            let d = abs(mto - from)
            if (d = 2) then
                // In the case the king moved to castle, we must also move the rook accordingly,
                // making a secondary move. To ensure proper reverting, we must also update our revert move.
                if (mto > from) then // King-side
                    rv.SecondaryFrom <- mto + 1
                    rv.SecondaryTo <- mto - 1
                else // Queen-side
                    rv.SecondaryFrom <- mto - 2
                    rv.SecondaryTo <- mto + 1
                // Make the secondary move.
                BitBoardMap.Move(&this.Map, rv.SecondaryFrom, rv.SecondaryTo)
        // If our rook was captured, we must also update castling rights so we don't castle with enemy piece.
        if pieceT = Rook then
            if colorT = 0 then
                if mto = A1 then this.Map.WhiteQCastle <- 0x0
                if mto = H1 then this.Map.WhiteKCastle <- 0x0
            elif colorT = 1 then
                if mto = A8 then this.Map.BlackQCastle <- 0x0
                if mto = H8 then this.Map.BlackKCastle <- 0x0
            else
                raise (InvalidOperationException("Rook cannot have no color."))
        // Re-hash castling rights.
        Zobrist.HashCastlingRights(
            &this.Map.ZobristHash, 
            this.Map.WhiteKCastle, this.Map.WhiteQCastle, 
            this.Map.BlackKCastle, this.Map.BlackQCastle
        )
        // Flip the turn.
        this.Map.IsWtm <- not this.Map.IsWtm  
        this.Map.Stm <- this.Map.Stm ^^^ 1  
        this.Map.Xstm <- this.Map.Xstm ^^^ 1  
        // Update Zobrist.
        Zobrist.FlipTurnInHash(&this.Map.ZobristHash)
        rv
    member this.UndoMove(rv:byref<MoveRec>)=
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
        if this.Map.EnPassantTarget <> Na then
            Zobrist.HashEp(&this.Map.ZobristHash, this.Map.EnPassantTarget)
        // Revert to the previous EP target.
        this.Map.EnPassantTarget <- rv.EnPassantTarget
        if this.Map.EnPassantTarget <> Na then 
            // If we don't have an empty EP, we should hash it in.
            Zobrist.HashEp(&this.Map.ZobristHash, this.Map.EnPassantTarget)
        // Revert to previous turn.
        this.Map.IsWtm <- not this.Map.IsWtm
        this.Map.Stm <- this.Map.Stm ^^^ 1  
        this.Map.Xstm <- this.Map.Xstm ^^^ 1  
        Zobrist.FlipTurnInHash(&this.Map.ZobristHash)
        if rv.Promotion then
            let color = this.Map.Squares.[rv.To]%2
            BitBoardMap.Empty(&this.Map, rv.To)
            BitBoardMap.InsertPiece(&this.Map, color, rv.To)
        let pF = this.Map.Squares[rv.To]
        let pT = this.Map.Squares[rv.From]
        // Undo the move by moving the piece back.
        BitBoardMap.Move(&this.Map, rv.To, rv.From)
        if rv.EnPassant then
            // If it was an EP attack, we must insert a pawn at the affected square.
            let insertion = if rv.CapturedPiece = WhitePawn then rv.To - 8 else rv.To + 8
            BitBoardMap.InsertPiece(&this.Map, rv.CapturedPiece, insertion)
        elif rv.CapturedPiece <> EmptyColPc then
            // If a capture happened, we must insert the piece at the relevant square.
            BitBoardMap.InsertPiece(&this.Map, rv.CapturedPiece, rv.To)
        // If there was a secondary move (castling), revert the secondary move.
        elif rv.SecondaryFrom <> Na then BitBoardMap.Move(&this.Map, rv.SecondaryTo, rv.SecondaryFrom) 
    // Insert/Remove
    member this.InsertPiece(cpc, sq) = 
        BitBoardMap.InsertPiece(&this.Map, cpc, sq)
    member this.RemovePiece(cpc, sq) =
        BitBoardMap.Empty(&this.Map, sq)
    override this.ToString() =
        "FEN: " + this.GenerateFen() + "\nHash: " + $"{this.Map.ZobristHash:X}" + "\n"
    member this.GenerateFen() =
        let boardData =  BitBoardMap.GenerateBoardFen(this.Map)
        let turnData = if this.Map.IsWtm then "w" else "b"
        let mutable castlingRight = ""
        if (this.Map.WhiteKCastle = 0x0 && this.Map.WhiteQCastle = 0x0 && this.Map.BlackKCastle = 0x0 && this.Map.BlackQCastle = 0x0) then
            castlingRight <- "-"
        else    
            if (this.Map.WhiteKCastle <> 0x0) then castlingRight <- castlingRight + "K"
            if (this.Map.WhiteQCastle <> 0x0) then castlingRight <- castlingRight + "Q"
            if (this.Map.BlackKCastle <> 0x0) then castlingRight <- castlingRight + "k"
            if (this.Map.BlackQCastle <> 0x0) then castlingRight <- castlingRight + "q"
        let mutable enPassantTarget = "-"
        if this.Map.EnPassantTarget <> Na then
            enPassantTarget <- this.Map.EnPassantTarget.ToString().ToLower()
        let fen = [| boardData; turnData; castlingRight; enPassantTarget |]
        fen|>Array.reduce (fun a b -> a + " " + b)
module Board =
    let FromFen(fen:string) = 
        let parts = fen.Split(" ")
        Board(parts.[0], parts.[1], parts.[2], parts.[3])
    let Default() = 
        let DEFAULT_FEN = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
        FromFen(DEFAULT_FEN)
