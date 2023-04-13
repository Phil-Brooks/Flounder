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
module Board =
    let Move(brd:byref<Board>, from:int, mto:int, promotion:int) =
        let cpcF = brd.Map.Squares[from]
        let cpcT = brd.Map.Squares[mto]
        let pieceF, colorF = ColPiece.ToPcCol(cpcF)
        let pieceT, colorT = ColPiece.ToPcCol(cpcT)
        // Generate a revert move before the map has been altered.
        let mutable rv = BitBoardMap.ToMove(&brd.Map)
        if pieceT <> EmptyPc then
            // If piece we're moving to isn't an empty one, we will be capturing.
            // Thus, we need to set it in revert move to ensure we can properly revert it.
            rv.CapturedPiece <- cpcT
        if (brd.Map.EnPassantTarget = mto && pieceF = Pawn) then
            // If the attack is an EP attack, we must empty the piece affected by EP.
            let epPieceSq = if colorF = 0 then brd.Map.EnPassantTarget + 8 else brd.Map.EnPassantTarget - 8
            let oppositeColor = colorF^^^1
            let eppc = if colorF=0 then BlackPawn else WhitePawn
            BitBoardMap.Empty(&brd.Map, epPieceSq)
            // Set it in revert move.
            rv.EnPassant <- true
            // We only need to reference the color.
            rv.CapturedPiece <- oppositeColor
        // Update Zobrist.
        if brd.Map.EnPassantTarget<> Na then Zobrist.HashEp(&brd.Map.ZobristHash, brd.Map.EnPassantTarget)
        if (pieceF = Pawn && Math.Abs(int(mto) - int(from)) = 16) then
            // If the pawn push is a 2-push, the square behind it will be EP target.
            brd.Map.EnPassantTarget <- if colorF = 0 then from - 8 else from + 8
            // Update Zobrist.
            Zobrist.HashEp(&brd.Map.ZobristHash, brd.Map.EnPassantTarget)
        else brd.Map.EnPassantTarget <- Na
        // Make the move.
        BitBoardMap.Move(&brd.Map, from, mto)
        if promotion <> PromNone then
            BitBoardMap.Empty(&brd.Map, mto)
            let prompc = promotion*2 + colorF
            BitBoardMap.InsertPiece(&brd.Map, prompc, mto)
            rv.Promotion <- true
        // Update revert move.
        rv.From <- from
        rv.To <- mto
        // Remove castling rights from hash to allow easy update.
        Zobrist.HashCastlingRights(
            &brd.Map.ZobristHash, 
            brd.Map.WhiteKCastle, brd.Map.WhiteQCastle, 
            brd.Map.BlackKCastle, brd.Map.BlackQCastle
        )
        // If our rook moved, we must update castling rights.
        if pieceF = Rook then
            if colorF = 0 then
                if from % 8 = 0 then brd.Map.WhiteQCastle <- 0x0
                if from % 8 = 7 then brd.Map.WhiteKCastle <- 0x0
            elif colorF = 1 then
                if from % 8 = 0 then brd.Map.BlackQCastle <- 0x0
                if from % 8 = 7 then brd.Map.BlackKCastle <- 0x0
            else
                raise (InvalidOperationException("Rook cannot have no color."))
        // If our king moved, we also must update castling rights.
        if pieceF = King then
            if colorF = 0 then
                brd.Map.WhiteQCastle <- 0x0
                brd.Map.WhiteKCastle <- 0x0
            elif colorF = 1 then
                brd.Map.BlackQCastle <- 0x0
                brd.Map.BlackKCastle <- 0x0
            else
                failwith "King cannot have no color."
            let d = abs(mto - from)
            if d = 2 then
                // In the case the king moved to castle, we must also move the rook accordingly,
                // making a secondary move. To ensure proper reverting, we must also update our revert move.
                if (mto > from) then // King-side
                    rv.SecondaryFrom <- mto + 1
                    rv.SecondaryTo <- mto - 1
                else // Queen-side
                    rv.SecondaryFrom <- mto - 2
                    rv.SecondaryTo <- mto + 1
                // Make the secondary move.
                BitBoardMap.Move(&brd.Map, rv.SecondaryFrom, rv.SecondaryTo)
        // If our rook was captured, we must also update castling rights so we don't castle with enemy piece.
        if pieceT = Rook then
            if colorT = 0 then
                if mto = A1 then brd.Map.WhiteQCastle <- 0x0
                if mto = H1 then brd.Map.WhiteKCastle <- 0x0
            elif colorT = 1 then
                if mto = A8 then brd.Map.BlackQCastle <- 0x0
                if mto = H8 then brd.Map.BlackKCastle <- 0x0
            else
                raise (InvalidOperationException("Rook cannot have no color."))
        // Re-hash castling rights.
        Zobrist.HashCastlingRights(
            &brd.Map.ZobristHash, 
            brd.Map.WhiteKCastle, brd.Map.WhiteQCastle, 
            brd.Map.BlackKCastle, brd.Map.BlackQCastle
        )
        // Flip the turn.
        brd.Map.IsWtm <- not brd.Map.IsWtm  
        brd.Map.Stm <- brd.Map.Stm ^^^ 1  
        brd.Map.Xstm <- brd.Map.Xstm ^^^ 1  
        // Update Zobrist.
        Zobrist.FlipTurnInHash(&brd.Map.ZobristHash)
        rv
    let UndoMove(brd:byref<Board>, rv:byref<MoveRec>)=
        // Remove castling rights from hash to allow easy update.
        Zobrist.HashCastlingRights(
            &brd.Map.ZobristHash, 
            brd.Map.WhiteKCastle, brd.Map.WhiteQCastle, 
            brd.Map.BlackKCastle, brd.Map.BlackQCastle
        )
        // Revert to old castling rights.
        brd.Map.WhiteKCastle <- rv.WhiteKCastle
        brd.Map.WhiteQCastle <- rv.WhiteQCastle
        brd.Map.BlackKCastle <- rv.BlackKCastle
        brd.Map.BlackQCastle <- rv.BlackQCastle
        // Re-hash castling rights.
        Zobrist.HashCastlingRights(
            &brd.Map.ZobristHash, 
            brd.Map.WhiteKCastle, brd.Map.WhiteQCastle, 
            brd.Map.BlackKCastle, brd.Map.BlackQCastle
        )
        // Update Zobrist.
        if brd.Map.EnPassantTarget <> Na then
            Zobrist.HashEp(&brd.Map.ZobristHash, brd.Map.EnPassantTarget)
        // Revert to the previous EP target.
        brd.Map.EnPassantTarget <- rv.EnPassantTarget
        if brd.Map.EnPassantTarget <> Na then 
            // If we don't have an empty EP, we should hash it in.
            Zobrist.HashEp(&brd.Map.ZobristHash, brd.Map.EnPassantTarget)
        // Revert to previous turn.
        brd.Map.IsWtm <- not brd.Map.IsWtm
        brd.Map.Stm <- brd.Map.Stm ^^^ 1  
        brd.Map.Xstm <- brd.Map.Xstm ^^^ 1  
        Zobrist.FlipTurnInHash(&brd.Map.ZobristHash)
        if rv.Promotion then
            let color = brd.Map.Squares.[rv.To]%2
            BitBoardMap.Empty(&brd.Map, rv.To)
            BitBoardMap.InsertPiece(&brd.Map, color, rv.To)
        let pF = brd.Map.Squares[rv.To]
        let pT = brd.Map.Squares[rv.From]
        // Undo the move by moving the piece back.
        BitBoardMap.Move(&brd.Map, rv.To, rv.From)
        if rv.EnPassant then
            // If it was an EP attack, we must insert a pawn at the affected square.
            let insertion = if rv.CapturedPiece = WhitePawn then rv.To - 8 else rv.To + 8
            BitBoardMap.InsertPiece(&brd.Map, rv.CapturedPiece, insertion)
        elif rv.CapturedPiece <> EmptyColPc then
            // If a capture happened, we must insert the piece at the relevant square.
            BitBoardMap.InsertPiece(&brd.Map, rv.CapturedPiece, rv.To)
        // If there was a secondary move (castling), revert the secondary move.
        elif rv.SecondaryFrom <> Na then BitBoardMap.Move(&brd.Map, rv.SecondaryTo, rv.SecondaryFrom) 

    let GenerateFen(brd:Board) =
        let map = brd.Map
        let expandedBoardData:string array = Array.zeroCreate 8
        for v = 0 to 7 do
            let mutable rankData = ""
            let mutable h = 0
            while h < 8 do
                let sq = v * 8 + h
                let cpc = map.Squares[sq]
                if cpc = EmptyColPc then
                    let mutable c = 1
                    let mutable fnd = false
                    for i = h + 1 to 7 do
                        if not fnd then
                            let sq = v * 8 + i
                            let pc = map.Squares[sq]
                            if pc = EmptyColPc then c <- c + 1
                            else fnd <- true
                    rankData <- rankData + c.ToString()
                    h <- h + c
                else
                    let input = ColPiece.ToStr(cpc)
                    rankData <- rankData + input
                    h <- h + 1
            expandedBoardData[v] <- rankData
        let boardData = String.Join("/", expandedBoardData)
        let turnData = if brd.Map.IsWtm then "w" else "b"
        let mutable castlingRight = ""
        if (brd.Map.WhiteKCastle = 0x0 && brd.Map.WhiteQCastle = 0x0 && brd.Map.BlackKCastle = 0x0 && brd.Map.BlackQCastle = 0x0) then
            castlingRight <- "-"
        else    
            if (brd.Map.WhiteKCastle <> 0x0) then castlingRight <- castlingRight + "K"
            if (brd.Map.WhiteQCastle <> 0x0) then castlingRight <- castlingRight + "Q"
            if (brd.Map.BlackKCastle <> 0x0) then castlingRight <- castlingRight + "k"
            if (brd.Map.BlackQCastle <> 0x0) then castlingRight <- castlingRight + "q"
        let mutable enPassantTarget = "-"
        if brd.Map.EnPassantTarget <> Na then
            enPassantTarget <- brd.Map.EnPassantTarget.ToString().ToLower()
        let fen = [| boardData; turnData; castlingRight; enPassantTarget |]
        fen|>Array.reduce (fun a b -> a + " " + b)
    let FromFen(fen:string) = 
        let parts = fen.Split(" ")
        Board(parts.[0], parts.[1], parts.[2], parts.[3])
    let Default() = 
        let DEFAULT_FEN = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
        FromFen(DEFAULT_FEN)
