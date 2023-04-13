namespace FlounderLib
open System.IO
open System

module BitBoardMap =
    let FromParts(boardFen:string, turnData:string, castlingData:string, enPassantTargetData:string) =
        let pieces = Array.zeroCreate 12
        let squares:int array = Array.zeroCreate 64
        for i = 0 to 63 do
            squares[i] <- EmptyColPc
        let expandedBoardData = boardFen.Split("/")
        if expandedBoardData.Length <> 8 then 
            failwith ("Wrong board data provided: " + boardFen)
        let mutable wkloc = -1
        let mutable bkloc = -1
        for v = 0 to 7 do
            let rankData = expandedBoardData[v]
            let mutable h = 0
            for p in rankData do
                if (Char.IsNumber(p)) then
                    h <- h + int(p.ToString())
                else
                    if (Char.IsUpper(p)) then
                        if p = 'P' then
                            Bits.SetBit(&pieces[WhitePawn], v * 8 + h)
                            squares[v * 8 + h] <- WhitePawn
                        elif p = 'N' then
                            Bits.SetBit(&pieces[WhiteKnight], v * 8 + h)
                            squares[v * 8 + h] <- WhiteKnight
                        elif p = 'B' then
                            Bits.SetBit(&pieces[WhiteBishop], v * 8 + h)
                            squares[v * 8 + h] <- WhiteBishop
                        elif p = 'R' then
                            Bits.SetBit(&pieces[WhiteRook], v * 8 + h)
                            squares[v * 8 + h] <- WhiteRook
                        elif p = 'Q' then
                            Bits.SetBit(&pieces[WhiteQueen], v * 8 + h)
                            squares[v * 8 + h] <- WhiteQueen
                        elif p = 'K' then
                            Bits.SetBit(&pieces[WhiteKing], v * 8 + h)
                            squares[v * 8 + h] <- WhiteKing
                            wkloc <- v * 8 + h 
                    else
                        if p = 'p' then
                            Bits.SetBit(&pieces[BlackPawn], v * 8 + h)
                            squares[v * 8 + h] <- BlackPawn
                        elif p = 'n' then
                            Bits.SetBit(&pieces[BlackKnight], v * 8 + h)
                            squares[v * 8 + h] <- BlackKnight
                        elif p = 'b' then
                            Bits.SetBit(&pieces[BlackBishop], v * 8 + h)
                            squares[v * 8 + h] <- BlackBishop
                        elif p = 'r' then
                            Bits.SetBit(&pieces[BlackRook], v * 8 + h)
                            squares[v * 8 + h] <- BlackRook
                        elif p = 'q' then
                            Bits.SetBit(&pieces[BlackQueen], v * 8 + h)
                            squares[v * 8 + h] <- BlackQueen
                        elif p = 'k' then
                            Bits.SetBit(&pieces[BlackKing], v * 8 + h)
                            squares[v * 8 + h] <- BlackKing
                            bkloc <- v * 8 + h 
                    h <- h + 1
        let white = pieces[WhitePawn] ||| pieces[WhiteKnight] ||| 
                    pieces[WhiteBishop] ||| pieces[WhiteRook] ||| 
                    pieces[WhiteQueen] ||| pieces[WhiteKing] 
        let black = pieces[BlackPawn] ||| pieces[BlackKnight] ||| 
                    pieces[BlackBishop] ||| pieces[BlackRook] ||| 
                    pieces[BlackQueen] ||| pieces[BlackKing] 
        let both = white ||| black
        let isWtm = turnData[0] = 'w'
        let stm = if isWtm then White else Black
        let xstm = if isWtm then Black else White
        let whiteKCastle = if castlingData.Contains('K') then 0x1 else 0x0
        let whiteQCastle = if castlingData.Contains('Q') then 0x2 else 0x0
        let blackKCastle = if castlingData.Contains('k') then 0x4 else 0x0
        let blackQCastle = if castlingData.Contains('q') then 0x8 else 0x0
        let mutable enPassantTarget = Na
        if enPassantTargetData.Length = 2 then
            enPassantTarget <- Square.FromStr(enPassantTargetData)
        let mutable ans =
            {
                IsWtm = isWtm
                Stm = stm
                Xstm = xstm
                Pieces = pieces
                Squares = squares
                WhiteKingLoc = wkloc
                BlackKingLoc = bkloc
                White = white
                Black = black
                Both = both
                WhiteKCastle = whiteKCastle
                WhiteQCastle = whiteQCastle 
                BlackKCastle = blackKCastle
                BlackQCastle = blackQCastle
                EnPassantTarget = enPassantTarget
                ZobristHash = 0UL
            }
        ans.ZobristHash <- Zobrist.Hash(ans)
        ans
    let ToMove(map:byref<BoardRec>) =
        {
            WhiteKCastle = map.WhiteKCastle
            WhiteQCastle = map.WhiteQCastle
            BlackKCastle = map.BlackKCastle
            BlackQCastle = map.BlackQCastle
            EnPassantTarget = map.EnPassantTarget
            Promotion = false
            EnPassant = false
            From = Na
            To = Na
            CapturedPiece = EmptyColPc
            SecondaryFrom = Na
            SecondaryTo = Na
        }
    let Move(map:byref<BoardRec>, from:int, mto:int) =
        let pF = map.Squares[from]
        let pT = map.Squares[mto]
        let cT = pT%2
        let cF = pF%2
        if pT <> EmptyColPc then
            // If moving to piece isn't empty, then we capture.
            Bits.PopBit(&map.Pieces[pT], mto)
            // Remove from color bitboards.
            if cT = White then
                Bits.PopBit(&map.White, mto)
            else
                Bits.PopBit(&map.Black, mto)
            Bits.PopBit(&map.Both, mto)
            // Update Zobrist.
            Zobrist.HashPiece(&map.ZobristHash, pT, mto)
        // We remove from original square.
        Bits.PopBit(&map.Pieces[pF], from)
        // Set at next square.
        Bits.SetBit(&map.Pieces[pF], mto)
        // Make sure to update the pieces and colors.
        map.Squares[mto] <- map.Squares[from]
        map.Squares[from] <- EmptyColPc
        //update king locations
        if map.Squares[mto] = WhiteKing then map.WhiteKingLoc <- mto
        if map.Squares[mto] = BlackKing then map.BlackKingLoc <- mto
        // Update color bitboards.
        if cF = White then
            Bits.PopBit(&map.White, from)
            Bits.SetBit(&map.White, mto)
        else 
            Bits.PopBit(&map.Black, from)
            Bits.SetBit(&map.Black, mto)
        Bits.PopBit(&map.Both, from)
        Bits.SetBit(&map.Both, mto)
        // Update Zobrist.
        Zobrist.HashPiece(&map.ZobristHash, pF, from)
        Zobrist.HashPiece(&map.ZobristHash, pF, mto)
    let Empty(map:byref<BoardRec>, sq:int) =
        let cpc = map.Squares[sq]
        let color = cpc%2
        // Remove from square.
        Bits.PopBit(&map.Pieces[cpc], sq)
        // Set empty in pieces and colors.
        map.Squares[sq] <- EmptyColPc
        // Remove from color bitboards.
        if color = White then
            Bits.PopBit(&map.White, sq)
        else 
            Bits.PopBit(&map.Black, sq)
        Bits.PopBit(&map.Both, sq)
        // Update Zobrist.
        Zobrist.HashPiece(&map.ZobristHash, cpc, sq)
    let InsertPiece(map:byref<BoardRec>, cpc:int, sq:int) =
        let color = cpc%2
        // Insert the piece at square.
        Bits.SetBit(&map.Pieces[cpc], sq)
        // Insert into color bitboards.
        if color = White then
            Bits.SetBit(&map.White, sq)
        else 
            Bits.SetBit(&map.Black, sq)
        Bits.SetBit(&map.Both, sq)
        // Set piece in pieces and colors.
        map.Squares[sq] <- cpc
        if map.Squares[sq] = WhiteKing then map.WhiteKingLoc <- sq
        if map.Squares[sq] = BlackKing then map.BlackKingLoc <- sq
        // Update Zobrist.
        Zobrist.HashPiece(&map.ZobristHash, cpc, sq)
    let GenerateBoardFen(map:BoardRec) =
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
        String.Join("/", expandedBoardData)
