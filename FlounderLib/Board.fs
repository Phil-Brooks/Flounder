namespace FlounderLib
open System

module Board =
    let FromFen(fen:string) = 
        let parts = fen.Split(" ")
        let boardFen = parts[0]
        let turnData = parts[1]
        let castlingData = parts[2]
        let enPassantTargetData = parts[3]
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
    let Default() = 
        let DEFAULT_FEN = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
        FromFen(DEFAULT_FEN)
    let ToMove() =
        {
            WhiteKCastle = Brd.WhiteKCastle
            WhiteQCastle = Brd.WhiteQCastle
            BlackKCastle = Brd.BlackKCastle
            BlackQCastle = Brd.BlackQCastle
            EnPassantTarget = Brd.EnPassantTarget
            Promotion = false
            EnPassant = false
            From = Na
            To = Na
            CapturedPiece = EmptyColPc
            SecondaryFrom = Na
            SecondaryTo = Na
        }
    let BaseMove(from:int, mto:int) =
        let pF = Brd.Squares[from]
        let pT = Brd.Squares[mto]
        let cT = pT%2
        let cF = pF%2
        if pT <> EmptyColPc then
            Bits.PopBit(&Brd.Pieces[pT], mto)
            if cT = White then
                Bits.PopBit(&Brd.White, mto)
            else
                Bits.PopBit(&Brd.Black, mto)
            Bits.PopBit(&Brd.Both, mto)
            Zobrist.HashPiece(&Brd.ZobristHash, pT, mto)
        Bits.PopBit(&Brd.Pieces[pF], from)
        Bits.SetBit(&Brd.Pieces[pF], mto)
        Brd.Squares[mto] <- Brd.Squares[from]
        Brd.Squares[from] <- EmptyColPc
        if Brd.Squares[mto] = WhiteKing then Brd.WhiteKingLoc <- mto
        if Brd.Squares[mto] = BlackKing then Brd.BlackKingLoc <- mto
        if cF = White then
            Bits.PopBit(&Brd.White, from)
            Bits.SetBit(&Brd.White, mto)
        else 
            Bits.PopBit(&Brd.Black, from)
            Bits.SetBit(&Brd.Black, mto)
        Bits.PopBit(&Brd.Both, from)
        Bits.SetBit(&Brd.Both, mto)
        Zobrist.HashPiece(&Brd.ZobristHash, pF, from)
        Zobrist.HashPiece(&Brd.ZobristHash, pF, mto)
    let Empty(sq:int) =
        let cpc = Brd.Squares[sq]
        let color = cpc%2
        Bits.PopBit(&Brd.Pieces[cpc], sq)
        Brd.Squares[sq] <- EmptyColPc
        if color = White then
            Bits.PopBit(&Brd.White, sq)
        else 
            Bits.PopBit(&Brd.Black, sq)
        Bits.PopBit(&Brd.Both, sq)
        Zobrist.HashPiece(&Brd.ZobristHash, cpc, sq)
    let InsertPiece(cpc:int, sq:int) =
        let color = cpc%2
        Bits.SetBit(&Brd.Pieces[cpc], sq)
        if color = White then
            Bits.SetBit(&Brd.White, sq)
        else 
            Bits.SetBit(&Brd.Black, sq)
        Bits.SetBit(&Brd.Both, sq)
        Brd.Squares[sq] <- cpc
        if Brd.Squares[sq] = WhiteKing then Brd.WhiteKingLoc <- sq
        if Brd.Squares[sq] = BlackKing then Brd.BlackKingLoc <- sq
        Zobrist.HashPiece(&Brd.ZobristHash, cpc, sq)
    let Move(from:int, mto:int, promotion:int) =
        let cpcF = Brd.Squares[from]
        let cpcT = Brd.Squares[mto]
        let cF = cpcF%2
        let cT = cpcT%2
        let pF = cpcF/2
        let pT = cpcT/2
        let mutable rv = ToMove()
        if cpcT <> EmptyColPc then
            rv.CapturedPiece <- cpcT
        if Brd.EnPassantTarget = mto && pF = Pawn then
            let epPieceSq = if cF = White then Brd.EnPassantTarget + 8 else Brd.EnPassantTarget - 8
            let oppositeColor = cF ^^^ 1
            Empty(epPieceSq)
            rv.EnPassant <- true
            rv.CapturedPiece <- oppositeColor
        if Brd.EnPassantTarget<> Na then Zobrist.HashEp(&Brd.ZobristHash, Brd.EnPassantTarget)
        if pF = Pawn && abs(mto - from) = 16 then
            Brd.EnPassantTarget <- if cF = White then from - 8 else from + 8
            Zobrist.HashEp(&Brd.ZobristHash, Brd.EnPassantTarget)
        else Brd.EnPassantTarget <- Na
        BaseMove(from, mto)
        if promotion <> PromNone then
            Empty(mto)
            let prompc = promotion*2 + cF
            InsertPiece(prompc, mto)
            rv.Promotion <- true
        rv.From <- from
        rv.To <- mto
        Zobrist.HashCastlingRights(
            &Brd.ZobristHash, 
            Brd.WhiteKCastle, Brd.WhiteQCastle, 
            Brd.BlackKCastle, Brd.BlackQCastle
        )
        if pF = Rook then
            if cF = White then
                if from % 8 = 0 then Brd.WhiteQCastle <- 0x0
                if from % 8 = 7 then Brd.WhiteKCastle <- 0x0
            else
                if from % 8 = 0 then Brd.BlackQCastle <- 0x0
                if from % 8 = 7 then Brd.BlackKCastle <- 0x0
        if pF = King then
            if cF = White then
                Brd.WhiteQCastle <- 0x0
                Brd.WhiteKCastle <- 0x0
            else
                Brd.BlackQCastle <- 0x0
                Brd.BlackKCastle <- 0x0
            let d = abs(mto - from)
            if d = 2 then
                if mto > from then
                    rv.SecondaryFrom <- mto + 1
                    rv.SecondaryTo <- mto - 1
                else
                    rv.SecondaryFrom <- mto - 2
                    rv.SecondaryTo <- mto + 1
                BaseMove(rv.SecondaryFrom, rv.SecondaryTo)
        if pT = Rook then
            if cT = White then
                if mto = A1 then Brd.WhiteQCastle <- 0x0
                if mto = H1 then Brd.WhiteKCastle <- 0x0
            else
                if mto = A8 then Brd.BlackQCastle <- 0x0
                if mto = H8 then Brd.BlackKCastle <- 0x0
        Zobrist.HashCastlingRights(
            &Brd.ZobristHash, 
            Brd.WhiteKCastle, Brd.WhiteQCastle, 
            Brd.BlackKCastle, Brd.BlackQCastle
        )
        Brd.IsWtm <- not Brd.IsWtm  
        Brd.Stm <- Brd.Stm ^^^ 1  
        Brd.Xstm <- Brd.Xstm ^^^ 1  
        Zobrist.FlipTurnInHash(&Brd.ZobristHash)
        rv
    let UndoMove(rv:MoveRec)=
        Zobrist.HashCastlingRights(
            &Brd.ZobristHash, 
            Brd.WhiteKCastle, Brd.WhiteQCastle, 
            Brd.BlackKCastle, Brd.BlackQCastle
        )
        Brd.WhiteKCastle <- rv.WhiteKCastle
        Brd.WhiteQCastle <- rv.WhiteQCastle
        Brd.BlackKCastle <- rv.BlackKCastle
        Brd.BlackQCastle <- rv.BlackQCastle
        Zobrist.HashCastlingRights(
            &Brd.ZobristHash, 
            Brd.WhiteKCastle, Brd.WhiteQCastle, 
            Brd.BlackKCastle, Brd.BlackQCastle
        )
        if Brd.EnPassantTarget <> Na then
            Zobrist.HashEp(&Brd.ZobristHash, Brd.EnPassantTarget)
        Brd.EnPassantTarget <- rv.EnPassantTarget
        if Brd.EnPassantTarget <> Na then 
            Zobrist.HashEp(&Brd.ZobristHash, Brd.EnPassantTarget)
        Brd.IsWtm <- not Brd.IsWtm
        Brd.Stm <- Brd.Stm ^^^ 1  
        Brd.Xstm <- Brd.Xstm ^^^ 1  
        Zobrist.FlipTurnInHash(&Brd.ZobristHash)
        if rv.Promotion then
            let color = Brd.Squares.[rv.To]%2
            Empty(rv.To)
            InsertPiece(color, rv.To)
        let pF = Brd.Squares[rv.To]
        let pT = Brd.Squares[rv.From]
        BaseMove(rv.To, rv.From)
        if rv.EnPassant then
            let insertion = if rv.CapturedPiece = WhitePawn then rv.To - 8 else rv.To + 8
            InsertPiece(rv.CapturedPiece, insertion)
        elif rv.CapturedPiece <> EmptyColPc then
            InsertPiece(rv.CapturedPiece, rv.To)
        elif rv.SecondaryFrom <> Na then BaseMove(rv.SecondaryTo, rv.SecondaryFrom) 
    let GenerateFen() =
        let expandedBoardData:string array = Array.zeroCreate 8
        for v = 0 to 7 do
            let mutable rankData = ""
            let mutable h = 0
            while h < 8 do
                let sq = v * 8 + h
                let cpc = Brd.Squares[sq]
                if cpc = EmptyColPc then
                    let mutable c = 1
                    let mutable fnd = false
                    for i = h + 1 to 7 do
                        if not fnd then
                            let sq = v * 8 + i
                            let pc = Brd.Squares[sq]
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
        let turnData = if Brd.IsWtm then "w" else "b"
        let mutable castlingRight = ""
        if (Brd.WhiteKCastle = 0x0 && Brd.WhiteQCastle = 0x0 && Brd.BlackKCastle = 0x0 && Brd.BlackQCastle = 0x0) then
            castlingRight <- "-"
        else    
            if (Brd.WhiteKCastle <> 0x0) then castlingRight <- castlingRight + "K"
            if (Brd.WhiteQCastle <> 0x0) then castlingRight <- castlingRight + "Q"
            if (Brd.BlackKCastle <> 0x0) then castlingRight <- castlingRight + "k"
            if (Brd.BlackQCastle <> 0x0) then castlingRight <- castlingRight + "q"
        let mutable enPassantTarget = "-"
        if Brd.EnPassantTarget <> Na then
            enPassantTarget <- Brd.EnPassantTarget.ToString().ToLower()
        let fen = [| boardData; turnData; castlingRight; enPassantTarget |]
        fen|>Array.reduce (fun a b -> a + " " + b)
