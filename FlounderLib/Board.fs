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
    let ToMove(brd:byref<BoardRec>) =
        {
            WhiteKCastle = brd.WhiteKCastle
            WhiteQCastle = brd.WhiteQCastle
            BlackKCastle = brd.BlackKCastle
            BlackQCastle = brd.BlackQCastle
            EnPassantTarget = brd.EnPassantTarget
            Promotion = false
            EnPassant = false
            From = Na
            To = Na
            CapturedPiece = EmptyColPc
            SecondaryFrom = Na
            SecondaryTo = Na
        }
    let BaseMove(brd:byref<BoardRec>, from:int, mto:int) =
        let pF = brd.Squares[from]
        let pT = brd.Squares[mto]
        let cT = pT%2
        let cF = pF%2
        if pT <> EmptyColPc then
            Bits.PopBit(&brd.Pieces[pT], mto)
            if cT = White then
                Bits.PopBit(&brd.White, mto)
            else
                Bits.PopBit(&brd.Black, mto)
            Bits.PopBit(&brd.Both, mto)
            Zobrist.HashPiece(&brd.ZobristHash, pT, mto)
        Bits.PopBit(&brd.Pieces[pF], from)
        Bits.SetBit(&brd.Pieces[pF], mto)
        brd.Squares[mto] <- brd.Squares[from]
        brd.Squares[from] <- EmptyColPc
        if brd.Squares[mto] = WhiteKing then brd.WhiteKingLoc <- mto
        if brd.Squares[mto] = BlackKing then brd.BlackKingLoc <- mto
        if cF = White then
            Bits.PopBit(&brd.White, from)
            Bits.SetBit(&brd.White, mto)
        else 
            Bits.PopBit(&brd.Black, from)
            Bits.SetBit(&brd.Black, mto)
        Bits.PopBit(&brd.Both, from)
        Bits.SetBit(&brd.Both, mto)
        Zobrist.HashPiece(&brd.ZobristHash, pF, from)
        Zobrist.HashPiece(&brd.ZobristHash, pF, mto)
    let Empty(brd:byref<BoardRec>, sq:int) =
        let cpc = brd.Squares[sq]
        let color = cpc%2
        Bits.PopBit(&brd.Pieces[cpc], sq)
        brd.Squares[sq] <- EmptyColPc
        if color = White then
            Bits.PopBit(&brd.White, sq)
        else 
            Bits.PopBit(&brd.Black, sq)
        Bits.PopBit(&brd.Both, sq)
        Zobrist.HashPiece(&brd.ZobristHash, cpc, sq)
    let InsertPiece(brd:byref<BoardRec>, cpc:int, sq:int) =
        let color = cpc%2
        Bits.SetBit(&brd.Pieces[cpc], sq)
        if color = White then
            Bits.SetBit(&brd.White, sq)
        else 
            Bits.SetBit(&brd.Black, sq)
        Bits.SetBit(&brd.Both, sq)
        brd.Squares[sq] <- cpc
        if brd.Squares[sq] = WhiteKing then brd.WhiteKingLoc <- sq
        if brd.Squares[sq] = BlackKing then brd.BlackKingLoc <- sq
        Zobrist.HashPiece(&brd.ZobristHash, cpc, sq)
    let Move(brd:byref<BoardRec>, from:int, mto:int, promotion:int) =
        let cpcF = brd.Squares[from]
        let cpcT = brd.Squares[mto]
        let cF = cpcF%2
        let cT = cpcT%2
        let pF = cpcF/2
        let pT = cpcT/2
        let mutable rv = ToMove(&brd)
        if cpcT <> EmptyColPc then
            rv.CapturedPiece <- cpcT
        if brd.EnPassantTarget = mto && pF = Pawn then
            let epPieceSq = if cF = White then brd.EnPassantTarget + 8 else brd.EnPassantTarget - 8
            let oppositeColor = cF ^^^ 1
            Empty(&brd, epPieceSq)
            rv.EnPassant <- true
            rv.CapturedPiece <- oppositeColor
        if brd.EnPassantTarget<> Na then Zobrist.HashEp(&brd.ZobristHash, brd.EnPassantTarget)
        if pF = Pawn && abs(mto - from) = 16 then
            brd.EnPassantTarget <- if cF = White then from - 8 else from + 8
            Zobrist.HashEp(&brd.ZobristHash, brd.EnPassantTarget)
        else brd.EnPassantTarget <- Na
        BaseMove(&brd, from, mto)
        if promotion <> PromNone then
            Empty(&brd, mto)
            let prompc = promotion*2 + cF
            InsertPiece(&brd, prompc, mto)
            rv.Promotion <- true
        rv.From <- from
        rv.To <- mto
        Zobrist.HashCastlingRights(
            &brd.ZobristHash, 
            brd.WhiteKCastle, brd.WhiteQCastle, 
            brd.BlackKCastle, brd.BlackQCastle
        )
        if pF = Rook then
            if cF = White then
                if from % 8 = 0 then brd.WhiteQCastle <- 0x0
                if from % 8 = 7 then brd.WhiteKCastle <- 0x0
            else
                if from % 8 = 0 then brd.BlackQCastle <- 0x0
                if from % 8 = 7 then brd.BlackKCastle <- 0x0
        if pF = King then
            if cF = White then
                brd.WhiteQCastle <- 0x0
                brd.WhiteKCastle <- 0x0
            else
                brd.BlackQCastle <- 0x0
                brd.BlackKCastle <- 0x0
            let d = abs(mto - from)
            if d = 2 then
                if mto > from then
                    rv.SecondaryFrom <- mto + 1
                    rv.SecondaryTo <- mto - 1
                else
                    rv.SecondaryFrom <- mto - 2
                    rv.SecondaryTo <- mto + 1
                BaseMove(&brd, rv.SecondaryFrom, rv.SecondaryTo)
        if pT = Rook then
            if cT = White then
                if mto = A1 then brd.WhiteQCastle <- 0x0
                if mto = H1 then brd.WhiteKCastle <- 0x0
            else
                if mto = A8 then brd.BlackQCastle <- 0x0
                if mto = H8 then brd.BlackKCastle <- 0x0
        Zobrist.HashCastlingRights(
            &brd.ZobristHash, 
            brd.WhiteKCastle, brd.WhiteQCastle, 
            brd.BlackKCastle, brd.BlackQCastle
        )
        brd.IsWtm <- not brd.IsWtm  
        brd.Stm <- brd.Stm ^^^ 1  
        brd.Xstm <- brd.Xstm ^^^ 1  
        Zobrist.FlipTurnInHash(&brd.ZobristHash)
        rv
    let UndoMove(brd:byref<BoardRec>, rv:byref<MoveRec>)=
        Zobrist.HashCastlingRights(
            &brd.ZobristHash, 
            brd.WhiteKCastle, brd.WhiteQCastle, 
            brd.BlackKCastle, brd.BlackQCastle
        )
        brd.WhiteKCastle <- rv.WhiteKCastle
        brd.WhiteQCastle <- rv.WhiteQCastle
        brd.BlackKCastle <- rv.BlackKCastle
        brd.BlackQCastle <- rv.BlackQCastle
        Zobrist.HashCastlingRights(
            &brd.ZobristHash, 
            brd.WhiteKCastle, brd.WhiteQCastle, 
            brd.BlackKCastle, brd.BlackQCastle
        )
        if brd.EnPassantTarget <> Na then
            Zobrist.HashEp(&brd.ZobristHash, brd.EnPassantTarget)
        brd.EnPassantTarget <- rv.EnPassantTarget
        if brd.EnPassantTarget <> Na then 
            Zobrist.HashEp(&brd.ZobristHash, brd.EnPassantTarget)
        brd.IsWtm <- not brd.IsWtm
        brd.Stm <- brd.Stm ^^^ 1  
        brd.Xstm <- brd.Xstm ^^^ 1  
        Zobrist.FlipTurnInHash(&brd.ZobristHash)
        if rv.Promotion then
            let color = brd.Squares.[rv.To]%2
            Empty(&brd, rv.To)
            InsertPiece(&brd, color, rv.To)
        let pF = brd.Squares[rv.To]
        let pT = brd.Squares[rv.From]
        BaseMove(&brd, rv.To, rv.From)
        if rv.EnPassant then
            let insertion = if rv.CapturedPiece = WhitePawn then rv.To - 8 else rv.To + 8
            InsertPiece(&brd, rv.CapturedPiece, insertion)
        elif rv.CapturedPiece <> EmptyColPc then
            InsertPiece(&brd, rv.CapturedPiece, rv.To)
        elif rv.SecondaryFrom <> Na then BaseMove(&brd, rv.SecondaryTo, rv.SecondaryFrom) 
    let GenerateFen(brd:BoardRec) =
        let expandedBoardData:string array = Array.zeroCreate 8
        for v = 0 to 7 do
            let mutable rankData = ""
            let mutable h = 0
            while h < 8 do
                let sq = v * 8 + h
                let cpc = brd.Squares[sq]
                if cpc = EmptyColPc then
                    let mutable c = 1
                    let mutable fnd = false
                    for i = h + 1 to 7 do
                        if not fnd then
                            let sq = v * 8 + i
                            let pc = brd.Squares[sq]
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
        let turnData = if brd.IsWtm then "w" else "b"
        let mutable castlingRight = ""
        if (brd.WhiteKCastle = 0x0 && brd.WhiteQCastle = 0x0 && brd.BlackKCastle = 0x0 && brd.BlackQCastle = 0x0) then
            castlingRight <- "-"
        else    
            if (brd.WhiteKCastle <> 0x0) then castlingRight <- castlingRight + "K"
            if (brd.WhiteQCastle <> 0x0) then castlingRight <- castlingRight + "Q"
            if (brd.BlackKCastle <> 0x0) then castlingRight <- castlingRight + "k"
            if (brd.BlackQCastle <> 0x0) then castlingRight <- castlingRight + "q"
        let mutable enPassantTarget = "-"
        if brd.EnPassantTarget <> Na then
            enPassantTarget <- brd.EnPassantTarget.ToString().ToLower()
        let fen = [| boardData; turnData; castlingRight; enPassantTarget |]
        fen|>Array.reduce (fun a b -> a + " " + b)
