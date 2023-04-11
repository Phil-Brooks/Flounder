namespace rec FlounderLib
open System.IO
open System

type BitBoardMap =
    struct
        val mutable IsWtm:bool
        val Pieces:uint64 array
        val Squares:int array
        val mutable White:uint64
        val mutable Black:uint64
        val mutable Both:uint64
        val mutable WhiteKCastle:int
        val mutable WhiteQCastle:int
        val mutable BlackKCastle:int
        val mutable BlackQCastle:int
        val mutable EnPassantTarget:int
        val mutable ZobristHash:uint64
        new(isWtm,pieces,squares,white,black,both,whiteKCastle,whiteQCastle,blackKCastle,blackQCastle,enPassantTarget,zobristHash) =
            {
                IsWtm = isWtm
                Pieces = pieces
                Squares = squares
                White = white
                Black = black
                Both = both
                WhiteKCastle = whiteKCastle
                WhiteQCastle = whiteQCastle 
                BlackKCastle = blackKCastle
                BlackQCastle = blackQCastle
                EnPassantTarget = enPassantTarget
                ZobristHash = zobristHash
            }
        new(boardFen:string, turnData:string, castlingData:string, enPassantTargetData:string) =
            let FEN_SPR = "/"
            let pieces = Array.zeroCreate 12
            let piecesAndColors:int array = Array.zeroCreate 64
            for i = 0 to 63 do
                piecesAndColors[i] <- EmptyColPc
            let expandedBoardData = boardFen.Split(FEN_SPR)
            if expandedBoardData.Length <> 8 then 
                raise (InvalidDataException("Wrong board data provided: " + boardFen))
            for v = 0 to 7 do
                let rankData = expandedBoardData.[v]
                let mutable h = 0
                for p in rankData do
                    if (Char.IsNumber(p)) then
                        h <- h + int(p.ToString())
                    else
                        if (Char.IsUpper(p)) then
                            if p = 'P' then
                                Bits.SetBit(&pieces.[WhitePawn], v * 8 + h)
                                piecesAndColors.[v * 8 + h] <- WhitePawn
                            elif p = 'N' then
                                Bits.SetBit(&pieces.[WhiteKnight], v * 8 + h)
                                piecesAndColors.[v * 8 + h] <- WhiteKnight
                            elif p = 'B' then
                                Bits.SetBit(&pieces.[WhiteBishop], v * 8 + h)
                                piecesAndColors.[v * 8 + h] <- WhiteBishop
                            elif p = 'R' then
                                Bits.SetBit(&pieces.[WhiteRook], v * 8 + h)
                                piecesAndColors.[v * 8 + h] <- WhiteRook
                            elif p = 'Q' then
                                Bits.SetBit(&pieces.[WhiteQueen], v * 8 + h)
                                piecesAndColors.[v * 8 + h] <- WhiteQueen
                            elif p = 'K' then
                                Bits.SetBit(&pieces.[WhiteKing], v * 8 + h)
                                piecesAndColors.[v * 8 + h] <- WhiteKing
                        else
                            if p = 'p' then
                                Bits.SetBit(&pieces.[BlackPawn], v * 8 + h)
                                piecesAndColors.[v * 8 + h] <- BlackPawn
                            elif p = 'n' then
                                Bits.SetBit(&pieces.[BlackKnight], v * 8 + h)
                                piecesAndColors.[v * 8 + h] <- BlackKnight
                            elif p = 'b' then
                                Bits.SetBit(&pieces.[BlackBishop], v * 8 + h)
                                piecesAndColors.[v * 8 + h] <- BlackBishop
                            elif p = 'r' then
                                Bits.SetBit(&pieces.[BlackRook], v * 8 + h)
                                piecesAndColors.[v * 8 + h] <- BlackRook
                            elif p = 'q' then
                                Bits.SetBit(&pieces.[BlackQueen], v * 8 + h)
                                piecesAndColors.[v * 8 + h] <- BlackQueen
                            elif p = 'k' then
                                Bits.SetBit(&pieces.[BlackKing], v * 8 + h)
                                piecesAndColors.[v * 8 + h] <- BlackKing
                        h <- h + 1
            let white = pieces.[WhitePawn] ||| pieces.[WhiteKnight] ||| 
                        pieces.[WhiteBishop] ||| pieces.[WhiteRook] ||| 
                        pieces.[WhiteQueen] ||| pieces.[WhiteKing] 
            let black = pieces.[BlackPawn] ||| pieces.[BlackKnight] ||| 
                        pieces.[BlackBishop] ||| pieces.[BlackRook] ||| 
                        pieces.[BlackQueen] ||| pieces.[BlackKing] 
            let both = white ||| black
            let isWtm = turnData.[0] = 'w'
            let whiteKCastle = if castlingData.Contains('K') then 0x1 else 0x0
            let whiteQCastle = if castlingData.Contains('Q') then 0x2 else 0x0
            let blackKCastle = if castlingData.Contains('k') then 0x4 else 0x0
            let blackQCastle = if castlingData.Contains('q') then 0x8 else 0x0
            let mutable enPassantTarget = Na
            if enPassantTargetData.Length = 2 then
                enPassantTarget <- Square.FromStr(enPassantTargetData)
            let gethash() =
                let mutable zobristHash = 0UL
                for cpc = WhitePawn to BlackKing do
                    let psbb = pieces.[cpc]
                    let sqarr = Bits.ToArray(psbb)
                    Array.iter (fun sq -> zobristHash <- zobristHash ^^^ Zobrist.ZobristPieces.[cpc][sq]) sqarr
                if not isWtm then zobristHash <- zobristHash ^^^ Zobrist.ZobristSideKey
                if enPassantTarget <> Na then zobristHash <- zobristHash ^^^ Zobrist.ZobristEpKeys.[enPassantTarget]
                zobristHash <- zobristHash ^^^ Zobrist.ZobristCastleKeys.[whiteKCastle ||| whiteQCastle ||| blackKCastle ||| blackQCastle]
                zobristHash
            let zobristHash = gethash()
            BitBoardMap(isWtm,pieces,piecesAndColors,white,black,both,whiteKCastle,whiteQCastle,blackKCastle,blackQCastle,enPassantTarget,zobristHash)
        new(map:BitBoardMap, pieces:uint64 array, piecesAndColors:int array) =
            let isWtm = map.IsWtm
            let White = map.White
            let Black = map.Black
            let Both = map.Both
            let WhiteKCastle = map.WhiteKCastle
            let WhiteQCastle = map.WhiteQCastle
            let BlackKCastle = map.BlackKCastle
            let BlackQCastle = map.BlackQCastle
            let EnPassantTarget = map.EnPassantTarget
            let Pieces = Array.copy pieces
            let PiecesAndColors = Array.copy piecesAndColors
            let ZobristHash = map.ZobristHash
            BitBoardMap(isWtm,Pieces,PiecesAndColors,White,Black,Both,WhiteKCastle,WhiteQCastle,BlackKCastle,BlackQCastle,EnPassantTarget,ZobristHash)
        member this.Item 
            with get(piece:int, color:int) = 
                let icpc = 
                    if color = 2||piece=EmptyPc then 12
                    else (int(piece)*2 + color)
                this.Pieces.[icpc]
        member this.PieceOnly(sq:int):int = this.Squares.[sq]/2
        member this.ColorOnly(sq:int):int = this.Squares.[sq]%2
        member this.Move(pF:int, pT:int, from:int, mto:int) =
            let cT = pT%2
            let cF = pF%2
            if (pT <> EmptyColPc) then
                // If moving to piece isn't empty, then we capture.
                Bits.PopBit(&this.Pieces.[pT], mto)
                // Remove from color bitboards.
                if cT = 0 then
                    Bits.PopBit(&this.White, mto)
                else
                    Bits.PopBit(&this.Black, mto)
                Bits.PopBit(&this.Both, mto)
                // Update Zobrist.
                Zobrist.HashPiece(&this.ZobristHash, pT, mto)
            // We remove from original square.
            Bits.PopBit(&this.Pieces.[pF], from)
            // Set at next square.
            Bits.SetBit(&this.Pieces.[pF], mto)
            // Make sure to update the pieces and colors.
            this.Squares.[mto] <- this.Squares.[from]
            this.Squares.[from] <- EmptyColPc
            // Update color bitboards.
            if cF = 0 then
                Bits.PopBit(&this.White, from)
                Bits.SetBit(&this.White, mto)
            else 
                Bits.PopBit(&this.Black, from)
                Bits.SetBit(&this.Black, mto)
            Bits.PopBit(&this.Both, from)
            Bits.SetBit(&this.Both, mto)
            // Update Zobrist.
            Zobrist.HashPiece(&this.ZobristHash, pF, from)
            Zobrist.HashPiece(&this.ZobristHash, pF, mto)
        member this.Move(from:int, mto:int) =
            let pF = this.Squares[from]
            let pT = this.Squares[mto]
            this.Move(pF, pT, from, mto)
        member this.Empty(cpc:int, sq:int) =
            let _,color = ColPiece.ToPcCol(cpc)
            // Remove from square.
            Bits.PopBit(&this.Pieces.[cpc], sq)
            // Set empty in pieces and colors.
            this.Squares.[sq] <- EmptyColPc
            // Remove from color bitboards.
            if color = 0 then
                Bits.PopBit(&this.White, sq)
            else 
                Bits.PopBit(&this.Black, sq)
            Bits.PopBit(&this.Both, sq)
            // Update Zobrist.
            Zobrist.HashPiece(&this.ZobristHash, cpc, sq)
        member this.Empty(sq:int) =
            let cpc = this.Squares[sq]
            this.Empty(cpc, sq)
        member this.InsertPiece(cpc:int,sq:int) =
            let _,color = ColPiece.ToPcCol(cpc)
            // Insert the piece at square.
            Bits.SetBit(&this.Pieces.[cpc], sq)
            // Insert into color bitboards.
            if color = 0 then
                Bits.SetBit(&this.White, sq)
            else 
                Bits.SetBit(&this.Black, sq)
            Bits.SetBit(&this.Both, sq)
            // Set piece in pieces and colors.
            this.Squares.[sq] <- cpc
            // Update Zobrist.
            Zobrist.HashPiece(&this.ZobristHash, cpc, sq)
        member this.Copy() = BitBoardMap(this, this.Pieces, this.Squares)
        member this.GenerateBoardFen() =
            let expandedBoardData:string array = Array.zeroCreate 8
            for v = 0 to 7 do
                let mutable rankData = ""
                let mutable h = 0
                while h < 8 do
                    let sq = v * 8 + h
                    let cpc = this.Squares[sq]
                    if cpc = EmptyColPc then
                        let mutable c = 1
                        let mutable fnd = false
                        for i = h + 1 to 7 do
                            if not fnd then
                                let sq = v * 8 + i
                                let pc = this.Squares[sq]
                                if pc = EmptyColPc then c <- c + 1
                                else fnd <- true
                        rankData <- rankData + c.ToString()
                        h <- h + c
                    else
                        let input = ColPiece.ToStr(cpc)
                        rankData <- rankData + input
                        h <- h + 1
                expandedBoardData.[v] <- rankData
            let FEN_SPR = "/"
            String.Join(FEN_SPR, expandedBoardData)
    end

module BitBoardMap =
    let Hash(map:BitBoardMap) =
        let mutable zobristHash = 0UL
        for cpc = WhitePawn to BlackKing do
            let psbb = map.Pieces[cpc]
            let sqarr = Bits.ToArray(psbb)
            Array.iter (fun sq -> zobristHash <- zobristHash ^^^ Zobrist.ZobristPieces[cpc][sq]) sqarr
        if not map.IsWtm then zobristHash <- zobristHash ^^^ Zobrist.ZobristSideKey
        if map.EnPassantTarget <> Na then zobristHash <- zobristHash ^^^ Zobrist.ZobristEpKeys.[map.EnPassantTarget]
        zobristHash <- zobristHash ^^^ Zobrist.ZobristCastleKeys.[map.WhiteKCastle ||| map.WhiteQCastle ||| map.BlackKCastle ||| map.BlackQCastle]
        zobristHash
