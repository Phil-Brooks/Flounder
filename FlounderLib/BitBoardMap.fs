namespace rec FlounderLib
open System.IO
open System

type BitBoardMap =
    struct
        val mutable stm:int // side to move
        val mutable xstm:int // not side to move
        val Pieces:BitBoard array
        val PiecesAndColors:int array
        val mutable White:BitBoard
        val mutable Black:BitBoard
        val mutable WhiteKCastle:int
        val mutable WhiteQCastle:int
        val mutable BlackKCastle:int
        val mutable BlackQCastle:int
        val mutable EnPassantTarget:Square
        val mutable ZobristHash:uint64
        new(stm,xstm,pieces,piecesAndColors,white,black,whiteKCastle,whiteQCastle,blackKCastle,blackQCastle,enPassantTarget,zobristHash) =
            {
                stm = stm
                xstm = xstm
                Pieces = pieces
                PiecesAndColors = piecesAndColors
                White = white
                Black = black
                WhiteKCastle = whiteKCastle
                WhiteQCastle = whiteQCastle 
                BlackKCastle = blackKCastle
                BlackQCastle = blackQCastle
                EnPassantTarget = enPassantTarget
                ZobristHash = zobristHash
            }
        new(boardFen:string, turnData:string, castlingData:string, enPassantTargetData:string) =
            let FEN_SPR = "/"
            let pieces =
                [|
                    BitBoard.Default;BitBoard.Default;BitBoard.Default;BitBoard.Default;BitBoard.Default;BitBoard.Default;
                    BitBoard.Default;BitBoard.Default;BitBoard.Default;BitBoard.Default;BitBoard.Default;BitBoard.Default
                |]
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
                                pieces.[WhitePawn].[v * 8 + h] <- true
                                piecesAndColors.[v * 8 + h] <- WhitePawn
                            elif p = 'N' then
                                pieces.[WhiteKnight].[v * 8 + h] <- true
                                piecesAndColors.[v * 8 + h] <- WhiteKnight
                            elif p = 'B' then
                                pieces.[WhiteBishop].[v * 8 + h] <- true
                                piecesAndColors.[v * 8 + h] <- WhiteBishop
                            elif p = 'R' then
                                pieces.[WhiteRook].[v * 8 + h] <- true
                                piecesAndColors.[v * 8 + h] <- WhiteRook
                            elif p = 'Q' then
                                pieces.[WhiteQueen].[v * 8 + h] <- true
                                piecesAndColors.[v * 8 + h] <- WhiteQueen
                            elif p = 'K' then
                                pieces.[WhiteKing].[v * 8 + h] <- true
                                piecesAndColors.[v * 8 + h] <- WhiteKing
                        else
                            if p = 'p' then
                                pieces.[BlackPawn].[v * 8 + h] <- true
                                piecesAndColors.[v * 8 + h] <- BlackPawn
                            elif p = 'n' then
                                pieces.[BlackKnight].[v * 8 + h] <- true
                                piecesAndColors.[v * 8 + h] <- BlackKnight
                            elif p = 'b' then
                                pieces.[BlackBishop].[v * 8 + h] <- true
                                piecesAndColors.[v * 8 + h] <- BlackBishop
                            elif p = 'r' then
                                pieces.[BlackRook].[v * 8 + h] <- true
                                piecesAndColors.[v * 8 + h] <- BlackRook
                            elif p = 'q' then
                                pieces.[BlackQueen].[v * 8 + h] <- true
                                piecesAndColors.[v * 8 + h] <- BlackQueen
                            elif p = 'k' then
                                pieces.[BlackKing].[v * 8 + h] <- true
                                piecesAndColors.[v * 8 + h] <- BlackKing
                        h <- h + 1
            let white = pieces.[WhitePawn] ||| pieces.[WhiteKnight] ||| 
                        pieces.[WhiteBishop] ||| pieces.[WhiteRook] ||| 
                        pieces.[WhiteQueen] ||| pieces.[WhiteKing] 
            let black = pieces.[BlackPawn] ||| pieces.[BlackKnight] ||| 
                        pieces.[BlackBishop] ||| pieces.[BlackRook] ||| 
                        pieces.[BlackQueen] ||| pieces.[BlackKing] 
            let stm = if turnData.[0] = 'w' then 0 else 1
            let xstm = stm^^^1
            let whiteKCastle = if castlingData.Contains('K') then 0x1 else 0x0
            let whiteQCastle = if castlingData.Contains('Q') then 0x2 else 0x0
            let blackKCastle = if castlingData.Contains('k') then 0x4 else 0x0
            let blackQCastle = if castlingData.Contains('q') then 0x8 else 0x0
            let mutable enPassantTarget = Square.Na
            if (enPassantTargetData.Length = 2) then
                enPassantTarget <- System.Enum.Parse<Square>(enPassantTargetData, true)
            let gethash() =
                let mutable zobristHash = 0UL
                for piece in Pcs do
                    let cpc = ColPiece.FromPcCol(piece,stm)
                    let psbb = pieces.[int(cpc)]
                    let mutable pieceSquareIterator = psbb.GetEnumerator()  
                    let mutable sq:Square = pieceSquareIterator.Current
                    while (pieceSquareIterator.MoveNext()) do
                        zobristHash <- zobristHash ^^^ Zobrist.PieceKeys.[piece, stm, sq]
                        sq <- pieceSquareIterator.Current
                if stm=0 then zobristHash <- zobristHash ^^^ Zobrist.TurnKey
                if (enPassantTarget <> Square.Na) then zobristHash <- zobristHash ^^^ Zobrist.EnPassantKeys.[int(enPassantTarget)]
                zobristHash <- zobristHash ^^^ Zobrist.CastlingKeys.[int(whiteKCastle) ||| int(whiteQCastle) ||| int(blackKCastle) ||| int(blackQCastle)]
                zobristHash
            let zobristHash = gethash()
            BitBoardMap(stm,xstm,pieces,piecesAndColors,white,black,whiteKCastle,whiteQCastle,blackKCastle,blackQCastle,enPassantTarget,zobristHash)
        new(map:BitBoardMap, pieces:BitBoard array, piecesAndColors:int array) =
            let stm = map.stm
            let xstm = map.xstm
            let White = map.White
            let Black = map.Black
            let WhiteKCastle = map.WhiteKCastle
            let WhiteQCastle = map.WhiteQCastle
            let BlackKCastle = map.BlackKCastle
            let BlackQCastle = map.BlackQCastle
            let EnPassantTarget = map.EnPassantTarget
            let Pieces = Array.copy pieces
            let PiecesAndColors = Array.copy piecesAndColors
            let ZobristHash = map.ZobristHash
            BitBoardMap(stm,xstm,Pieces,PiecesAndColors,White,Black,WhiteKCastle,WhiteQCastle,BlackKCastle,BlackQCastle,EnPassantTarget,ZobristHash)
        member this.Item 
            with get(sq:Square):int = 
                this.PiecesAndColors.[int(sq)]
        member this.Item 
            with get(color:int) = 
                if color = 0 then this.White
                elif color = 1 then this.Black
                elif color = 2 then ~~~(this.White ||| this.Black)
                else raise (InvalidOperationException("Must provide a valid PieceColor."))
        member this.Item 
            with get(piece:Piece, color:int) = 
                let icpc = 
                    if color = 2||piece=Piece.Empty then 12
                    else (int(piece)*2 + color)
                this.Pieces.[icpc]
        member this.PieceOnly(sq:Square):Piece = Piece.FromInt(int(this.PiecesAndColors.[int(sq)])/2)
        member this.ColorOnly(sq:Square):int = int(this.PiecesAndColors.[int(sq)])%2
        member this.Move(pF:int, pT:int, from:Square, mto:Square) =
            let cT = int(pT)%2
            let cF = int(pF)%2
            if (pT <> EmptyColPc) then
                // If moving to piece isn't empty, then we capture.
                this.Pieces.[int(pT)].[mto] <- false
                // Remove from color bitboards.
                if (cT = 0) then
                    this.White.[mto] <- false
                else
                    this.Black.[mto] <- false
                // Update Zobrist.
                Zobrist.HashPiece(&this.ZobristHash, Piece.FromInt(int(pT)/2), cT, mto)
            // We remove from original square.
            this.Pieces.[int(pF)].[from] <- false
            // Set at next square.
            this.Pieces.[int(pF)].[mto] <- true
            // Make sure to update the pieces and colors.
            this.PiecesAndColors.[int(mto)] <- this.PiecesAndColors.[int(from)]
            this.PiecesAndColors.[int(from)] <- EmptyColPc
            // Update color bitboards.
            if (cF = 0) then
                this.White.[from] <- false
                this.White.[mto] <- true
            else 
                this.Black.[from] <- false
                this.Black.[mto] <- true
            // Update Zobrist.
            Zobrist.HashPiece(&this.ZobristHash, Piece.FromInt(int(pF)/2), cF, from)
            Zobrist.HashPiece(&this.ZobristHash, Piece.FromInt(int(pF)/2), cF, mto)
        member this.Move(from:Square, mto:Square) =
            let pF = this.[from]
            let pT = this.[mto]
            this.Move(pF, pT, from, mto)
        member this.Empty(cpc:int, sq:Square) =
            let piece,color = ColPiece.ToPcCol(cpc)
            // Remove from square.
            this.Pieces.[int(cpc)].[sq] <- false
            // Set empty in pieces and colors.
            this.PiecesAndColors.[int(sq)] <- EmptyColPc
            // Remove from color bitboards.
            if color = 0 then
                this.White.[sq] <- false
            else 
                this.Black.[sq] <- false
            // Update Zobrist.
            Zobrist.HashPiece(&this.ZobristHash, piece, int(color), sq)
        member this.Empty(sq:Square) =
            let cpc = this.[sq]
            this.Empty(cpc, sq)
        member this.InsertPiece(cpc:int,sq:Square) =
            let piece,color = ColPiece.ToPcCol(cpc)
            // Insert the piece at square.
            this.Pieces.[int(cpc)].[sq] <- true
            // Insert into color bitboards.
            if color = 0 then
                this.White.[sq] <- true
            else 
                this.Black.[sq] <- true
            // Set piece in pieces and colors.
            this.PiecesAndColors.[int(sq)] <- cpc
            // Update Zobrist.
            Zobrist.HashPiece(&this.ZobristHash, piece, int(color), sq)
        member this.Copy() = BitBoardMap(this, this.Pieces, this.PiecesAndColors)
        member this.GenerateBoardFen() =
            let expandedBoardData:string array = Array.zeroCreate 8
            for v = 0 to 7 do
                let mutable rankData = ""
                let mutable h = 0
                while h < 8 do
                    let sq = Square.FromInt(v * 8 + h)
                    let piece, color = ColPiece.ToPcCol(this.[sq])
                    if (piece = Piece.Empty) then
                        let mutable c = 1
                        let mutable fnd = false
                        for i = h + 1 to 7 do
                            if not fnd then
                                let sq = Square.FromInt(v * 8 + i)
                                let pc = this.PieceOnly(sq)
                                if (pc = Piece.Empty) then c <- c + 1
                                else fnd <- true
                        rankData <- rankData + c.ToString()
                        h <- h + c
                    else
                        let mutable input = piece.ToString().[0].ToString()
                        if piece = Piece.Knight then input <- "N"
                        if color = 0 then rankData <- rankData + input
                        else rankData <- rankData + input.ToLower()
                        h <- h + 1
                expandedBoardData.[v] <- rankData
            let FEN_SPR = "/"
            String.Join(FEN_SPR, expandedBoardData)
    end

module BitBoardMap =
    let Hash(map:BitBoardMap) =
        let mutable zobristHash = 0UL
        for piece in Pcs do
            let psbb:BitBoard = map.[piece,map.stm]
            let mutable pieceSquareIterator:BitBoardIterator = psbb.GetEnumerator()  
            let mutable sq:Square = pieceSquareIterator.Current
            while (pieceSquareIterator.MoveNext()) do
                zobristHash <- zobristHash ^^^ Zobrist.PieceKeys.[piece, map.stm, sq]
                sq <- pieceSquareIterator.Current
        if (map.stm = 0) then zobristHash <- zobristHash ^^^ Zobrist.TurnKey
        if (map.EnPassantTarget <> Square.Na) then zobristHash <- zobristHash ^^^ Zobrist.EnPassantKeys.[int(map.EnPassantTarget)]
        zobristHash <- zobristHash ^^^ Zobrist.CastlingKeys.[map.WhiteKCastle ||| map.WhiteQCastle ||| map.BlackKCastle ||| map.BlackQCastle]
        zobristHash
