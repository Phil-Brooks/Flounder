namespace rec FlounderLib
open System.IO
open System

type BitBoardMap =
    struct
        val mutable stm:int// side to move
        val Pieces:BitBoard array
        val PiecesAndColors:ColPiece array
        val mutable White:BitBoard
        val mutable Black:BitBoard
        val mutable ColorToMove:PieceColor
        val mutable WhiteKCastle:int
        val mutable WhiteQCastle:int
        val mutable BlackKCastle:int
        val mutable BlackQCastle:int
        val mutable EnPassantTarget:Square
        val mutable ZobristHash:uint64
        new(stm,pieces,piecesAndColors,white,black,colorToMove,whiteKCastle,whiteQCastle,blackKCastle,blackQCastle,enPassantTarget,zobristHash) =
            {
                stm = stm
                Pieces = pieces
                PiecesAndColors = piecesAndColors
                White = white
                Black = black
                ColorToMove = colorToMove
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
            let piecesAndColors:ColPiece array = Array.zeroCreate 64
            for i = 0 to 63 do
                piecesAndColors[i] <- ColPiece.Empty
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
                                pieces.[int(ColPiece.WhitePawn)].[v * 8 + h] <- true
                                piecesAndColors.[v * 8 + h] <- ColPiece.WhitePawn
                            elif p = 'N' then
                                pieces.[int(ColPiece.WhiteKnight)].[v * 8 + h] <- true
                                piecesAndColors.[v * 8 + h] <- ColPiece.WhiteKnight
                            elif p = 'B' then
                                pieces.[int(ColPiece.WhiteBishop)].[v * 8 + h] <- true
                                piecesAndColors.[v * 8 + h] <- ColPiece.WhiteBishop
                            elif p = 'R' then
                                pieces.[int(ColPiece.WhiteRook)].[v * 8 + h] <- true
                                piecesAndColors.[v * 8 + h] <- ColPiece.WhiteRook
                            elif p = 'Q' then
                                pieces.[int(ColPiece.WhiteQueen)].[v * 8 + h] <- true
                                piecesAndColors.[v * 8 + h] <- ColPiece.WhiteQueen
                            elif p = 'K' then
                                pieces.[int(ColPiece.WhiteKing)].[v * 8 + h] <- true
                                piecesAndColors.[v * 8 + h] <- ColPiece.WhiteKing
                        else
                            if p = 'p' then
                                pieces.[int(ColPiece.BlackPawn)].[v * 8 + h] <- true
                                piecesAndColors.[v * 8 + h] <- ColPiece.BlackPawn
                            elif p = 'n' then
                                pieces.[int(ColPiece.BlackKnight)].[v * 8 + h] <- true
                                piecesAndColors.[v * 8 + h] <- ColPiece.BlackKnight
                            elif p = 'b' then
                                pieces.[int(ColPiece.BlackBishop)].[v * 8 + h] <- true
                                piecesAndColors.[v * 8 + h] <- ColPiece.BlackBishop
                            elif p = 'r' then
                                pieces.[int(ColPiece.BlackRook)].[v * 8 + h] <- true
                                piecesAndColors.[v * 8 + h] <- ColPiece.BlackRook
                            elif p = 'q' then
                                pieces.[int(ColPiece.BlackQueen)].[v * 8 + h] <- true
                                piecesAndColors.[v * 8 + h] <- ColPiece.BlackQueen
                            elif p = 'k' then
                                pieces.[int(ColPiece.BlackKing)].[v * 8 + h] <- true
                                piecesAndColors.[v * 8 + h] <- ColPiece.BlackKing
                        h <- h + 1
            let white = pieces.[int(ColPiece.WhitePawn)] ||| pieces.[int(ColPiece.WhiteKnight)] ||| 
                        pieces.[int(ColPiece.WhiteBishop)] ||| pieces.[int(ColPiece.WhiteRook)] ||| 
                        pieces.[int(ColPiece.WhiteQueen)] ||| pieces.[int(ColPiece.WhiteKing)] 
            let black = pieces.[int(ColPiece.BlackPawn)] ||| pieces.[int(ColPiece.BlackKnight)] ||| 
                        pieces.[int(ColPiece.BlackBishop)] ||| pieces.[int(ColPiece.BlackRook)] ||| 
                        pieces.[int(ColPiece.BlackQueen)] ||| pieces.[int(ColPiece.BlackKing)] 
            let colorToMove = if turnData.[0] = 'w' then PieceColor.White else PieceColor.Black
            let stm = if turnData.[0] = 'w' then 0 else 1
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
                    let cpc = ColPiece.FromPcCol(piece,colorToMove)
                    let psbb = pieces.[int(cpc)]
                    let mutable pieceSquareIterator = psbb.GetEnumerator()  
                    let mutable sq:Square = pieceSquareIterator.Current
                    while (pieceSquareIterator.MoveNext()) do
                        zobristHash <- zobristHash ^^^ Zobrist.PieceKeys.[piece, colorToMove, sq]
                        sq <- pieceSquareIterator.Current
                if (colorToMove = PieceColor.White) then zobristHash <- zobristHash ^^^ Zobrist.TurnKey
                if (enPassantTarget <> Square.Na) then zobristHash <- zobristHash ^^^ Zobrist.EnPassantKeys.[int(enPassantTarget)]
                zobristHash <- zobristHash ^^^ Zobrist.CastlingKeys.[int(whiteKCastle) ||| int(whiteQCastle) ||| int(blackKCastle) ||| int(blackQCastle)]
                zobristHash
            let zobristHash = gethash()
            BitBoardMap(stm,pieces,piecesAndColors,white,black,colorToMove,whiteKCastle,whiteQCastle,blackKCastle,blackQCastle,enPassantTarget,zobristHash)
        new(map:BitBoardMap, pieces:BitBoard array, piecesAndColors:ColPiece array) =
            let stm = map.stm
            let White = map.White
            let Black = map.Black
            let WhiteKCastle = map.WhiteKCastle
            let WhiteQCastle = map.WhiteQCastle
            let BlackKCastle = map.BlackKCastle
            let BlackQCastle = map.BlackQCastle
            let ColorToMove = map.ColorToMove
            let EnPassantTarget = map.EnPassantTarget
            let Pieces = Array.copy pieces
            let PiecesAndColors = Array.copy piecesAndColors
            let ZobristHash = map.ZobristHash
            BitBoardMap(stm,Pieces,PiecesAndColors,White,Black,ColorToMove,WhiteKCastle,WhiteQCastle,BlackKCastle,BlackQCastle,EnPassantTarget,ZobristHash)
        member this.Item 
            with get(sq:Square):(Piece*PieceColor) = 
                let colpc = this.PiecesAndColors.[int(sq)]
                ColPiece.ToPcCol(colpc)
        member this.Item 
            with get(color:PieceColor) = 
                if color = PieceColor.White then this.White
                elif color = PieceColor.Black then this.Black
                elif color = PieceColor.None then ~~~(this.White ||| this.Black)
                else raise (InvalidOperationException("Must provide a valid PieceColor."))
        member this.Item 
            with get(piece:Piece, color:PieceColor) = 
                let cpc = ColPiece.FromPcCol(piece,color)
                this.Pieces.[int(cpc)]
        member this.PieceOnly(sq:Square):Piece = Piece.FromInt(int(this.PiecesAndColors.[int(sq)])/2)
        member this.ColorOnly(sq:Square):PieceColor = PieceColor.FromInt(int(this.PiecesAndColors.[int(sq)])%2)
        member this.Move(pF:Piece, cF:PieceColor, pT:Piece, cT:PieceColor, from:Square, mto:Square) =
            let cpcT = ColPiece.FromPcCol(pT,cT)
            let cpcF = ColPiece.FromPcCol(pF,cF)
            if (pT <> Piece.Empty) then
                // If moving to piece isn't empty, then we capture.
                this.Pieces.[int(cpcT)].[mto] <- false
                // Remove from color bitboards.
                if (cT = PieceColor.White) then
                    this.White.[mto] <- false
                else
                    this.Black.[mto] <- false
                // Update Zobrist.
                Zobrist.HashPiece(&this.ZobristHash, pT, cT, mto)
            // We remove from original square.
            this.Pieces.[int(cpcF)].[from] <- false
            // Set at next square.
            this.Pieces.[int(cpcF)].[mto] <- true
            // Make sure to update the pieces and colors.
            this.PiecesAndColors.[int(mto)] <- this.PiecesAndColors.[int(from)]
            this.PiecesAndColors.[int(from)] <- ColPiece.Empty
            // Update color bitboards.
            if (cF = PieceColor.White) then
                this.White.[from] <- false
                this.White.[mto] <- true
            else 
                this.Black.[from] <- false
                this.Black.[mto] <- true
            // Update Zobrist.
            Zobrist.HashPiece(&this.ZobristHash, pF, cF, from)
            Zobrist.HashPiece(&this.ZobristHash, pF, cF, mto)
        member this.Move(from:Square, mto:Square) =
            let pF,cF = this.[from]
            let pT,cT = this.[mto]
            this.Move(pF, cF, pT, cT, from, mto)
        member this.Empty(piece:Piece, color:PieceColor, sq:Square) =
            let cpc = ColPiece.FromPcCol(piece,color)
            // Remove from square.
            this.Pieces.[int(cpc)].[sq] <- false
            // Set empty in pieces and colors.
            this.PiecesAndColors.[int(sq)] <- ColPiece.Empty
            // Remove from color bitboards.
            if (color = PieceColor.White) then
                this.White.[sq] <- false
            else 
                this.Black.[sq] <- false
            // Update Zobrist.
            Zobrist.HashPiece(&this.ZobristHash, piece, color, sq)
        member this.Empty(sq:Square) =
            let (piece, color) = this.[sq]
            this.Empty(piece, color, sq)
        member this.InsertPiece(piece:Piece, color:PieceColor, sq:Square) =
            let cpc = ColPiece.FromPcCol(piece,color)
            // Insert the piece at square.
            this.Pieces.[int(cpc)].[sq] <- true
            // Insert into color bitboards.
            if (color = PieceColor.White) then
                this.White.[sq] <- true
            else 
                this.Black.[sq] <- true
            // Set piece in pieces and colors.
            this.PiecesAndColors.[int(sq)] <- cpc
            // Update Zobrist.
            Zobrist.HashPiece(&this.ZobristHash, piece, color, sq)
        member this.Copy() = BitBoardMap(this, this.Pieces, this.PiecesAndColors)
        member this.GenerateBoardFen() =
            let expandedBoardData:string array = Array.zeroCreate 8
            for v = 0 to 7 do
                let mutable rankData = ""
                let mutable h = 0
                while h < 8 do
                    let sq = Square.FromInt(v * 8 + h)
                    let piece, color = this.[sq]
                    if (piece = Piece.Empty) then
                        let mutable c = 1
                        let mutable fnd = false
                        for i = h + 1 to 7 do
                            if not fnd then
                                let sq = Square.FromInt(v * 8 + i)
                                let pc,_ = this.[sq]
                                if (pc = Piece.Empty) then c <- c + 1
                                else fnd <- true
                        rankData <- rankData + c.ToString()
                        h <- h + c
                    else
                        let mutable input = piece.ToString().[0].ToString()
                        if (piece = Piece.Knight) then input <- "N"
                        if (color = PieceColor.White) then rankData <- rankData + input
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
            let psbb:BitBoard = map.[piece,map.ColorToMove]
            let mutable pieceSquareIterator:BitBoardIterator = psbb.GetEnumerator()  
            let mutable sq:Square = pieceSquareIterator.Current
            while (pieceSquareIterator.MoveNext()) do
                zobristHash <- zobristHash ^^^ Zobrist.PieceKeys.[piece, map.ColorToMove, sq]
                sq <- pieceSquareIterator.Current
        if (map.ColorToMove = PieceColor.White) then zobristHash <- zobristHash ^^^ Zobrist.TurnKey
        if (map.EnPassantTarget <> Square.Na) then zobristHash <- zobristHash ^^^ Zobrist.EnPassantKeys.[int(map.EnPassantTarget)]
        zobristHash <- zobristHash ^^^ Zobrist.CastlingKeys.[map.WhiteKCastle ||| map.WhiteQCastle ||| map.BlackKCastle ||| map.BlackQCastle]
        zobristHash
