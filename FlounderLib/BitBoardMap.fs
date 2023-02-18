namespace rec FlounderLib
open System.IO
open System

type BitBoardMap =
    struct
        val Bb:BitBoard array array
        val PiecesAndColors:int array
        val mutable White:BitBoard
        val mutable Black:BitBoard
        val mutable ColorToMove:PieceColor
        val mutable WhiteKCastle:int
        val mutable WhiteQCastle:int
        val mutable BlackKCastle:int
        val mutable BlackQCastle:int
        val mutable EnPassantTarget:Square
        val mutable ZobristHash:uint64
        new(bb,piecesAndColors,white,black,colorToMove,whiteKCastle,whiteQCastle,blackKCastle,blackQCastle,enPassantTarget,zobristHash) =
            {
                Bb = bb
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
            let bb = 
                [|
                    [|BitBoard.Default;BitBoard.Default;BitBoard.Default;BitBoard.Default;BitBoard.Default;BitBoard.Default|]
                    [|BitBoard.Default;BitBoard.Default;BitBoard.Default;BitBoard.Default;BitBoard.Default;BitBoard.Default|]
                |]
            let piecesAndColors:int array = Array.zeroCreate 64
            for i = 0 to 63 do
                piecesAndColors[i] <- 0x26
            let expandedBoardData = boardFen.Split(FEN_SPR)|>Array.rev
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
                                bb.[int(PieceColor.White)].[int(Piece.Pawn)].[v * 8 + h] <- true
                                piecesAndColors.[v * 8 + h] <- 0x0
                            elif p = 'R' then
                                bb.[int(PieceColor.White)].[int(Piece.Rook)].[v * 8 + h] <- true
                                piecesAndColors.[v * 8 + h] <- 0x1
                            elif p = 'N' then
                                bb.[int(PieceColor.White)].[int(Piece.Knight)].[v * 8 + h] <- true
                                piecesAndColors.[v * 8 + h] <- 0x2
                            elif p = 'B' then
                                bb.[int(PieceColor.White)].[int(Piece.Bishop)].[v * 8 + h] <- true
                                piecesAndColors.[v * 8 + h] <- 0x3
                            elif p = 'Q' then
                                bb.[int(PieceColor.White)].[int(Piece.Queen)].[v * 8 + h] <- true
                                piecesAndColors.[v * 8 + h] <- 0x4
                            elif p = 'K' then
                                bb.[int(PieceColor.White)].[int(Piece.King)].[v * 8 + h] <- true
                                piecesAndColors.[v * 8 + h] <- 0x5
                        else
                            if p = 'p' then
                                bb.[int(PieceColor.Black)].[int(Piece.Pawn)].[v * 8 + h] <- true
                                piecesAndColors.[v * 8 + h] <- 0x10
                            elif p = 'r' then
                                bb.[int(PieceColor.Black)].[int(Piece.Rook)].[v * 8 + h] <- true
                                piecesAndColors.[v * 8 + h] <- 0x11
                            elif p = 'n' then
                                bb.[int(PieceColor.Black)].[int(Piece.Knight)].[v * 8 + h] <- true
                                piecesAndColors.[v * 8 + h] <- 0x12
                            elif p = 'b' then
                                bb.[int(PieceColor.Black)].[int(Piece.Bishop)].[v * 8 + h] <- true
                                piecesAndColors.[v * 8 + h] <- 0x13
                            elif p = 'q' then
                                bb.[int(PieceColor.Black)].[int(Piece.Queen)].[v * 8 + h] <- true
                                piecesAndColors.[v * 8 + h] <- 0x14
                            elif p = 'k' then
                                bb.[int(PieceColor.Black)].[int(Piece.King)].[v * 8 + h] <- true
                                piecesAndColors.[v * 8 + h] <- 0x15
                        h <- h + 1
            let white = bb.[int(PieceColor.White)].[int(Piece.Pawn)] ||| bb.[int(PieceColor.White)].[int(Piece.Rook)] ||| 
                        bb.[int(PieceColor.White)].[int(Piece.Knight)] ||| bb.[int(PieceColor.White)].[int(Piece.Bishop)] |||
                        bb.[int(PieceColor.White)].[int(Piece.Queen)] ||| bb.[int(PieceColor.White)].[int(Piece.King)]
            let black = bb.[int(PieceColor.Black)].[int(Piece.Pawn)] ||| bb.[int(PieceColor.Black)].[int(Piece.Rook)] ||| 
                        bb.[int(PieceColor.Black)].[int(Piece.Knight)] ||| bb.[int(PieceColor.Black)].[int(Piece.Bishop)] |||
                        bb.[int(PieceColor.Black)].[int(Piece.Queen)] ||| bb.[int(PieceColor.Black)].[int(Piece.King)]
            let colorToMove = if turnData.[0] = 'w' then PieceColor.White else PieceColor.Black
            let whiteKCastle = if castlingData.Contains('K') then 0x1 else 0x0
            let whiteQCastle = if castlingData.Contains('Q') then 0x2 else 0x0
            let blackKCastle = if castlingData.Contains('k') then 0x4 else 0x0
            let blackQCastle = if castlingData.Contains('K') then 0x8 else 0x0
            let mutable enPassantTarget = Square.Na
            if (enPassantTargetData.Length = 2) then
                enPassantTarget <- System.Enum.Parse<Square>(enPassantTargetData, true)
            let gethash() =
                let mutable zobristHash = 0UL
                for piece in [Piece.Pawn;Piece.Rook;Piece.Knight;Piece.Bishop;Piece.Queen;Piece.King] do
                    let psbb = bb.[int(colorToMove)].[int(piece)]
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
            BitBoardMap(bb,piecesAndColors,white,black,colorToMove,whiteKCastle,whiteQCastle,blackKCastle,blackQCastle,enPassantTarget,zobristHash)
        new(map:BitBoardMap, bb:BitBoard array array, piecesAndColors:int array) =
            let White = map.White
            let Black = map.Black
            let WhiteKCastle = map.WhiteKCastle
            let WhiteQCastle = map.WhiteQCastle
            let BlackKCastle = map.BlackKCastle
            let BlackQCastle = map.BlackQCastle
            let ColorToMove = map.ColorToMove
            let EnPassantTarget = map.EnPassantTarget
            let PiecesAndColors = Array.zeroCreate 64
            let Bb = Array.zeroCreate 2
            for i = 0 to 1 do
                Bb.[i] <- Array.zeroCreate 6
                Array.Copy(bb.[i], Bb.[i], 6)
            Array.Copy(piecesAndColors, PiecesAndColors, 64)
            let ZobristHash = map.ZobristHash
            BitBoardMap(Bb,PiecesAndColors,White,Black,ColorToMove,WhiteKCastle,WhiteQCastle,BlackKCastle,BlackQCastle,EnPassantTarget,ZobristHash)
        member this.Item 
            with get(sq:Square):(Piece*PieceColor) = 
                let r = this.PiecesAndColors.[int(sq)]
                Piece.FromInt(r &&& 0xF), PieceColor.FromInt(r >>> 4)
        member this.Item 
            with get(color:PieceColor) = 
                if color = PieceColor.White then this.White
                elif color = PieceColor.Black then this.Black
                elif color = PieceColor.None then ~~~(this.White ||| this.Black)
                else raise (InvalidOperationException("Must provide a valid PieceColor."))
        member this.Item 
            with get(piece:Piece, color:PieceColor) = this.Bb.[int(color)].[int(piece)]
        member this.PieceOnly(sq:Square):Piece = Piece.FromInt(this.PiecesAndColors.[int(sq)] &&& 0xF)
        member this.ColorOnly(sq:Square):PieceColor = PieceColor.FromInt(this.PiecesAndColors.[int(sq)] >>> 4)
        member this.Move(pF:Piece, cF:PieceColor, pT:Piece, cT:PieceColor, from:Square, mto:Square) =
            if (pT <> Piece.Empty) then
                // If moving to piece isn't empty, then we capture.
                this.Bb.[int(cT)].[int(pT)].[mto] <- false
                // Remove from color bitboards.
                if (cT = PieceColor.White) then
                    this.White.[mto] <- false
                else
                    this.Black.[mto] <- false
                // Update Zobrist.
                Zobrist.HashPiece(&this.ZobristHash, pT, cT, mto)
            // We remove from original square.
            this.Bb.[int(cF)].[int(pF)].[from] <- false
            // Set at next square.
            this.Bb.[int(cF)].[int(pF)].[mto] <- true
            // Make sure to update the pieces and colors.
            this.PiecesAndColors.[int(mto)] <- this.PiecesAndColors.[int(from)]
            this.PiecesAndColors.[int(from)] <- 0x26
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
            let pT, cT = this.[mto]
            this.Move(pF, cF, pT, cT, from, mto)
        member this.Empty(piece:Piece, color:PieceColor, sq:Square) =
            // Remove from square.
            this.Bb.[int(color)].[int(piece)].[sq] <- false
            // Set empty in pieces and colors.
            this.PiecesAndColors.[int(sq)] <- 0x26
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
            // Insert the piece at square.
            this.Bb.[int(color)].[int(piece)].[sq] <- true
            // Insert into color bitboards.
            if (color = PieceColor.White) then
                this.White.[sq] <- true
            else 
                this.Black.[sq] <- true
            // Set piece in pieces and colors.
            let offset = if color = PieceColor.White then 0x0 else 0x10
            this.PiecesAndColors.[int(sq)] <- (int(piece) ||| offset)
            // Update Zobrist.
            Zobrist.HashPiece(&this.ZobristHash, piece, color, sq)
        member this.Copy() = BitBoardMap(this, this.Bb, this.PiecesAndColors)
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
            String.Join(FEN_SPR, expandedBoardData|>Array.rev)
    end

module BitBoardMap =
    let Hash(map:BitBoardMap) =
        let mutable zobristHash = 0UL
        for piece in [Piece.Pawn;Piece.Rook;Piece.Knight;Piece.Bishop;Piece.Queen;Piece.King] do
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
