﻿namespace rec FlounderLib
open System.IO
open System

type BitBoardMap =
    struct
        val Bb:BitBoard array array
        val PiecesAndColors:byte array
        val mutable White:BitBoard
        val mutable Black:BitBoard
        val mutable ColorToMove:PieceColor
        val mutable WhiteKCastle:byte
        val mutable WhiteQCastle:byte
        val mutable BlackKCastle:byte
        val mutable BlackQCastle:byte
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
            let piecesAndColors:byte array = Array.zeroCreate 64
            for i = 0 to 63 do
                piecesAndColors[i] <- 0x26uy
            
            let expandedBoardData:string array = boardFen.Split(FEN_SPR)|>Array.rev
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
                                piecesAndColors.[v * 8 + h] <- 0x0uy
                            elif p = 'R' then
                                bb.[int(PieceColor.White)].[int(Piece.Rook)].[v * 8 + h] <- true
                                piecesAndColors.[v * 8 + h] <- 0x1uy
                            elif p = 'N' then
                                bb.[int(PieceColor.White)].[int(Piece.Knight)].[v * 8 + h] <- true
                                piecesAndColors.[v * 8 + h] <- 0x2uy
                            elif p = 'B' then
                                bb.[int(PieceColor.White)].[int(Piece.Bishop)].[v * 8 + h] <- true
                                piecesAndColors.[v * 8 + h] <- 0x3uy
                            elif p = 'Q' then
                                bb.[int(PieceColor.White)].[int(Piece.Queen)].[v * 8 + h] <- true
                                piecesAndColors.[v * 8 + h] <- 0x4uy
                            elif p = 'K' then
                                bb.[int(PieceColor.White)].[int(Piece.King)].[v * 8 + h] <- true
                                piecesAndColors.[v * 8 + h] <- 0x5uy
                        else
                            if p = 'p' then
                                bb.[int(PieceColor.Black)].[int(Piece.Pawn)].[v * 8 + h] <- true
                                piecesAndColors.[v * 8 + h] <- 0x10uy
                            elif p = 'r' then
                                bb.[int(PieceColor.Black)].[int(Piece.Rook)].[v * 8 + h] <- true
                                piecesAndColors.[v * 8 + h] <- 0x11uy
                            elif p = 'n' then
                                bb.[int(PieceColor.Black)].[int(Piece.Knight)].[v * 8 + h] <- true
                                piecesAndColors.[v * 8 + h] <- 0x12uy
                            elif p = 'b' then
                                bb.[int(PieceColor.Black)].[int(Piece.Bishop)].[v * 8 + h] <- true
                                piecesAndColors.[v * 8 + h] <- 0x13uy
                            elif p = 'q' then
                                bb.[int(PieceColor.Black)].[int(Piece.Queen)].[v * 8 + h] <- true
                                piecesAndColors.[v * 8 + h] <- 0x14uy
                            elif p = 'k' then
                                bb.[int(PieceColor.Black)].[int(Piece.King)].[v * 8 + h] <- true
                                piecesAndColors.[v * 8 + h] <- 0x15uy

                        h <- h + 1
           
            let white = bb.[int(PieceColor.White)].[int(Piece.Pawn)] ||| bb.[int(PieceColor.White)].[int(Piece.Rook)] ||| 
                        bb.[int(PieceColor.White)].[int(Piece.Knight)] ||| bb.[int(PieceColor.White)].[int(Piece.Bishop)] |||
                        bb.[int(PieceColor.White)].[int(Piece.Queen)] ||| bb.[int(PieceColor.White)].[int(Piece.King)]
            let black = bb.[int(PieceColor.Black)].[int(Piece.Pawn)] ||| bb.[int(PieceColor.Black)].[int(Piece.Rook)] ||| 
                        bb.[int(PieceColor.Black)].[int(Piece.Knight)] ||| bb.[int(PieceColor.Black)].[int(Piece.Bishop)] |||
                        bb.[int(PieceColor.Black)].[int(Piece.Queen)] ||| bb.[int(PieceColor.Black)].[int(Piece.King)]
            let colorToMove = if turnData.[0] = 'w' then PieceColor.White else PieceColor.Black
            let whiteKCastle = if castlingData.Contains('K') then 0x1uy else 0x0uy
            let whiteQCastle = if castlingData.Contains('Q') then 0x2uy else 0x0uy
            let blackKCastle = if castlingData.Contains('k') then 0x4uy else 0x0uy
            let blackQCastle = if castlingData.Contains('K') then 0x8uy else 0x0uy
            let mutable enPassantTarget = Square.Na
            if (enPassantTargetData.Length = 2) then
                enPassantTarget <- System.Enum.Parse<Square>(enPassantTargetData, true)
            let gethash() =
                let mutable zobristHash = 0UL
                for piece in [Piece.Pawn;Piece.Rook;Piece.Knight;Piece.Bishop;Piece.Queen;Piece.King] do
                    let psbb:BitBoard = (DJAA(int(colorToMove), int(piece)) bb)
                    let mutable pieceSquareIterator:BitBoardIterator = psbb.GetEnumerator()  
                    let mutable sq:Square = pieceSquareIterator.Current
                    while (pieceSquareIterator.MoveNext()) do
                        zobristHash <- zobristHash ^^^ Zobrist.PieceKeys.[piece, colorToMove, sq]
                        sq <- pieceSquareIterator.Current

                if (colorToMove = PieceColor.White) then zobristHash <- zobristHash ^^^ Zobrist.TurnKey
                if (enPassantTarget <> Square.Na) then zobristHash <- zobristHash ^^^ Zobrist.EnPassantKeys.AA(int(enPassantTarget))
                zobristHash <- zobristHash ^^^ Zobrist.CastlingKeys.AA(int(whiteKCastle) ||| int(whiteQCastle) ||| int(blackKCastle) ||| int(blackQCastle))
                zobristHash
            let zobristHash = gethash()
            BitBoardMap(bb,piecesAndColors,white,black,colorToMove,whiteKCastle,whiteQCastle,blackKCastle,blackQCastle,enPassantTarget,zobristHash)
        new(map:BitBoardMap, bb:BitBoard array array, piecesAndColors:byte array) =
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
                let r = this.PiecesAndColors.AA(int(sq))
                LanguagePrimitives.EnumOfValue(r &&& 0xFuy), LanguagePrimitives.EnumOfValue(r >>> 4)
        member this.Item 
            with get(color:PieceColor) = 
                if color = PieceColor.White then this.White
                elif color = PieceColor.Black then this.Black
                elif color = PieceColor.None then ~~~(this.White ||| this.Black)
                else raise (InvalidOperationException("Must provide a valid PieceColor."))
        member this.Item 
            with get(piece:Piece, color:PieceColor) = DJAA(int(color), int(piece)) this.Bb
        member this.PieceOnly(sq:Square):Piece = LanguagePrimitives.EnumOfValue(this.PiecesAndColors.AA(int(sq)) &&& 0xFuy)
        member this.ColorOnly(sq:Square):PieceColor = LanguagePrimitives.EnumOfValue(this.PiecesAndColors.AA(int(sq)) >>> 4)
        member this.Move(moveType:MoveUpdateType, pF:Piece, cF:PieceColor, pT:Piece, cT:PieceColor, from:Square, mto:Square) =
            if (pT <> Piece.Empty) then
                // If moving to piece isn't empty, then we capture.
                (DJAA(int(cT), int(pT)) this.Bb).[mto] <- false
                // Remove from color bitboards.
                if (cT = PieceColor.White) then
                    this.White.[mto] <- false
                else
                    this.Black.[mto] <- false
                // Update Zobrist.
                Zobrist.HashPiece(&this.ZobristHash, pT, cT, mto)
            // We remove from original square.
            (DJAA(int(cF), int(pF)) this.Bb).[from] <- false
            // Set at next square.
            (DJAA(int(cF), int(pF)) this.Bb).[mto] <- true
            // Make sure to update the pieces and colors.
            this.PiecesAndColors.AA(int(mto)) <- this.PiecesAndColors.AA(int(from))
            this.PiecesAndColors.AA(int(from)) <- 0x26uy
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
        member this.Move(moveType:MoveUpdateType, from:Square, mto:Square) =
            let pF,cF = this.[from]
            let pT, cT = this.[mto]
            this.Move(moveType, pF, cF, pT, cT, from, mto)
        member this.Empty(updateType:MoveUpdateType, piece:Piece, color:PieceColor, sq:Square) =
            // Remove from square.
            (DJAA(int(color), int(piece)) this.Bb).[sq] <- false
            // Set empty in pieces and colors.
            this.PiecesAndColors.AA(int(sq)) <- 0x26uy
            // Remove from color bitboards.
            if (color = PieceColor.White) then
                this.White.[sq] <- false
            else 
                this.Black.[sq] <- false
            // Update Zobrist.
            Zobrist.HashPiece(&this.ZobristHash, piece, color, sq)
        member this.Empty(updateType:MoveUpdateType, sq:Square) =
            let (piece, color) = this.[sq]
            this.Empty(updateType, piece, color, sq)
        member this.InsertPiece(updateType:MoveUpdateType, piece:Piece, color:PieceColor, sq:Square) =
            // Insert the piece at square.
            (DJAA(int(color), int(piece)) this.Bb).[sq] <- true
            // Insert into color bitboards.
            if (color = PieceColor.White) then
                this.White.[sq] <- true
            else 
                this.Black.[sq] <- true
            // Set piece in pieces and colors.
            let offset = if color = PieceColor.White then 0x0 else 0x10
            this.PiecesAndColors.AA(int(sq)) <- byte(int(piece) ||| offset)
            // Update Zobrist.
            Zobrist.HashPiece(&this.ZobristHash, piece, color, sq)
        member this.Copy() = BitBoardMap(this, this.Bb, this.PiecesAndColors)
        member this.GenerateBoardFen() =
            let expandedBoardData:string array = Array.zeroCreate 8
            for v = 0 to 7 do
                let mutable rankData = ""
                let mutable h = 0
                while h < 8 do
                    let sq:Square = LanguagePrimitives.EnumOfValue(sbyte(v * 8 + h))
                    let piece, color = this.[sq]
                    if (piece = Piece.Empty) then
                        let mutable c = 1
                        let mutable fnd = false
                        for i = h + 1 to 7 do
                            if not fnd then
                                let sq:Square = LanguagePrimitives.EnumOfValue(sbyte(v * 8 + i))
                                let pc,_ = this.[sq]
                                if (pc= Piece.Empty) then c <- c + 1
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
        if (map.EnPassantTarget <> Square.Na) then zobristHash <- zobristHash ^^^ Zobrist.EnPassantKeys.AA(int(map.EnPassantTarget))
        zobristHash <- zobristHash ^^^ Zobrist.CastlingKeys.AA(int(map.WhiteKCastle) ||| int(map.WhiteQCastle) ||| int(map.BlackKCastle) ||| int(map.BlackQCastle))
        zobristHash