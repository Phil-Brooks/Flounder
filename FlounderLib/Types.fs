namespace FlounderLib

[<AutoOpen>]
module Types =
    let VersionNo = "0.4.0.8"

    type ColPiece =
        // The type of piece.
        |WhitePawn = 0
        |BlackPawn = 1
        |WhiteKnight = 2
        |BlackKnight = 3
        |WhiteBishop = 4
        |BlackBishop = 5
        |WhiteRook = 6
        |BlackRook = 7
        |WhiteQueen = 8
        |BlackQueen = 9
        |WhiteKing = 10
        |BlackKing = 11
        |Empty = 12
    let ColPcs = 
        [|
            ColPiece.WhitePawn;ColPiece.BlackPawn;
            ColPiece.WhiteKnight;ColPiece.BlackKnight;
            ColPiece.WhiteBishop;ColPiece.BlackBishop;
            ColPiece.WhiteRook;ColPiece.BlackRook;
            ColPiece.WhiteQueen;ColPiece.BlackQueen;
            ColPiece.WhiteKing;ColPiece.BlackKing;
        |]
    
    type Piece =
        // The type of piece.
        |Pawn = 0
        |Knight = 1
        |Bishop = 2
        |Rook = 3
        |Queen = 4
        |King = 5
        |Empty = 6
    let Pcs = [|Piece.Pawn;Piece.Knight;Piece.Bishop;Piece.Rook;Piece.Queen;Piece.King|]

    type PieceColor =
        // The color of the piece.
        |White = 0
        |Black = 1
        |None = 2
    let Cols = [|PieceColor.White;PieceColor.Black|]

    type Square =
        // Squares on a chess board.
        // Square.Na is if it's no square on the board.

        |A8 = 0 |B8 = 1 |C8 = 2 |D8 = 3 |E8 = 4 |F8 = 5 |G8 = 6 |H8 = 7
        |A7 = 8 |B7 = 9 |C7 = 10 |D7 = 11 |E7 = 12 |F7 = 13 |G7 = 14 |H7 = 15
        |A6 = 16 |B6 = 17 |C6 = 18 |D6 = 19 |E6 = 20 |F6 = 21 |G6 = 22 |H6 = 23
        |A5 = 24 |B5 = 25 |C5 = 26 |D5 = 27 |E5 = 28 |F5 = 29 |G5 = 30 |H5 = 31
        |A4 = 32 |B4 = 33 |C4 = 34 |D4 = 35 |E4 = 36 |F4 = 37 |G4 = 38 |H4 = 39
        |A3 = 40 |B3 = 41 |C3 = 42 |D3 = 43 |E3 = 44 |F3 = 45 |G3 = 46 |H3 = 47
        |A2 = 48 |B2 = 49 |C2 = 50 |D2 = 51 |E2 = 52 |F2 = 53 |G2 = 54 |H2 = 55
        |A1 = 56 |B1 = 57 |C1 = 58 |D1 = 59 |E1 = 60 |F1 = 61 |G1 = 62 |H1 = 63
        |Na = 64

    type Promotion =
        |None = 0
        |Knight = 1
        |Bishop = 2
        |Rook = 3
        |Queen = 4
    let Proms = [|Promotion.Queen;Promotion.Rook;Promotion.Bishop;Promotion.Knight|]

    type MoveTranspositionTableEntryType =
        | Exact = 0
        | BetaCutoff = 1
        | AlphaUnchanged = 2
        | Invalid = 3

    let KING_BUCKETS = 
        [|
            15; 15; 14; 14; 14; 14; 15; 15; 
            15; 15; 14; 14; 14; 14; 15; 15; 
            13; 13; 12; 12; 12; 12; 13; 13; 
            13; 13; 12; 12; 12; 12; 13; 13; 
            11; 10; 9;  8;  8;  9;  10; 11; 
            11; 10; 9;  8;  8;  9;  10; 11; 
            7;  6;  5;  4;  4;  5;  6;  7;  
            3;  2;  1;  0;  0;  1;  2;  3 
        |]



module Piece =
    let FromInt(i:int) =
        let pc:Piece = LanguagePrimitives.EnumOfValue(i)
        pc

module PieceColor =
    let OppositeColor(color:PieceColor):PieceColor =
        if color = PieceColor.White then PieceColor.Black else PieceColor.White
    let FromInt(i:int) =
        let cl:PieceColor = LanguagePrimitives.EnumOfValue(i)
        cl

module ColPiece =
    let FromInt(i:int) =
        let colpc:ColPiece = LanguagePrimitives.EnumOfValue(i)
        colpc
    let ToPcCol(colpc:ColPiece) =
        let i = int(colpc)
        let pc = Piece.FromInt(i/2)
        let col = if pc=Piece.Empty then PieceColor.None else PieceColor.FromInt(i%2)
        pc,col
    let FromPcCol(piece:Piece,color:PieceColor) =
        FromInt(int(piece)*2 + int(color))

module Square =
    let FromInt(i:int) =
        let sq:Square = LanguagePrimitives.EnumOfValue(i)
        sq
    let ToFile(sq:Square) = int(sq)%8
    let OldInt(sq:Square) =
        let i = int(sq)
        let r = i/8
        let c = i%8
        8*(7-r) + c

module Promotion =
    let ToStr(promotion:Promotion) =
        let notation = promotion.ToString().[0].ToString().ToLower()
        if promotion = Promotion.Knight then "n" else notation
    let FromInt(i:int) =
        let prm:Promotion = LanguagePrimitives.EnumOfValue(i)
        prm
    let FromChar(ch:char) =
        if ch = 'r' then Promotion.Rook
        elif ch = 'n' then Promotion.Knight
        elif ch = 'b' then Promotion.Bishop
        elif ch = 'q' then Promotion.Queen
        else Promotion.None

