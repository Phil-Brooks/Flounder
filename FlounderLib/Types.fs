namespace FlounderLib

[<AutoOpen>]
module Types =
    let VersionNo = "0.3.2.9"
    
    type Piece =
        // The type of piece.
        |Pawn = 0
        |Rook = 1
        |Knight = 2
        |Bishop = 3
        |Queen = 4
        |King = 5
        |Empty = 6
    let Pcs = [|Piece.Pawn;Piece.Rook;Piece.Knight;Piece.Bishop;Piece.Queen;Piece.King|]

    type PieceColor =
        // The color of the piece.
        |White = 0
        |Black = 1
        |None = 2
    let Cols = [|PieceColor.White;PieceColor.Black|]

    type Square =
        // Squares on a chess board.
        // Square.Na is if it's no square on the board.

        |A1 = 0 |B1 = 1 |C1 = 2 |D1 = 3 |E1 = 4 |F1 = 5 |G1 = 6 |H1 = 7
        |A2 = 8 |B2 = 9 |C2 = 10 |D2 = 11 |E2 = 12 |F2 = 13 |G2 = 14 |H2 = 15
        |A3 = 16 |B3 = 17 |C3 = 18 |D3 = 19 |E3 = 20 |F3 = 21 |G3 = 22 |H3 = 23
        |A4 = 24 |B4 = 25 |C4 = 26 |D4 = 27 |E4 = 28 |F4 = 29 |G4 = 30 |H4 = 31
        |A5 = 32 |B5 = 33 |C5 = 34 |D5 = 35 |E5 = 36 |F5 = 37 |G5 = 38 |H5 = 39
        |A6 = 40 |B6 = 41 |C6 = 42 |D6 = 43 |E6 = 44 |F6 = 45 |G6 = 46 |H6 = 47
        |A7 = 48 |B7 = 49 |C7 = 50 |D7 = 51 |E7 = 52 |F7 = 53 |G7 = 54 |H7 = 55
        |A8 = 56 |B8 = 57 |C8 = 58 |D8 = 59 |E8 = 60 |F8 = 61 |G8 = 62 |H8 = 63
        |Na = 64

    type Promotion =
        |None = 0
        |Knight = 2
        |Bishop = 3
        |Rook = 1
        |Queen = 4
    let Proms = [|Promotion.Queen;Promotion.Rook;Promotion.Bishop;Promotion.Knight|]

    type MoveTranspositionTableEntryType =
        | Exact = 0
        | BetaCutoff = 1
        | AlphaUnchanged = 2
        | Invalid = 3

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

module Square =
    let FromInt(i:int) =
        let sq:Square = LanguagePrimitives.EnumOfValue(i)
        sq

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

