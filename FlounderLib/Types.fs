namespace FlounderLib

[<AutoOpen>]
module Types =
    type MoveUpdateType =
        |Normal
        |ClassicalUpdate
        |NNUpdate

    type AccumulatorOperation =
        |Activate
        |Deactivate

    type Piece =
        // The type of piece.
        |Pawn = 0uy
        |Rook = 1uy
        |Knight = 2uy
        |Bishop = 3uy
        |Queen = 4uy
        |King = 5uy
        |Empty = 6uy

    type PieceColor =
        // The color of the piece.
        |White = 0uy
        |Black = 1uy
        |None = 2uy

    type Square =
        // Squares on a chess board.
        // Square.Na is if it's no square on the board.

        |A1 = 0y |B1 = 1y |C1 = 2y |D1 = 3y |E1 = 4y |F1 = 5y |G1 = 6y |H1 = 7y
        |A2 = 8y |B2 = 9y |C2 = 10y |D2 = 11y |E2 = 12y |F2 = 13y |G2 = 14y |H2 = 15y
        |A3 = 16y |B3 = 17y |C3 = 18y |D3 = 19y |E3 = 20y |F3 = 21y |G3 = 22y |H3 = 23y
        |A4 = 24y |B4 = 25y |C4 = 26y |D4 = 27y |E4 = 28y |F4 = 29y |G4 = 30y |H4 = 31y
        |A5 = 32y |B5 = 33y |C5 = 34y |D5 = 35y |E5 = 36y |F5 = 37y |G5 = 38y |H5 = 39y
        |A6 = 40y |B6 = 41y |C6 = 42y |D6 = 43y |E6 = 44y |F6 = 45y |G6 = 46y |H6 = 47y
        |A7 = 48y |B7 = 49y |C7 = 50y |D7 = 51y |E7 = 52y |F7 = 53y |G7 = 54y |H7 = 55y
        |A8 = 56y |B8 = 57y |C8 = 58y |D8 = 59y |E8 = 60y |F8 = 61y |G8 = 62y |H8 = 63y
        | Na = 64y

    type Promotion =
        |None = 0uy
        |Knight = 2uy
        |Bishop = 3uy
        |Rook = 1uy
        |Queen = 4uy

module Promotion =
    let ToUciNotation(promotion:Promotion) =
        let notation = promotion.ToString().[0].ToString().ToLower()
        if promotion = Promotion.Knight then "n" else notation



module PieceColor =
    let OppositeColor(color:PieceColor):PieceColor =
        LanguagePrimitives.EnumOfValue(byte(color) ^^^ 0x1uy)