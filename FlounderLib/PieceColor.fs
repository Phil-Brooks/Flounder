namespace FlounderLib

type PieceColor =
    // The color of the piece.
    |White = 0uy
    |Black = 1uy
    |None = 2uy

module PieceColor =
    let OppositeColor(color:PieceColor):PieceColor =
        LanguagePrimitives.EnumOfValue(byte(color) ^^^ 0x1uy)