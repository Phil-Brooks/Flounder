namespace FlounderLib

[<AutoOpen>]
module Globals =
    let mutable Brd =
        {
            IsWtm = true
            Stm = White
            Xstm = Black
            Pieces = Array.zeroCreate 12
            Squares = Array.zeroCreate 64
            WhiteKingLoc = 0
            BlackKingLoc = 0
            White = 0UL
            Black = 0UL
            Both = 0UL
            WhiteKCastle = 0
            WhiteQCastle = 0
            BlackKCastle = 0
            BlackQCastle = 0
            EnPassantTarget = 0
            ZobristHash = 0UL
        }
