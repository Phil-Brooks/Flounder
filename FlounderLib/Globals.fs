namespace FlounderLib
open System.Threading

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
    let mutable Tc =
        {
            Source = new CancellationTokenSource()
            Token = new CancellationToken()
            StartTime = 0L
            Time = 0
        }

    let mutable Srch =
        let om = 
            {
                From = Na
                To = Na
                Promotion = PromNone
                Score = 0
            }
        {
            NodeCount = 0
            SelDepth = 0
            RedTimeMove = om
        }
