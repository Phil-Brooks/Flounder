namespace FlounderLib

type RevertMove =
    struct
        val WhiteKCastle:int
        val WhiteQCastle:int
        val BlackKCastle:int
        val BlackQCastle:int
        val EnPassantTarget:int
        val ColorToMove:int
        val mutable Promotion:bool
        val mutable EnPassant:bool
        val mutable From:int
        val mutable To:int
        val mutable CapturedPiece:int
        val mutable CapturedColor:int
        val mutable SecondaryFrom:int
        val mutable SecondaryTo:int
        new(map:BoardRec) =
            {
                WhiteKCastle = map.WhiteKCastle
                WhiteQCastle = map.WhiteQCastle
                BlackKCastle = map.BlackKCastle
                BlackQCastle = map.BlackQCastle
                EnPassantTarget = map.EnPassantTarget
                ColorToMove = if map.IsWtm then 0 else 1
                Promotion = false
                EnPassant = false
                From = Na
                To = Na
                CapturedPiece = EmptyPc
                CapturedColor = 2
                SecondaryFrom = Na
                SecondaryTo = Na
            }
        end
    
module RevertMove =
    let FromBitBoardMap(map:byref<BoardRec>) =
        RevertMove(map)