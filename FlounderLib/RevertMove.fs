namespace FlounderLib

type RevertMove =
    struct
        val WhiteKCastle:int
        val WhiteQCastle:int
        val BlackKCastle:int
        val BlackQCastle:int
        val EnPassantTarget:Square
        val ColorToMove:PieceColor
        val mutable Promotion:bool
        val mutable EnPassant:bool
        val mutable From:Square
        val mutable To:Square
        val mutable CapturedPiece:Piece
        val mutable CapturedColor:PieceColor
        val mutable SecondaryFrom:Square
        val mutable SecondaryTo:Square
        new(map:BitBoardMap) =
            {
                WhiteKCastle = map.WhiteKCastle
                WhiteQCastle = map.WhiteQCastle
                BlackKCastle = map.BlackKCastle
                BlackQCastle = map.BlackQCastle
                EnPassantTarget = map.EnPassantTarget
                ColorToMove = if map.stm = 0 then PieceColor.White else PieceColor.Black
                Promotion = false
                EnPassant = false
                From = Square.Na
                To = Square.Na
                CapturedPiece = Piece.Empty
                CapturedColor = PieceColor.None
                SecondaryFrom = Square.Na
                SecondaryTo = Square.Na
            }
        end
    
module RevertMove =
    let FromBitBoardMap(map:byref<BitBoardMap>) =
        RevertMove(map)