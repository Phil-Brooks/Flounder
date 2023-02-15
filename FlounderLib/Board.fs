namespace FlounderLib

type Board =
    val mutable Map:BitBoardMap 
    new(imap:BitBoardMap) =
        {Map = imap}
    new(boardData, turnData, castlingData, enPassantTargetData) = 
            let map = new BitBoardMap(boardData, turnData, castlingData, enPassantTargetData)
            Board(map)
    new(board:Board) = 
            let map = board.Map.Copy()
            Board(map)
    member this.All(piece, color) = this.Map.[piece, color]