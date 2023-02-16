namespace FlounderLib

type HistoryTable() =
    let SIZE = 2 * 6 * 64
    let mutable Internal:int[] = Array.zeroCreate SIZE
    member _.Item 
        with get(piece:Piece, color:PieceColor, targetSq:Square) = Internal.[int(color) * 384 + int(piece) * 64 + int(targetSq)]
        and set(piece:Piece, color:PieceColor, targetSq:Square) value = Internal.[int(color) * 384 + int(piece) * 64 + int(targetSq)] <- value
    

