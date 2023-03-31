namespace FlounderLib

type HistoryTable =
    struct
        val mutable Internal:int array
        new(size) =
            {
                Internal = Array.zeroCreate size
            }
        member this.Item 
            with get(piece:Piece, color:int, targetSq:Square) = this.Internal.[int(color) * 384 + int(piece) * 64 + int(targetSq)]
            and set(piece:Piece, color:int, targetSq:Square) value = this.Internal.[int(color) * 384 + int(piece) * 64 + int(targetSq)] <- value
        member this.Clear() = this.Internal.Initialize()
    end
module HistoryTable =   
    let Default = HistoryTable(768)

