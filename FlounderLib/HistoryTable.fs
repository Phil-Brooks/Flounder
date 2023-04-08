namespace FlounderLib

type HistoryTable =
    struct
        val mutable Internal:int array
        new(size) =
            {
                Internal = Array.zeroCreate size
            }
        member this.Item 
            with get(piece:int, color:int, targetSq:int) = this.Internal.[color * 384 + piece * 64 + targetSq]
            and set(piece:int, color:int, targetSq:int) value = this.Internal.[color * 384 + piece * 64 + targetSq] <- value
        member this.Clear() = this.Internal.Initialize()
    end
module HistoryTable =   
    let Default = HistoryTable(768)

