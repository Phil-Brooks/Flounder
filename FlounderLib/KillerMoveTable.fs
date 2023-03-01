namespace FlounderLib

type KillerMoveTable =
    struct
        val mutable Internal:OrderedMoveEntry array
        new(size) =
            {
                Internal = Array.zeroCreate size
            }
        member this.Item 
            with get(typ:int, ply:int) = this.Internal.[typ * 128 + ply]
            and set(typ:int, ply:int) value = this.Internal.[typ * 128 + ply] <- value
        member this.ReOrder(ply:int) = this.Internal.[128 + ply] <- this.Internal.[ply]
        member this.Clear() = this.Internal.Initialize()
    end
module KillerMoveTable =
    let Default = KillerMoveTable(256)




    

