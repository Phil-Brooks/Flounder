namespace FlounderLib

type KillerMoveTable() =
    let SIZE = 128
    let mutable Internal:OrderedMoveEntry[] = Array.zeroCreate (2*SIZE)
    member _.Item 
        with get(typ:int, ply:int) = Internal.AA(typ * SIZE + ply)
        and set(typ:int, ply:int) value = Internal.AA(typ * SIZE + ply) <- value
    member _.ReOrder(ply:int) = Internal.AA(SIZE + ply) <- Internal.AA(ply)






    

