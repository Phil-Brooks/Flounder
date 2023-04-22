namespace FlounderLib

module KillMv =
    let Arr = Array.create 256 OrdMove.Default
    let Get(typ:int, ply:int) = Arr[typ * 128 + ply]
    let Set(typ:int, ply:int, value) = Arr[typ * 128 + ply] <- value
    let ReOrder(ply:int) = Arr[128 + ply] <- Arr[ply]
    let Clear() = Arr.Initialize()


    

