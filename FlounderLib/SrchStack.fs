namespace FlounderLib

module SrchStack =
    let Arr = Array.create 128 0
    let Get(ply:int) = Arr[ply]
    let Set(ply:int, value) = Arr[ply] <- value
    let Clear() = Arr.Initialize()
