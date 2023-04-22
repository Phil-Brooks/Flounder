namespace FlounderLib

module SrchEff =
    let Arr = Array.create 4096 0
    let Get(from:int, mto:int) = Arr[from * 64 + mto]
    let Set(from:int, mto:int, value) = Arr[from * 64 + mto] <- value
    let Clear() = Arr.Initialize()

