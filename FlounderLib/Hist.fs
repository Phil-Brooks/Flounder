namespace FlounderLib

module Hist =
    let Arr = Array.create 768 0
    let Get(piece:int, color:int, targetSq:int) = Arr[color * 384 + piece * 64 + targetSq]
    let Set(piece:int, color:int, targetSq:int, value) = Arr[color * 384 + piece * 64 + targetSq] <- value
    let Clear() = Arr.Initialize()

