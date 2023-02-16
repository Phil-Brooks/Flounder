namespace FlounderLib
open System

type LogarithmicReductionDepthTable() =
    let SIZE = 128
    let Internal = Array.zeroCreate (SIZE * SIZE)
    do
        for depth = 1 to SIZE-1 do
            for played = 1 to SIZE-1 do
            Internal.[depth * SIZE + played] <- int(Math.Log(depth) * Math.Log(played) / 2.0 - 0.2)
    member _.Item 
        with get(depth:int, played:int) = Internal.[depth * SIZE + played]
