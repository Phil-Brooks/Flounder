namespace FlounderLib
open System

type LogarithmicReductionDepthTable =
    struct
        val mutable Internal:int array
        new(iarr:int array) =
            {
                Internal = iarr
            }
        new(size) =
            let mutable intnal = Array.zeroCreate size
            for depth = 1 to 127 do
                for played = 1 to 127 do
                    intnal.[depth * 128 + played] <- int(Math.Log(depth) * Math.Log(played) / 2.0 - 0.2)
            LogarithmicReductionDepthTable(intnal)
        member this.Item 
            with get(depth:int, played:int) = this.Internal.[depth * 128 + played]
    end
module LogarithmicReductionDepthTable =
    let Default() = LogarithmicReductionDepthTable(16384)