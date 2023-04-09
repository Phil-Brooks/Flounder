namespace FlounderLib
open System.Numerics

module Bits =
    let rec ToSeq (bb:uint64) =
        seq{
            if bb = 0UL then ()
            else
                let i:int = BitOperations.TrailingZeroCount(bb)
                let nbb = bb &&& (bb - 1UL)
                yield i
                yield! ToSeq nbb
        }
    let ToArray (ibb:uint64) =
        let rec getarr bb cl =    
            if bb = 0UL then cl|>List.rev|>List.toArray
            else
                let i:int = BitOperations.TrailingZeroCount(bb)
                let nbb = bb &&& (bb - 1UL)
                getarr nbb (i::cl)
        getarr ibb []
