namespace FlounderLib
open System.Numerics

module Bits =
    let ToInt(bb:uint64) = BitOperations.TrailingZeroCount(bb)
    let rec ToSeq (bb:uint64) =
        seq{
            if bb = 0UL then ()
            else
                let i = ToInt(bb)
                let nbb = bb &&& (bb - 1UL)
                yield i
                yield! ToSeq nbb
        }
    let ToArray (ibb:uint64) =
        let rec getarr bb cl =    
            if bb = 0UL then cl|>List.rev|>List.toArray
            else
                let i = ToInt(bb)
                let nbb = bb &&& (bb - 1UL)
                getarr nbb (i::cl)
        getarr ibb []
    let SetBit(bb:byref<uint64>, sq) = bb <- (bb ||| (1UL <<< sq))
    let PopBit(bb:byref<uint64>, sq) = bb <- (bb &&& ~~~(1UL <<< sq))
    let Count(bb:uint64) = BitOperations.PopCount(bb)
    let IsSet(bb,sq) = ((bb >>> sq) &&& 1UL) <> 0UL