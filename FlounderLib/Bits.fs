namespace FlounderLib
open System.Numerics

type BitBoardIterator =
    struct
        val mutable Value:uint64
        val mutable Iteration:int
        new(value:uint64) =
            {
                Value = value
                Iteration = 0
            }
        member this.MoveNext() =
            this.Iteration <- this.Iteration + 1
            this.Value <> 0UL
        member this.Current =
            let i:int = BitOperations.TrailingZeroCount(this.Value)
            this.Value <- this.Value &&& (this.Value - 1UL)
            i
    end
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
    let ToArray (bb:uint64) =
        let count = BitOperations.PopCount(bb)
        let mutable iterator = BitBoardIterator(bb)
        let mutable ans:int array = Array.zeroCreate count
        for i = 0 to count - 1 do
            ans[i] <- iterator.Current
            iterator.MoveNext()|>ignore
        ans
    let SetBit(bb:byref<uint64>, sq) = bb <- (bb ||| (1UL <<< sq))
    let PopBit(bb:byref<uint64>, sq) = bb <- (bb &&& ~~~(1UL <<< sq))
    let Count(bb:uint64) = BitOperations.PopCount(bb)
    let IsSet(bb,sq) = ((bb >>> sq) &&& 1UL) <> 0UL
    let FromSq(sq:int) =
        let mutable bb = 0UL
        SetBit(&bb, sq)
        bb
