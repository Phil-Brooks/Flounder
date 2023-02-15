namespace FlounderLib
open System.Numerics

type BitBoardIterator =
    struct
        val mutable Count:int
        val mutable Value:uint64
        val mutable Iteration:int
        new(value:uint64,count:int) =
            {
                Count = count
                Value = value
                Iteration = 0
            }
        member this.MoveNext() =
            this.Iteration <- this.Iteration + 1
            this.Iteration <= this.Count
        member this.Current:Square =
            let i:int = BitOperations.TrailingZeroCount(this.Value)
            // Subtract 1 and only hold set bits in that mask.
            this.Value <- this.Value &&& (this.Value - 1UL)
            LanguagePrimitives.EnumOfValue(sbyte(i))
    end