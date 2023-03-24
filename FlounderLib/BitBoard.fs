namespace rec FlounderLib
open System
open System.Numerics
open System.Runtime.CompilerServices

type BitBoard =
    struct
        val mutable Internal:uint64
        new(vl) =
            {
                Internal = vl
            }
        new(from:BitBoard) =
            {
                Internal = from.Internal
            }
        // Number of set bits.
        // Intrinsic: POPCNT
        member this.Count = BitOperations.PopCount(this.Internal)
        // Operators
        static member (+) (left:BitBoard, right:BitBoard) =
            let mutable ans = left
            ans.Internal <- left.Internal + right.Internal
            ans
        static member (-) (left:BitBoard, right:BitBoard) =
            let mutable ans = left
            ans.Internal <- left.Internal - right.Internal
            ans
        static member (*) (left:BitBoard, right:BitBoard) =
            let mutable ans = left
            ans.Internal <- left.Internal * right.Internal
            ans
        static member (/) (left:BitBoard, right:BitBoard) =
            let mutable ans = left
            ans.Internal <- left.Internal / right.Internal
            ans
        static member (%) (bto:BitBoard, by:uint64) =
            let mutable ans = bto
            ans.Internal <- ans.Internal % by
            ans
        static member (|||) (left:BitBoard, right:BitBoard) =
            let mutable ans = left
            ans.Internal <- left.Internal ||| right.Internal
            ans
        static member (&&&) (left:BitBoard, right:BitBoard) =
            let mutable ans = left
            ans.Internal <- left.Internal &&& right.Internal
            ans
        static member (~~~) (left:BitBoard) =
            let mutable ans = left
            ans.Internal <- ~~~left.Internal
            ans
        static member (>>>) (bitBoard:BitBoard, by:int) =
            let mutable ans = bitBoard
            ans.Internal <- ans.Internal >>> by
            ans
        static member (<<<) (bitBoard:BitBoard, by:int) =
            let mutable ans = bitBoard
            ans.Internal <- ans.Internal <<< by
            ans
        static member op_Equality (left:BitBoard, right:BitBoard) =
            left.Internal = right.Internal
        static member op_Inequality (left:BitBoard, right:BitBoard) =
            left.Internal <> right.Internal
        member this.ToBool() =
            this.Internal <> 0UL
        member this.ToUint64() =
            this.Internal
        member this.ToSq() =
            Square.FromInt(BitOperations.TrailingZeroCount(this.Internal))
        member this.ToSqs():Square array =
            let c = this.Count
            let mutable iterator = BitBoardIterator(this.Internal, c)
            let mutable a:Square array = Array.zeroCreate c
            for i = 0 to c-1 do
                a.[i] <- iterator.Current
                iterator.MoveNext()|>ignore
            a
        // Indexers
        member this.Item 
            with get(sq:Square):bool = 
                let mutable value = byte((this.Internal >>> int(sq)) &&& 1UL)
                Unsafe.As<byte, bool>(&value)
            and set (sq:Square) value = 
                if value then
                    this.Internal <- this.Internal ||| (1UL <<< int(sq))
                else
                    this.Internal <- this.Internal &&& (~~~(1UL <<< int(sq)))
        member this.Item 
            with get(i:int):bool = 
                let mutable value = byte((this.Internal >>> i) &&& 1UL)
                Unsafe.As<byte, bool>(&value)
            and set (i:int) value = 
                if value then
                    this.Internal <- this.Internal ||| (1UL <<< i)
                else
                    this.Internal <- this.Internal &&& (~~~(1UL <<< i))
        member this.GetEnumerator() = BitBoardIterator(this.Internal, this.Count)
        override this.ToString() =
            let mutable final = ""
            for v = 0 to 7 do
                let mutable bitString = ""
                for h = 0 to 7 do
                    bitString <- bitString + (if this.[v * 8 + h] then "1" else "*") + " "
                final <- final + bitString + "\n"
            final
    end

module BitBoard =
    let Default = BitBoard(UInt64.MinValue)
    let Filled = BitBoard(UInt64.MaxValue)
    let FromSq(sq:Square) =
        let mutable a = Default
        a.[sq] <- true
        a
        
        
