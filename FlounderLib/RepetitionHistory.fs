namespace FlounderLib

type RepetitionHistory =
    struct
        val mutable Internal:uint64 array
        val mutable Index:int
        new(size) =
            {
                Internal = Array.zeroCreate size
                Index = 0
            }
        member this.Append(zobristHash) = 
            this.Internal.[this.Index] <- zobristHash
            this.Index <- this.Index + 1
        member this.RemoveLast() = this.Index <- this.Index - 1
        member this.Count(zobristHash) =
            let mutable count = 0
            for i = this.Index - 1 downto 0 do
                if (this.Internal.[i] = zobristHash) then count <- count + 1
            count
        member this.Clone() =
            let mutable history = new RepetitionHistory(1024)
            for i = this.Index downto 0 do
                history.Internal.[i] <- this.Internal.[i]
            history.Index <- this.Index
            history
    end
module RepetitionHistory =
    let Default = RepetitionHistory(1024)