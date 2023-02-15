namespace FlounderLib

type RepetitionHistory() =
    let SIZE = 1024
    let mutable InternalValue:uint64 array = Array.zeroCreate (SIZE)
    let mutable IndexValue = 0
    member _.Internal
        with get () = InternalValue
        and set (value) = InternalValue <- value
    member _.Index
        with get () = IndexValue
        and set (value) = IndexValue <- value
    member _.Append(zobristHash) = 
        InternalValue.AA(IndexValue) <- zobristHash
        IndexValue <- IndexValue+1
    member _.RemoveLast() = IndexValue <- IndexValue-1
    member _.Count(zobristHash) =
        let mutable count = 0
        for i = IndexValue - 1 downto 0 do
            if (InternalValue.AA(i) = zobristHash) then count <- count+1
        count
    member _.Clone() =
        let history = new RepetitionHistory()
        for i = IndexValue downto 0 do
            history.Internal.AA(i) <- InternalValue.AA(i)
        history.Index <- IndexValue
        history
