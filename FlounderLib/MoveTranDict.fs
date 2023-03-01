namespace FlounderLib
open System.Runtime.CompilerServices
open System.Collections.Concurrent

type MoveTranDict =
    val mutable TDict:ConcurrentDictionary<uint64,MoveTranDictEntry>
    new() =
        {
            TDict = new ConcurrentDictionary<uint64,MoveTranDictEntry>()
        }
    new(threads, byteSize:int) =
        {
            TDict = new ConcurrentDictionary<uint64,MoveTranDictEntry>(threads,byteSize/Unsafe.SizeOf<MoveTranDictEntry>())
        }
    member this.Item 
        with get(zobristHash:uint64) = 
            if this.TDict.ContainsKey(zobristHash) then
                let ans = this.TDict.[zobristHash]
                ans
            else MoveTranDictEntry.Default
    member this.InsertEntry(zobristHash:uint64, entry:byref<MoveTranDictEntry>) =
        if this.TDict.ContainsKey(zobristHash) then 
            let REPLACEMENT_DEPTH_THRESHOLD = 3
            let oldEntry = this.TDict.[zobristHash]
            // Replace Scheme:
            // - ENTRY_TYPE == EXACT
            // - OLD_ENTRY_TYPE == ALPHA_UNCHANGED && ENTRY_TYPE == BETA_CUTOFF
            // - ENTRY_DEPTH > OLD_ENTRY_DEPTH - REPLACEMENT_THRESHOLD
            if (entry.Type = MoveTranspositionTableEntryType.Exact || 
                (oldEntry.Type = MoveTranspositionTableEntryType.AlphaUnchanged && 
                entry.Type = MoveTranspositionTableEntryType.BetaCutoff) ||
                int(entry.Depth) > int(oldEntry.Depth) - REPLACEMENT_DEPTH_THRESHOLD) then
                    this.TDict.[zobristHash] <- entry
        else this.TDict.[zobristHash] <- entry
    member this.Clear() = 
        this.TDict.Clear()

module MoveTran =
    let mutable Table = MoveTranDict() 
    let Init(threads,megabyteSize) = 
        let MB_TO_B = 1_048_576
        Table.Clear()
        Table <- MoveTranDict(threads-1,megabyteSize * MB_TO_B)
