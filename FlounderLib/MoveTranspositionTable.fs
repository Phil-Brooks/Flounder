namespace FlounderLib
open System.Runtime.CompilerServices
open System.Threading.Tasks

[<AllowNullLiteral>]
type MoveTranspositionTable(byteSize:int) =
    static let MB_TO_B = 1_048_576
    let REPLACEMENT_DEPTH_THRESHOLD = 3
    let mutable HashFilter = 0
    let mutable Internal:MoveTranspositionTableEntry[] = null
    do
        HashFilter <- 0x0
        let mutable i = 0x1
        while (byteSize >= (i + 1) * Unsafe.SizeOf<MoveTranspositionTableEntry>()) do 
            HashFilter <- i
            i <- (i <<< 1) ||| 0x1
        Internal <- Array.zeroCreate (HashFilter + 1)
        Parallel.For(0, HashFilter + 1, fun i -> Internal.[i] <- new MoveTranspositionTableEntry())|>ignore
#if DEBUG
        System.Console.WriteLine("Allocated " + (HashFilter * Unsafe.SizeOf<MoveTranspositionTableEntry>()).ToString() + 
                          " bytes for " + HashFilter.ToString() + " TT entries.");
#endif
    static member GenerateTable(megabyteSize) = 
        MoveTranspositionTable(megabyteSize * MB_TO_B)
    member _.Item 
        with get(zobristHash:uint64) = 
            let ans = &(Internal.AA(int(zobristHash) &&& HashFilter))
            ans
    member _.InsertEntry(zobristHash:uint64, entry:byref<MoveTranspositionTableEntry>) =
        let index = int(zobristHash) &&& HashFilter
        let oldEntry = &(Internal.AA(index))
        // Replace Scheme:
        // - ENTRY_TYPE == EXACT
        // - OLD_ENTRY_HASH != NEW_ENTRY_HASH
        // - OLD_ENTRY_TYPE == ALPHA_UNCHANGED && ENTRY_TYPE == BETA_CUTOFF
        // - ENTRY_DEPTH > OLD_ENTRY_DEPTH - REPLACEMENT_THRESHOLD
        if (entry.Type = MoveTranspositionTableEntryType.Exact || entry.ZobristHash <> oldEntry.ZobristHash || 
            (oldEntry.Type = MoveTranspositionTableEntryType.AlphaUnchanged && 
            entry.Type = MoveTranspositionTableEntryType.BetaCutoff) ||
            int(entry.Depth) > int(oldEntry.Depth) - REPLACEMENT_DEPTH_THRESHOLD) then
                Internal.AA(index) <- entry

    member _.FreeMemory() = 
        Internal <- null
 