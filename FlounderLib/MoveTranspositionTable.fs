namespace FlounderLib
open System.Runtime.CompilerServices

type MoveTranspositionTable =
    struct
        val mutable HashFilter:int
        val mutable Internal:MoveTranspositionTableEntry array
        new(iHashFilter,iInternal) =
            {
                HashFilter = iHashFilter
                Internal = iInternal
            }
        new(byteSize:int) =
            let mutable hashFilter = 0x0
            let mutable i = 0x1
            while (byteSize >= (i + 1) * Unsafe.SizeOf<MoveTranspositionTableEntry>()) do 
                hashFilter <- i
                i <- (i <<< 1) ||| 0x1
            let mutable intnal = Array.zeroCreate (hashFilter + 1)
            for j = 0 to hashFilter do
                intnal.[j] <- new MoveTranspositionTableEntry(0UL, Invalid, OrderedMoveEntry.Default, 0)
    #if DEBUG
            System.Console.WriteLine("Allocated " + (hashFilter * Unsafe.SizeOf<MoveTranspositionTableEntry>()).ToString() + 
                              " bytes for " + hashFilter.ToString() + " TT entries.");
    #endif
            MoveTranspositionTable(hashFilter,intnal)
        member this.Item 
            with get(zobristHash:uint64) = 
                let ans = &(this.Internal.[int(zobristHash) &&& this.HashFilter])
                ans
        member this.InsertEntry(zobristHash:uint64, entry:byref<MoveTranspositionTableEntry>) =
            let REPLACEMENT_DEPTH_THRESHOLD = 3
            let index = int(zobristHash) &&& this.HashFilter
            let oldEntry = &(this.Internal.[index])
            // Replace Scheme:
            // - ENTRY_TYPE == EXACT
            // - OLD_ENTRY_HASH != NEW_ENTRY_HASH
            // - OLD_ENTRY_TYPE == ALPHA_UNCHANGED && ENTRY_TYPE == BETA_CUTOFF
            // - ENTRY_DEPTH > OLD_ENTRY_DEPTH - REPLACEMENT_THRESHOLD
            if (entry.Type = Exact || entry.ZobristHash <> oldEntry.ZobristHash || 
                (oldEntry.Type = AlphaUnchanged && 
                entry.Type = BetaCutoff) ||
                int(entry.Depth) > int(oldEntry.Depth) - REPLACEMENT_DEPTH_THRESHOLD) then
                    this.Internal.[index] <- entry
        member this.FreeMemory() = 
            this.Internal <- null
        member this.Clear() = this.Internal.Initialize()
    end

module MoveTranspositionTable =
    let GenerateTable(megabyteSize) = 
        let MB_TO_B = 1_048_576
        MoveTranspositionTable(megabyteSize * MB_TO_B)