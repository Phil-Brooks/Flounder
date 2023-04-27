namespace FlounderLib
open System.Runtime.CompilerServices

module TranTable =
    let mutable megabyteSize = 16
    let GenerateTable() = 
        let byteSize = megabyteSize * 1024 * 1024
        let mutable hashFilter = 0
        let mutable i = 0x1
        while (byteSize >= (i + 1) * Unsafe.SizeOf<TranEntryRec>()) do 
            hashFilter <- i
            i <- (i <<< 1) ||| 0x1
        let mutable intnal = Array.zeroCreate (hashFilter + 1)
        for j = 0 to hashFilter do
            intnal[j] <- {Hash=0UL;Type=Invalid;BestMove=OrdMove.Default;Depth=0}
#if DEBUG
        System.Console.WriteLine("Allocated " + (hashFilter * Unsafe.SizeOf<TranEntryRec>()).ToString() + 
                            " bytes for " + hashFilter.ToString() + " TT entries.");
#endif
        {HashFilter=hashFilter;Internal=intnal}
    let mutable tt = GenerateTable()
    let Reset() = tt <- GenerateTable()
    let GetEntry(zobristHash:uint64) = 
        let index = int(zobristHash) &&& tt.HashFilter
        let ans = &(tt.Internal[index])
        ans
    let InsertEntry(zobristHash:uint64, entry:TranEntryRec) =
        let REPLACEMENT_DEPTH_THRESHOLD = 3
        let index = int(zobristHash) &&& tt.HashFilter
        let oldEntry = &(tt.Internal[index])
        if entry.Type = Exact || entry.Hash <> oldEntry.Hash then
            tt.Internal[index] <- entry
        elif oldEntry.Type = AlphaUnchanged && entry.Type = BetaCutoff then
            tt.Internal[index] <- entry
        elif entry.Depth > oldEntry.Depth - REPLACEMENT_DEPTH_THRESHOLD then
            tt.Internal[index] <- entry
