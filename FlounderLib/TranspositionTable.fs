namespace FlounderLib
open System
open System.Runtime.CompilerServices

module TranspositionTable =
    let mutable megabyteSize = 16
    let GenerateTable() = 
        let byteSize = megabyteSize * 1024 * 1024
        let mutable hashFilter = 0
        let mutable i = 0x1
        while (byteSize >= (i + 1) * Unsafe.SizeOf<TranEntryRec>()) do 
            hashFilter <- i
            //looking for hash filter as a series of 1s
            i <- (i <<< 1) ||| 0x1
        let mutable intnal = Array.zeroCreate (hashFilter + 1)
        for j = 0 to hashFilter do
            intnal[j] <- {Hash=0UL;Type=Invalid;BestMove=OrderedMoveEntry.Default;Depth=0} 
#if DEBUG
        Console.WriteLine("Allocated " + (hashFilter * Unsafe.SizeOf<TranEntryRec>()).ToString() + 
                            " bytes for " + hashFilter.ToString() + " (Binary:" + Convert.ToString(hashFilter, 2) + ") TT entries.");
#endif
        {HashFilter=uint64(hashFilter);Internal=intnal}
    let mutable tt = GenerateTable()
    let Reset() = 
        for j = 0 to int(tt.HashFilter) do
            tt.Internal[j] <- {Hash=0UL;Type=Invalid;BestMove=OrderedMoveEntry.Default;Depth=0}
    
    let inline idx(hash:uint64) = int(hash &&& tt.HashFilter)
    let GetEntry(zobristHash:uint64) = 
        let index = idx(zobristHash)
        tt.Internal[index]
    let InsertEntry(zobristHash:uint64, entry:TranEntryRec) =
        let index = idx(zobristHash)
        let oldEntry = &(tt.Internal.[index])
        if entry.Type = Exact || entry.Hash <> oldEntry.Hash then
            tt.Internal.[index] <- entry
        elif oldEntry.Type = AlphaUnchanged && entry.Type = BetaCutoff then
            tt.Internal.[index] <- entry
        elif entry.Depth > oldEntry.Depth - 3 then
            tt.Internal.[index] <- entry
