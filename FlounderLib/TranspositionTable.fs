namespace FlounderLib
open System
open System.Runtime.CompilerServices

module TranspositionTable =
    let mutable megabyteSize = 16
    let GenerateTable() = 
        let byteSize = megabyteSize * 1024 * 1024
        let mutable hashFilter1 = 0
        let mutable i = 0x1
        while (byteSize >= (i + 1) * Unsafe.SizeOf<TranEntryRec>()) do 
            hashFilter1 <- i
            //looking for hash filter as a series of 1s
            i <- (i <<< 1) ||| 0x1
        let hashFilter = hashFilter1 >>> 2
        let mutable intnal = Array.zeroCreate (hashFilter + 1)
        for j = 0 to hashFilter do
            let ent = Array.create 4 {Hash=0UL;Type=Invalid;BestMove=OrderedMoveEntry.Default;Depth=0;Age=0} 
            intnal[j] <- ent
#if DEBUG
        Console.WriteLine("Allocated " + (hashFilter1 * Unsafe.SizeOf<TranEntryRec>()).ToString() + 
                            " bytes for " + hashFilter.ToString() + " (Binary:" + Convert.ToString(hashFilter, 2) + ") TT entries.");
#endif
        {HashFilter=uint64(hashFilter);Internal=intnal;Age=0}
    let mutable tt = GenerateTable()
    let Reset() = 
        for j = 0 to int(tt.HashFilter) do
            let ent = Array.create 4 {Hash=0UL;Type=Invalid;BestMove=OrderedMoveEntry.Default;Depth=0;Age=0} 
            tt.Internal[j] <- ent
        tt.Age <- 0
    let UpdateAge() = tt.Age <- tt.Age + 8
    
    let inline idx(hash:uint64) = int(hash &&& tt.HashFilter)
    let GetEntry(zobristHash:uint64) = 
        let index = idx(zobristHash)
        let tts = tt.Internal[index]
        let rec getans i curans =
            if i = 3 then curans
            else
                if curans.Hash = zobristHash then curans
                else getans (i+1) tts[i+1]
        let ans = getans 0 tts[0]
        ans
    let InsertEntry(zobristHash:uint64, entry:TranEntryRec) =
        let index = idx(zobristHash)
        let mutable inserted = false
        let rec putent i oldEntry =
            if oldEntry.Type = Invalid then
                tt.Internal.[index].[i] <- entry
                inserted <- true
            elif entry.Hash = oldEntry.Hash then
                if entry.Type = Exact then
                    tt.Internal.[index].[i] <- entry
                    inserted <- true
                elif entry.Depth + 3 > oldEntry.Depth then
                    tt.Internal.[index].[i] <- entry
                    inserted <- true
                else inserted <- true
            elif i < 3 then
                putent (i+1) tt.Internal.[index].[i+1]
        putent 0 tt.Internal.[index].[0]
        if not inserted then 
            //slot is full - best to replace oldest
            let mutable idx = 0
            let mutable age = tt.Internal.[index].[0].Age
            for i = 1 to 3 do
                if age > tt.Internal.[index].[i].Age then
                    age <- tt.Internal.[index].[i].Age
                    idx <- i
            if age < entry.Age then 
                tt.Internal.[index].[idx] <- entry
                //Console.WriteLine("TT slot updated at " + idx.ToString() + " with age " + age.ToString())
            else
                //Console.WriteLine("No older slots with age " + age.ToString())
                let mutable depth = tt.Internal.[index].[0].Depth
                for i = 1 to 3 do
                    if depth > tt.Internal.[index].[i].Depth then
                        depth <- tt.Internal.[index].[i].Depth
                        idx <- i
                tt.Internal.[index].[idx] <- entry
                //Console.WriteLine("TT slot updated at " + idx.ToString() + " with depth " + depth.ToString() + "new depth: " + entry.Depth.ToString())

