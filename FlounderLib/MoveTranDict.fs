namespace FlounderLib
open System.Collections.Concurrent

module MoveTran =
    let mutable Table:ConcurrentDictionary<uint64,MoveTranDictEntry> = new ConcurrentDictionary<uint64,MoveTranDictEntry>()
    let Init(threads) = 
        Table.Clear()
        Table <- new ConcurrentDictionary<uint64,MoveTranDictEntry>(threads-1,99999)
    let InsertEntry(zobristHash:uint64, entry:byref<MoveTranDictEntry>) =
        if Table.ContainsKey(zobristHash) then 
            let REPLACEMENT_DEPTH_THRESHOLD = 3
            let oldEntry = Table.[zobristHash]
            // Replace Scheme:
            // - ENTRY_TYPE == EXACT
            // - OLD_ENTRY_TYPE == ALPHA_UNCHANGED && ENTRY_TYPE == BETA_CUTOFF
            // - ENTRY_DEPTH > OLD_ENTRY_DEPTH - REPLACEMENT_THRESHOLD
            if (entry.Type = MoveTranspositionTableEntryType.Exact || 
                (oldEntry.Type = MoveTranspositionTableEntryType.AlphaUnchanged && 
                entry.Type = MoveTranspositionTableEntryType.BetaCutoff) ||
                int(entry.Depth) > int(oldEntry.Depth) - REPLACEMENT_DEPTH_THRESHOLD) then
                    Table.[zobristHash] <- entry
        else Table.[zobristHash] <- entry
    let GetEvalQ(zobristHash:uint64, alpha, beta) =
        let intable,storedEntry = Table.TryGetValue(zobristHash)
        if intable then
            if (storedEntry.Type = MoveTranspositionTableEntryType.Exact ||
                storedEntry.Type = MoveTranspositionTableEntryType.BetaCutoff &&
                storedEntry.BestMove.Evaluation >= beta ||
                storedEntry.Type = MoveTranspositionTableEntryType.AlphaUnchanged &&
                storedEntry.BestMove.Evaluation <= alpha) then
                // If our entry is valid for our position, and it's one of the following caseS:
                // - Exact
                // - Beta Cutoff with transposition evaluation >= beta
                // - Alpha Unchanged with transposition evaluation <= alpha
                // we can return early.
                storedEntry.BestMove.Evaluation|>Some
            else None
        else None
