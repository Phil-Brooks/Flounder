namespace FlounderLib

type MoveTranspositionTableEntry =
    struct
        val ZobristHash: uint64
        val Type: MoveTranspositionTableEntryType
        val BestMove: SearchedMove
        val Depth: int
        new(zobristHash, typ, bestMove, depth) = 
            { 
                ZobristHash = zobristHash
                Type = typ
                BestMove = bestMove
                Depth = depth
            }
    end