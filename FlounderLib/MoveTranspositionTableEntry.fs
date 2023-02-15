namespace FlounderLib

type MoveTranspositionTableEntry =
    struct
        val ZobristHash: uint64
        val Type: MoveTranspositionTableEntryType
        val BestMove: SearchedMove
        val Depth: byte
        new(zobristHash, typ, bestMove, depth:int) = 
            { 
                ZobristHash = zobristHash
                Type = typ
                BestMove = bestMove
                Depth = byte(depth)
            }
    end