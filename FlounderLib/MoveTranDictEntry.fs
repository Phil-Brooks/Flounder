namespace FlounderLib

type MoveTranDictEntry =
    struct
        val Type: MoveTranspositionTableEntryType
        val BestMove: SearchedMove
        val Depth: int
        new(typ, bestMove, depth) = 
            { 
                Type = typ
                BestMove = bestMove
                Depth = depth
            }
    end
module MoveTranDictEntry =
    let Default = MoveTranDictEntry(MoveTranspositionTableEntryType.Invalid, SearchedMove.Default,0)