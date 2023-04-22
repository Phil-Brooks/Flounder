namespace FlounderLib

module Perft =
    let D1 = 20uL
    let D2 = 400uL
    let D3 = 8902uL
    let D4 = 197281uL
    let D5 = 4865609uL
    let D6 = 119060324uL
    let D7 = 3195901860uL
    EngBoard.Default()
    let rec MoveGeneration(depth) =
        // Get all squares occupied by our color.
        let stm = Brd.Stm
        let xstm = Brd.Xstm
        let colored = if Brd.IsWtm then Brd.White else Brd.Black
        // Generate pins and check bitboards.
        let kingSq = if stm = White then Brd.WhiteKingLoc else Brd.BlackKingLoc
        let (hv, d) = MoveList.PinBitBoards(kingSq, stm, xstm)
        let (checks, doubleChecked) = MoveList.CheckBitBoard(kingSq, xstm)
        // Generate all pseudo-legal moves for our square iteration.
        if depth = 1 then
            // If depth is 1, then we don't need to do any further recursion and can just do +1 to the count.
            let mutable tot = 0UL
            let sqs = Bits.ToArray(colored)
            for sq in sqs do
                let moveList = MoveList.Double(sq, hv, d, checks, doubleChecked)
                tot <- tot + if moveList.Promotion then uint64(moveList.Count * 4) else uint64(moveList.Count)
            tot
        else
            // If depth is > 1, then we need to do recursion at depth = depth - 1.
            // Pre-figure our next depth to avoid calculations inside loop.
            let nextDepth = depth - 1
            let mutable tot = 0UL
            let sqs = Bits.ToArray(colored)
            for sq in sqs do
                // Generate all pseudo-legal moves for our square iteration.
                let moveList = MoveList.Double(sq, hv, d, checks, doubleChecked)
                let mvs = Bits.ToArray(moveList.Moves)
                for mv in mvs do
                   // Make our move iteration for our square iteration. Save the revert move for reverting
                    // in future.
                    let mutable rv = Board.Move(sq, mv, PromNone)
                    // If our king is safe, that move is legal and we can calculate moves at lesser
                    // depth recursively, but we shouldn't divide at lesser depth.
                    if rv.Promotion then
                        // Undo original pawn move without promotion.
                        Board.UndoMove(&rv)
                        for pr = PromKnight to PromQueen do 
                            rv <- Board.Move(sq, mv, pr)
                            tot <- tot + (MoveGeneration(nextDepth))
                            Board.UndoMove(&rv)
                        // Don't undo the final move as it's done outside.
                    else 
                        tot <- tot + MoveGeneration(nextDepth)
                        Board.UndoMove(&rv)
            tot
 
    let Depth1() = (D1, MoveGeneration(1))
    let Depth2() = (D2, MoveGeneration(2))
    let Depth3() = (D3, MoveGeneration(3))
    let Depth4() = (D4, MoveGeneration(4))
    let Depth5() = (D5, MoveGeneration(5))
    let Depth6() = (D6, MoveGeneration(6))
    let Depth7() = (D7, MoveGeneration(7))
