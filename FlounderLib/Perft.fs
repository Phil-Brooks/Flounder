namespace FlounderLib

module Perft =
    let D1 = 20uL
    let D2 = 400uL
    let D3 = 8902uL
    let D4 = 197281uL
    let D5 = 4865609uL
    let D6 = 119060324uL
    let D7 = 3195901860uL
    let mutable board = Board.Default()
    let rec MoveGeneration(board:byref<BoardRec>, depth) =
        // Get all squares occupied by our color.
        let stm = board.Stm
        let xstm = board.Xstm
        let colored = if board.IsWtm then board.White else board.Black
        // Generate pins and check bitboards.
        let kingSq = if stm = White then board.WhiteKingLoc else board.BlackKingLoc
        let (hv, d) = MoveList.PinBitBoards(board, kingSq, stm, xstm)
        let (checks, doubleChecked) = MoveList.CheckBitBoard(board, kingSq, xstm)
        // Generate all pseudo-legal moves for our square iteration.
        if depth = 1 then
            // If depth is 1, then we don't need to do any further recursion and can just do +1 to the count.
            let mutable tot = 0UL
            let sqs = Bits.ToArray(colored)
            for sq in sqs do
                let piece, pieceColor = ColPiece.ToPcCol(board.Squares[sq])
                let moveList = MoveList(&board, sq, piece, int(pieceColor), hv, d, checks, doubleChecked)
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
                let (piece, pieceColor) = ColPiece.ToPcCol(board.Squares[sq])
                let moveList = MoveList(&board, sq, piece, pieceColor, hv, d, checks, doubleChecked)
                let mvs = Bits.ToArray(moveList.Moves)
                for mv in mvs do
                   // Make our move iteration for our square iteration. Save the revert move for reverting
                    // in future.
                    let mutable rv = Board.Move(&board, sq, mv, PromNone)
                    // If our king is safe, that move is legal and we can calculate moves at lesser
                    // depth recursively, but we shouldn't divide at lesser depth.
                    if rv.Promotion then
                        // Undo original pawn move without promotion.
                        Board.UndoMove(&board, &rv)
                        for pr = PromKnight to PromQueen do 
                            rv <- Board.Move(&board, sq, mv, pr)
                            tot <- tot + (MoveGeneration(&board, nextDepth))
                            Board.UndoMove(&board, &rv)
                        // Don't undo the final move as it's done outside.
                    else 
                        tot <- tot + MoveGeneration(&board, nextDepth)
                        Board.UndoMove(&board, &rv)
            tot
 
    let Depth1() = (D1, MoveGeneration(&board,1))
    let Depth2() = (D2, MoveGeneration(&board,2))
    let Depth3() = (D3, MoveGeneration(&board,3))
    let Depth4() = (D4, MoveGeneration(&board,4))
    let Depth5() = (D5, MoveGeneration(&board,5))
    let Depth6() = (D6, MoveGeneration(&board,6))
    let Depth7() = (D7, MoveGeneration(&board,7))
