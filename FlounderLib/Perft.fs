namespace FlounderLib

module Perft =
    let D1 = 20uL
    let D2 = 400uL
    let D3 = 8902uL
    let D4 = 197281uL
    let D5 = 4865609uL
    let D6 = 119060324uL
    let D7 = 3195901860uL
    let Board = Board.Default()
    let rec MoveGeneration(board:Board, depth) =
        // Get all squares occupied by our color.
        let stm = if board.Map.IsWtm then 0 else 1 
        let xstm = if board.Map.IsWtm then 1 else 0 
        let colored = board.All(stm)
        // Generate pins and check bitboards.
        let kingSq = Bits.ToInt(board.KingLoc(stm))
        let (hv, d) = MoveList.PinBitBoards(board, kingSq, stm, xstm)
        let (checks, doubleChecked) = MoveList.CheckBitBoard(board, kingSq, xstm)
        // Generate all pseudo-legal moves for our square iteration.
        if (depth = 1) then
            // If depth is 1, then we don't need to do any further recursion and can just do +1 to the count.
            let sqs = Bits.ToArray(colored)
            let dosq sq =
                let piece, pieceColor = ColPiece.ToPcCol(board.At(sq))
                let moveList = MoveList(board, sq, piece, int(pieceColor), hv, d, checks, doubleChecked)
                if moveList.Promotion then uint64(moveList.Count * 4) else uint64(moveList.Count)
            sqs|>Array.map dosq|>Array.sum
        else
            // If depth is > 1, then we need to do recursion at depth = depth - 1.
            // Pre-figure our next depth to avoid calculations inside loop.
            let nextDepth = depth - 1
            let sqs = Bits.ToArray(colored)
            let dosq sq =
                // Generate all pseudo-legal moves for our square iteration.
                let (piece, pieceColor) = ColPiece.ToPcCol(board.At(sq))
                let moveList = MoveList(board, sq, piece, pieceColor, hv, d, checks, doubleChecked)
                let mvs = Bits.ToArray(moveList.Moves)
                let domv mv =
                   // Make our move iteration for our square iteration. Save the revert move for reverting
                    // in future.
                    let mutable rv = board.Move(sq, mv)
                    // If our king is safe, that move is legal and we can calculate moves at lesser
                    // depth recursively, but we shouldn't divide at lesser depth.
                    let mutable nextCount = 0uL
                    if rv.Promotion then
                        // Undo original pawn move without promotion.
                        board.UndoMove(&rv)
                        for pr = PromKnight to PromQueen do 
                            rv <- board.Move(sq, mv, pr)
                            nextCount <- nextCount + (MoveGeneration(board, nextDepth))
                            board.UndoMove(&rv)
                        // Don't undo the final move as it's done outside.
                    else 
                        nextCount <- MoveGeneration(board, nextDepth)
                        board.UndoMove(&rv)
                    nextCount
                mvs|>Array.map domv|>Array.sum
            sqs|>Array.map dosq|>Array.sum
 
    let Depth1() = (D1, MoveGeneration(Board,1))
    let Depth2() = (D2, MoveGeneration(Board,2))
    let Depth3() = (D3, MoveGeneration(Board,3))
    let Depth4() = (D4, MoveGeneration(Board,4))
    let Depth5() = (D5, MoveGeneration(Board,5))
    let Depth6() = (D6, MoveGeneration(Board,6))
    let Depth7() = (D7, MoveGeneration(Board,7))
