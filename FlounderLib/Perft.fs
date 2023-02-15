namespace FlounderLib

module Perft =
    let D1 = 20uL
    let D2 = 400uL
    let D3 = 8902uL
    let D4 = 197281uL
    let D5 = 4865609uL
    let D6 = 119060324uL
    let D7 = 3195901860uL
    let Board = Board1.Default()
    let rec MoveGeneration(iboard:Board1, depth) =
        // Figure out color and opposite color from the one set in the board.
        let oppositeColor = PieceColor.OppositeColor(iboard.ColorToMove)
        // Get all squares occupied by our color.
        let colored = iboard.All(iboard.ColorToMove)
        // Generate pins and check bitboards.
        let kingSq = iboard.KingLoc(iboard.ColorToMove).ToSq()
        let (hv, d) = MoveList.PinBitBoards(iboard, kingSq, iboard.ColorToMove, oppositeColor)
        let (checks, doubleChecked) = MoveList.CheckBitBoard(iboard, kingSq, oppositeColor)
        // Generate all pseudo-legal moves for our square iteration.
        if (depth = 1) then
            // If depth is 1, then we don't need to do any further recursion and can just do +1 to the count.
            let sqs = colored.ToSqs()
            let dosq sq =
                let piece, pieceColor = iboard.At(sq)
                let moveList = MoveList(iboard, sq, piece, pieceColor, hv, d, checks, doubleChecked)
                if moveList.Promotion then uint64(moveList.Count * 4) else uint64(moveList.Count)
            sqs|>Array.map dosq|>Array.sum
        else
            // If depth is > 1, then we need to do recursion at depth = depth - 1.
            // Pre-figure our next depth to avoid calculations inside loop.
            let nextDepth = depth - 1
            let sqs = colored.ToSqs()
            let dosq sq =
                let board = iboard.Clone()
                // Generate all pseudo-legal moves for our square iteration.
                let (piece, pieceColor) = board.At(sq)
                let moveList = MoveList(board, sq, piece, pieceColor, hv, d, checks, doubleChecked)
                let mvs = moveList.Moves.ToSqs()
                let domv mv =
                   // Make our move iteration for our square iteration. Save the revert move for reverting
                    // in future.
                    let mutable rv = board.Move(MoveUpdateType.Normal, sq, mv)
                    // If our king is safe, that move is legal and we can calculate moves at lesser
                    // depth recursively, but we shouldn't divide at lesser depth.
                    let mutable nextCount = 0uL
                    if (rv.Promotion) then
                        // Undo original pawn move without promotion.
                        board.UndoMove(MoveUpdateType.Normal, &rv)
                        let mutable i = 1
                        //TODO:must be better to iterate through promotion types
                        while (i < 5) do 
                            let pr:Promotion = LanguagePrimitives.EnumOfValue(byte(i))
                            let nbd = board.Clone()
                            let tnp = nbd.Move(MoveUpdateType.Normal, sq, mv, pr)
                            nextCount <- nextCount + ( MoveGeneration(nbd, nextDepth))
                            i<-i+1
                        // Don't undo the final move as it's done outside.
                    else 
                        //let nbd = board.Clone()
                        nextCount <- MoveGeneration(board, nextDepth)
                        board.UndoMove(MoveUpdateType.Normal, &rv)
                    // Revert the move to get back to original state.
                    //board.UndoMove<Normal>(&rv)
                    nextCount
                mvs|>Array.map domv|>Array.sum
            if depth>4 then
                sqs|>Array.Parallel.map dosq|>Array.sum
            else
                sqs|>Array.map dosq|>Array.sum
 
    let Depth1() = (D1, MoveGeneration(Board,1))
    let Depth2() = (D2, MoveGeneration(Board,2))
    let Depth3() = (D3, MoveGeneration(Board,3))
    let Depth4() = (D4, MoveGeneration(Board,4))
    let Depth5() = (D5, MoveGeneration(Board,5))
    let Depth6() = (D6, MoveGeneration(Board,6))
    let Depth7() = (D7, MoveGeneration(Board,7))
