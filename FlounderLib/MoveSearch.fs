namespace FlounderLib
open System
open System.Diagnostics
open System.Text
    
type NodeType = NonPvNode|PvNode
type MoveSearch(board:EngineBoard, table:MoveTranspositionTable, timeControl:TimeControl) =

    let POS_INFINITY = 100000000
    let NEG_INFINITY = -POS_INFINITY
    let MATE = POS_INFINITY - 1
    let NULL_MOVE_REDUCTION = 4
    let NULL_MOVE_DEPTH = 2
    let NULL_MOVE_SCALING_FACTOR = 3
    let NULL_MOVE_SCALING_CORRECTION = 1
    let ASPIRATION_BOUND = 3500
    let ASPIRATION_SIZE = 16
    let ASPIRATION_DELTA = 23
    let ASPIRATION_DEPTH = 4
    let RAZORING_EVALUATION_THRESHOLD = 150
    let LMR_FULL_SEARCH_THRESHOLD = 4
    let LMR_DEPTH_THRESHOLD = 3
    let LMP_QUIET_THRESHOLD_BASE = 3
    let LMP_DEPTH_THRESHOLD = 3
    let NODE_COUNTING_DEPTH = 8
    let NODE_COUNTING_REQUIRED_EFFORT = 95
    let REVERSE_FUTILITY_D = 67
    let REVERSE_FUTILITY_I = 76
    let REVERSE_FUTILITY_DEPTH_THRESHOLD = 7
    let IIR_DEPTH_THRESHOLD = 3
    let IIR_DEPTH_REDUCTION = 1
    let FUTILITY_DEPTH_FACTOR = 150
    let CHECK_EXTENSION = 1
    let TIME_TO_DEPTH_THRESHOLD = 0.2

    let mutable tableCutoffCount = 0
    let mutable totalNodeSearchCount = 0
    let mutable selectiveDepth = 0
    let HistoryTable = HistoryTable()
    let KillerMoveTable = KillerMoveTable()
    let SearchEffort = MoveSearchEffortTable()
    let PvTable = PrincipleVariationTable()
    let MoveSearchStack = MoveSearchStack()
    let mutable ReducedTimeMove = OrderedMoveEntry.Default
    static let mutable reductionDepthTable = LogarithmicReductionDepthTable()

    let mutable Board:EngineBoard option =  None
    let mutable TimeControl:TimeControl = TimeControl(9999999)
    let mutable Table:MoveTranspositionTable option = None
    do
        Board <- board|>Some
        TimeControl <- timeControl
        Table <- Some(table)
    static member ReductionDepthTable
        with get() = reductionDepthTable
        and set(v) = reductionDepthTable <- v
    member _.TotalNodeSearchCount = totalNodeSearchCount
    member _.TableCutoffCount = tableCutoffCount
    member _.PvLine() = 
        let pv = StringBuilder()
        let count = PvTable.Count()
        
        for i = 0 to count-1 do
            let move:OrderedMoveEntry = PvTable.Get(i)
            
            pv.Append(move.From).Append(move.To)|>ignore
            if (move.Promotion <> Promotion.None) then pv.Append(Promotion.ToStr(move.Promotion))|>ignore
            pv.Append(' ')|>ignore
        pv.ToString().ToLower()
    member this.NodeCounting(depth:int, bestMove:OrderedMoveEntry, itimePreviouslyUpdated:bool) = 
        let mutable timePreviouslyUpdated = itimePreviouslyUpdated
        // This idea is from the Koivisto Engine:
        // The branch being searched the most is likely the best branch as we're having to evaluate it very deeply
        // across depths. Thus it's reasonable to end the search earlier and make the move instantly.

        // Check whether we're past the depth to start reducing our search time with node counting and make sure that
        // we're past the required effort threshold to do this move quickly.
        if (depth >= NODE_COUNTING_DEPTH && TimeControl.TimeLeft() <> 0 && not timePreviouslyUpdated
            && SearchEffort.[bestMove.From, bestMove.To] * 100 / this.TotalNodeSearchCount >= NODE_COUNTING_REQUIRED_EFFORT) then
            timePreviouslyUpdated <- true
            TimeControl.ChangeTime(TimeControl.Time / 3)
            ReducedTimeMove <- bestMove
        if (timePreviouslyUpdated && bestMove <> ReducedTimeMove) then
            // In the rare case that our previous node count guess was incorrect, give us a little bit more time
            // to see if we can find a better move.
            TimeControl.ChangeTime(TimeControl.Time * 3)
        timePreviouslyUpdated
    member this.DepthSearchLog(depth:int, evaluation:int, stopwatch:Stopwatch) =
        let elapSec = float(stopwatch.ElapsedMilliseconds) / 1000.0
        let ratio = int(float(this.TotalNodeSearchCount) / elapSec)
        Console.Write(
            "info depth " + depth.ToString() + " seldepth " + selectiveDepth.ToString() + " score cp " + evaluation.ToString() + " nodes " + 
            this.TotalNodeSearchCount.ToString() + " nps " + ratio.ToString() 
            + " pv " + this.PvLine() + "\n"
        )
    member this.QSearch(node:NodeType, board:EngineBoard, plyFromRoot:int, depth:int, ialpha:int, beta:int) =
        let mutable alpha = ialpha
        //// If we're out of time, we should exit the search as fast as possible.
        //// NOTE: Due to the nature of this exit (using exceptions to do it as fast as possible), the board state
        //// is not reverted. Thus, a cloned board must be provided.
        if (TimeControl.Finished()) then raise (OperationCanceledException())
        
        if (node = PvNode) then selectiveDepth <- Math.Max(selectiveDepth, plyFromRoot)

        let mutable ans = None 
        if (node = NonPvNode) then
            let storedEntry = Table.Value.[board.Board.ZobristHash]
            if (storedEntry.ZobristHash = board.Board.ZobristHash &&
                (storedEntry.Type = MoveTranspositionTableEntryType.Exact ||
                storedEntry.Type = MoveTranspositionTableEntryType.BetaCutoff &&
                storedEntry.BestMove.Evaluation >= beta ||
                storedEntry.Type = MoveTranspositionTableEntryType.AlphaUnchanged &&
                storedEntry.BestMove.Evaluation <= alpha)) then
                // If our entry is valid for our position, and it's one of the following caseS:
                // - Exact
                // - Beta Cutoff with transposition evaluation >= beta
                // - Alpha Unchanged with transposition evaluation <= alpha
                // we can return early.
                ans <- storedEntry.BestMove.Evaluation|>Some

        if ans.IsSome then 
            ans.Value
        else
            let mutable earlyEval = Evaluation.Relative(board.Board)
            // In the rare case our evaluation is already too good, we don't need to further evaluate captures any further,
            // as this position is overwhelmingly winning.
            if (earlyEval >= beta) then ans <- beta|>Some

            if ans.IsSome then ans.Value
            else
                // In the case that our current evaluation is better than our alpha, we need to recalibrate alpha to make sure
                // we don't skip over our already good move.
                if (earlyEval > alpha) then alpha <- earlyEval
 
                // Allocate memory on the stack to be used for our move-list.
                let moveSpanarr = Array.zeroCreate<OrderedMoveEntry>(OrderedMoveList.SIZE)//stackalloc OrderedMoveEntry[OrderedMoveList.SIZE];
                let mutable moveSpan = new Span<OrderedMoveEntry>(moveSpanarr)
                let moveList = OrderedMoveList(moveSpan, plyFromRoot, KillerMoveTable, HistoryTable)
                let moveCount = moveList.QSearchMoveGeneration(board.Board, SearchedMove.Default)
        
                let mutable bestEvaluation = earlyEval
        
                let inline handleEvaluation(evaluation:int) =
                    if (evaluation <= bestEvaluation) then true
            
                    // If our evaluation was better than our current best evaluation, we should update our evaluation
                    // with the new evaluation.
                    else
                        bestEvaluation <- evaluation

                        if (evaluation <= alpha) then true
                        else
                            // If our evaluation was better than our alpha (best unavoidable evaluation so far), then we should
                            // replace our alpha with our evaluation.
                            alpha <- evaluation
            
                            // If the evaluation was better than beta, it means the position was too good. Thus, there
                            // is a good chance that the opponent will avoid this path. Hence, there is currently no
                            // reason to evaluate it further.
                            evaluation < beta

                // Calculate next iteration variables before getting into the loop.
                let nextDepth = depth - 1
                let nextPlyFromRoot = plyFromRoot + 1
            
                let mutable i = 0
                let mutable keepgoing = true
                
                while (keepgoing && i < moveCount) do
                    // We should being the move that's likely to be the best move at this depth to the top. This ensures
                    // that we are searching through the likely best moves first, allowing us to return early.
                    moveList.SortNext(i, moveCount)
            
                    let mutable move = moveList.[i]

                    // Calculate approximation of SEE.
                    let see = SEE.Approximate(board, move)
            
                    // If SEE + the positional evaluation is greater than beta, then this capture is far too good, and hence
                    // causes a beta cutoff.
                    let seeEval = see + earlyEval;
                    if (seeEval > beta) then 
                        bestEvaluation <- seeEval
                        keepgoing <- false
                    else
                        // Make the move.
                        let mutable rv = board.Move(&move)
                        totalNodeSearchCount <- totalNodeSearchCount+1
        
                        // Evaluate position by searching deeper and negating the result. An evaluation that's good for
                        // our opponent will obviously be bad for us.
                        let evaluation = -this.QSearch(node, board, nextPlyFromRoot, nextDepth, -beta, -alpha)
                
                        // Undo the move.
                        board.UndoMove(&rv)

                        if not (handleEvaluation(evaluation)) then keepgoing <- false
            
                    i <- i+1
                bestEvaluation
    member this.AbSearch(node:NodeType, board:EngineBoard, plyFromRoot:int, idepth:int, ialpha:int, ibeta:int) =
        let mutable depth = idepth
        let mutable alpha = ialpha
        let mutable beta = ibeta
        let mutable ans = None

        // If we're out of time, we should exit the search as fast as possible.
        // NOTE: Due to the nature of this exit (using exceptions to do it as fast as possible), the board state
        // is not reverted. Thus, a cloned board must be provided.
        if (TimeControl.Finished()) then raise (OperationCanceledException())
        
        if (node = PvNode) then PvTable.InitializeLength(plyFromRoot)

        if (node = PvNode) then selectiveDepth <- Math.Max(selectiveDepth, plyFromRoot)
        
        // At depth 0 (or less in the case of reductions etc.), since we may be having a capture train, we should
        // jump into QSearch and evaluate even deeper. In the case of no captures available, QSearch will throw us
        // out instantly.
        let rootNode = plyFromRoot = 0
        let notRootNode = not rootNode

        if (depth <= 0) then ans <- this.QSearch(node, board, plyFromRoot, 15, alpha, beta)|>Some
        else
            if (notRootNode) then
                // We had a three-fold repetition, so return earlier.
                if (board.IsRepetition()) then ans <- 0|>Some
                else
                    let allPiecesCount = board.Board.All().Count
                    // If only the kings are left, it's a draw.
                    if (allPiecesCount = 2) then ans <- 0|>Some
                    else
                        let knightLeft = board.Board.All(Piece.Knight, PieceColor.White).ToBool() || board.Board.All(Piece.Knight, PieceColor.Black).ToBool()
                        // If only the kings and one knight is left, it's a draw.
                        if (allPiecesCount = 3 && knightLeft) then ans <- 0|>Some
                        else
                            let bishopLeft = board.Board.All(Piece.Bishop, PieceColor.White).ToBool() || board.Board.All(Piece.Bishop, PieceColor.Black).ToBool()
                            // If only the kings and one bishop is left, it's a draw.
                            if (allPiecesCount = 3 && bishopLeft) then ans <- 0|>Some
                            else
                                // If we are not at the root, we should check and see if there is a ready mate.
                                // If there is, we shouldn't really care about other moves or slower mates, but instead
                                // we should prune as fast as possible. It's crucial to ensuring we hit high depths.
                                alpha <- Math.Max(alpha, -MATE + plyFromRoot)
                                beta <- Math.Min(beta, MATE - plyFromRoot - 1);
                                if (alpha >= beta) then ans <- alpha|>Some
        if ans.IsSome then 
            ans.Value
        else
            let storedEntry = Table.Value.[board.Board.ZobristHash]
            let valid = storedEntry.Type <> MoveTranspositionTableEntryType.Invalid
            let mutable transpositionMove = SearchedMove.Default
            let mutable transpositionHit = false

            if (valid && storedEntry.ZobristHash = board.Board.ZobristHash) then
                // We had a transposition table hit. However, at this point, we don't know if this is a trustworthy
                // transposition hit or not.
                transpositionMove <- storedEntry.BestMove
                transpositionHit <- true

                if (node = NonPvNode && int(storedEntry.Depth) >= depth) then
                    // If it came from a higher depth search than our current depth, it means the results are definitely
                    // more trustworthy than the ones we could achieve at this depth.
                    if storedEntry.Type=MoveTranspositionTableEntryType.Exact then
                            // In the case of an exact evaluation, we have previously found this was our best move
                            // in said transposition. Therefore, it is reasonable to return early.
                            ans <- storedEntry.BestMove.Evaluation|>Some
                    
                        // In the case that we didn't have an exact, we must alter our bounds to make our search for this
                        // depth as best as possible (and possibly get a cutoff without having to search).
                    elif storedEntry.Type=MoveTranspositionTableEntryType.BetaCutoff then
                            // In the case we had a beta-cutoff, we can check the max between our alpha and the stored 
                            // beta-cutoff and set it as our new alpha. This is to ensure all moves will be better than the
                            // stored cutoff.
                            alpha <- Math.Max(alpha, storedEntry.BestMove.Evaluation)
                    elif storedEntry.Type=MoveTranspositionTableEntryType.AlphaUnchanged then
                            // In the rare case that alpha was unchanged, we must try and change the beta value to
                            // be the minimum value between our current beta and the stored unchanged alpha. This ensures
                            // that if alpha would remain unchanged, we would receive a beta-cutoff.
                            beta <- Math.Min(beta, storedEntry.BestMove.Evaluation)

                    if (alpha >= beta) then
#if DEBUG
                        tableCutoffCount<-tableCutoffCount+1
#endif
                        // In the case that our alpha was equal or greater than our beta, we should return the stored
                        // evaluation earlier because it was the best one possible at this transposition. Otherwise,
                        // we are required to search deeper.
                        // This happens because we edited bounds earlier.
                        ans <- storedEntry.BestMove.Evaluation|>Some

            if ans.IsSome then 
                ans.Value
            else
                // Calculate deeper ply.
                let nextPlyFromRoot = plyFromRoot + 1
                // Determine whether we should prune moves.
                let oppositeColor = PieceColor.OppositeColor(board.Board.ColorToMove)
                let kingSq = board.Board.KingLoc(board.Board.ColorToMove).ToSq()
                let mutable inCheck = MoveList.UnderAttack(board.Board, kingSq, oppositeColor)
                let mutable improving = false
                // We should use the evaluation from our transposition table if we had a hit.
                // As that evaluation isn't truly static and may have been from a previous deep search.
                let positionalEvaluation = if transpositionHit then transpositionMove.Evaluation else Evaluation.Relative(board.Board)
            
                // Also store the evaluation to later check if it improved.
                MoveSearchStack.[plyFromRoot].PositionalEvaluation <- positionalEvaluation
        
                if (node = NonPvNode && not inCheck) then
                    // Roughly estimate whether the deeper search improves the position or not.
                    improving <- plyFromRoot >= 2 && positionalEvaluation >= MoveSearchStack.[plyFromRoot - 2].PositionalEvaluation

                    if (depth < REVERSE_FUTILITY_DEPTH_THRESHOLD && Math.Abs(beta) < MATE &&
                        // If our depth is less than our threshold and our beta is less than mate on each end of the number
                        // line, then attempting reverse futility pruning is safe.
                
                        // We calculate margined positional evaluation as the difference between the current positional
                        // evaluation and a margin: D * depth + I * improving.
                        // If it is greater or equal than beta, then in most cases than not, it is futile to further evaluate
                        // this tree and hence better to just return early.
                        let improvingInt = if improving then 1 else 0
                        positionalEvaluation - REVERSE_FUTILITY_D * depth + REVERSE_FUTILITY_I * improvingInt >= beta) then
                        ans <- beta|>Some
            
                    elif (depth = 1 && positionalEvaluation + RAZORING_EVALUATION_THRESHOLD < alpha) then
                        // If after any move, the positional evaluation of the resulting position with some added threshold is
                        // less than alpha, then the opponent will be able to find at least one move that improves their
                        // position.
                        // Thus, we can avoid trying moves and jump into QSearch to get exact evaluation of the position.
                        ans <- this.QSearch(NonPvNode, board, plyFromRoot, 15, alpha, beta)|>Some
        
                    elif (notRootNode && depth > NULL_MOVE_DEPTH) then
                        // Reduced depth for null move pruning.
                        let reducedDepth = depth - NULL_MOVE_REDUCTION - 
                                           (depth / NULL_MOVE_SCALING_FACTOR - NULL_MOVE_SCALING_CORRECTION)
                
                        // For null move pruning, we give the turn to the opponent and let them make the move.
                        let mutable rv = board.NullMove()
                
                        // Then we evaluate position by searching at a reduced depth using same characteristics as normal
                        // search. The idea is that if there are cutoffs, most will be found using this reduced search and we
                        // can cutoff this branch earlier.
                        // Being reduced, it's not as expensive as the regular search (especially if we can avoid a jump into
                        // QSearch).
                        let evaluation = -this.AbSearch(NonPvNode, board, nextPlyFromRoot, reducedDepth, -beta, -beta + 1);
                
                        // Undo the null move so we can get back to original state of the board.
                        board.UndoNullMove(rv);
        
                        // In the case our evaluation was better than our beta, we achieved a cutoff here. 
                        if (evaluation >= beta) then ans <- beta|>Some

                elif (inCheck) then

                    // If we're in check, then it's better to evaluate this position deeper as to get good idea of situation,
                    // avoiding unseen blunders. Due to the number of moves being very less when under check, one shouldn't
                    // be concerned about search explosion.
                    depth <- depth + CHECK_EXTENSION

                if ans.IsSome then 
                    ans.Value
                else
                    // Reduce depth if there are no transposition hits and we're at a high enough depth to do it safely.
                    if (depth > IIR_DEPTH_THRESHOLD && not transpositionHit) then depth <- depth - IIR_DEPTH_REDUCTION

                    // Allocate memory on the stack to be used for our move-list.
                    let moveSpanarr = Array.zeroCreate<OrderedMoveEntry>(OrderedMoveList.SIZE)//stackalloc OrderedMoveEntry[OrderedMoveList.SIZE];
                    let mutable moveSpan = new Span<OrderedMoveEntry>(moveSpanarr)
                    let moveList = OrderedMoveList(moveSpan, plyFromRoot, KillerMoveTable, HistoryTable)
                    let moveCount = moveList.NormalMoveGeneration(board.Board, transpositionMove)
                    if (moveCount = 0) then
                        // If we had no moves at this depth, we should check if our king is in check. If our king is in check, it
                        // means we lost as nothing can save the king anymore. Otherwise, it's a stalemate where we can't really do
                        // anything but the opponent cannot kill our king either. It isn't a beneficial position or a position
                        // that's bad for us, so returning 0 is fine here.
                        ans <- (if inCheck then -MATE + plyFromRoot else 0)|>Some
                    if ans.IsSome then 
                        ans.Value
                    else

                        let mutable bestEvaluation = NEG_INFINITY
                        let mutable bestMoveSoFar = OrderedMoveEntry(Square.Na, Square.Na, Promotion.None)
                        let mutable transpositionTableEntryType = MoveTranspositionTableEntryType.AlphaUnchanged
                        let historyBonus = depth * depth

                        //NB this neeeds move passing in not byref -TODO: check when used that this is OK! Should be fine as I can't see it being changed. 
                        let handleEvaluation(evaluation:int,imove:OrderedMoveEntry, quietMove:bool) =
                            let mutable move = imove
                            if (evaluation <= bestEvaluation) then true
                            else
                                // If our evaluation was better than our current best evaluation, we should update our evaluation
                                // with the new evaluation. We should also take into account that it was our best move so far.
                                bestEvaluation <- evaluation
                                bestMoveSoFar <- move

                                if node = PvNode then
                                    // Insert move into PV Table.
                                    PvTable.Insert(plyFromRoot, &move)
            
                                    // Copy moves from lesser ply to current ply PV Line.
                                    let mutable nextPly = plyFromRoot + 1
                                    while (PvTable.PlyInitialized(plyFromRoot, nextPly)) do
                                        PvTable.Copy(plyFromRoot, nextPly)
                                        nextPly <- nextPly+1
            
                                    // Update our PV Length.
                                    PvTable.UpdateLength(plyFromRoot)

                                if (evaluation <= alpha) then true
                                else

                                    // If our evaluation was better than our alpha (best unavoidable evaluation so far), then we should
                                    // replace our alpha with our evaluation.
                                    alpha <- evaluation
            
                                    // Our alpha changed, so it is no longer an unchanged alpha entry.
                                    transpositionTableEntryType <- MoveTranspositionTableEntryType.Exact
            
                                    // If the evaluation was better than beta, it means the position was too good. Thus, there
                                    // is a good chance that the opponent will avoid this path. Hence, there is currently no
                                    // reason to evaluate it further.
                                    evaluation < beta

                        // Calculate next iteration variables before getting into the loop.
                        let nextDepth = depth - 1
            
                        let mutable i = 0
                        let mutable quietMoveCounter = 0
                        let lmpQuietThreshold = LMP_QUIET_THRESHOLD_BASE + depth * depth;
                        let lmp = notRootNode && (not inCheck) && depth <= LMP_DEPTH_THRESHOLD
                        let lmr = depth >= LMR_DEPTH_THRESHOLD && (not inCheck)

                        let mutable keepgoing = true
                        while (i < moveCount && keepgoing) do
                            // We should being the move that's likely to be the best move at this depth to the top. This ensures
                            // that we are searching through the likely best moves first, allowing us to return early.
                            moveList.SortNext(i, moveCount)
                            let previousNodeCount = this.TotalNodeSearchCount
                            let mutable move = moveList.[i]

                            let quietMove = not (board.Board.All(oppositeColor).[move.To])
                            let quietInt = if quietMove then 1 else 0
                            quietMoveCounter <- quietMoveCounter + quietInt

                            //Futility Pruning
                            if (i > 0 && quietMove && positionalEvaluation + depth * FUTILITY_DEPTH_FACTOR <= alpha) then
                                // If our move is a quiet and static evaluation of a position with a depth-relative margin is below
                                // our alpha, then the move won't really help us improve our position. And nor will any future move.
                                // Hence, it's futile to evaluate this position any further.
                                keepgoing <- false
                            //Late Move Pruning
                            elif (node = NonPvNode && lmp && bestEvaluation > NEG_INFINITY && quietMoveCounter > lmpQuietThreshold) then 
                                // If we are past a certain threshold and we have searched the required quiet moves for this depth for
                                // pruning to be relatively safe, we can avoid searching any more moves since the likely best move
                                // will have been determined by now.
                                keepgoing <- false
                            else

                                // Make the move.
                                let mutable rv = board.Move(&move)
                                totalNodeSearchCount <- totalNodeSearchCount+1
            
                                let mutable evaluation = 0
            
                                if (i = 0) then
                                    // If we haven't searched any moves, we should do a full depth search without any reductions.
                                    // Without a full depth search beforehand, there's no way to guarantee principle variation search being
                                    // safe.
                                    evaluation <- -this.AbSearch(node, board, nextPlyFromRoot, nextDepth, -beta, -alpha)
                                else 
                                    // Otherwise, to speed up search, we should try applying certain reductions to see if we can speed up
                                    // the search. Moreover, even if those reductions are still unsafe, we can still save time by trying
                                    // search inside our principle variation window. In most cases, this will allow us to get a beta cutoff
                                    // earlier.
                
                                    //Late Move Reduction
                
                                    if (i >= LMR_FULL_SEARCH_THRESHOLD && lmr) then
                                        // If we're past the move count and depth threshold where we can usually safely apply LMR and we
                                        // also aren't in check, then we can reduce the depth of the subtree, speeding up search.

                                        // Logarithmic reduction: ln(depth) * ln(i) / 2 - 0.2
                                        let mutable r = MoveSearch.ReductionDepthTable.[depth, i]
                                        // Reduce more on non-PV nodes.
                                        if (node <> PvNode) then r <- r+1
                                        // Reduce if not improving.
                                        if (not improving) then r <- r+1
                                        // Determine the reduced depth. Ensure it's >= 1 as we want to avoid dropping into QSearch.
                                        let reducedDepth = Math.Max(depth - r, 1)
                                        // Evaluate the position by searching at a reduced depth. The idea is that these moves will likely
                                        // not improve alpha, and thus not trigger researches. Therefore, one will be able to get away with
                                        // reduced depth searches with reasonable safety. Result is negated as an evaluation that's good
                                        // for our opponent will be bad for us.
                                        evaluation <- 
                                            -this.AbSearch(NonPvNode, board, nextPlyFromRoot, reducedDepth, -alpha - 1, -alpha)
                
                                        // In the case that LMR fails, our evaluation will be greater than alpha which will force a
                                        // principle variation research. However, in the case we couldn't apply LMR (due to safety reasons,
                                        // setting the evaluation to be a value greater than alpha allows us to force the principle
                                        // variation search.
                                    else evaluation <- alpha + 1
                                    //Principle Variation Search
            
                                    if (evaluation > alpha) then
                                        // If we couldn't attempt LMR because it was unsafe or if LMR failed, we should try a null-window
                                        // search at a normal progressive depth. If this is a research, it'll likely be fast enough to have
                                        // no impact due to transposition tables.
                                        evaluation <- -this.AbSearch(NonPvNode, board, nextPlyFromRoot, nextDepth, -alpha - 1, -alpha)

                                        if (evaluation > alpha && evaluation < beta) then
                                            // If our evaluation was good enough to change our alpha but not our beta, it means we're on a
                                            // principle variation node. Essentially: beta - alpha > 1.
                                            // This means this is our best move from the search, and it isn't too good to be deemed
                                            // an unlikely path. Thus, we should evaluate it clearly using a full-window research.
                                            evaluation <- -this.AbSearch(PvNode, board, nextPlyFromRoot, nextDepth, -beta, -alpha)
            
                                // Undo the move.
                                board.UndoMove(&rv)
                                if not (handleEvaluation(evaluation, move, quietMove)) then
                                    if (quietMove) then
                                        if (KillerMoveTable.[0, plyFromRoot] <> move) then
                                            // Given this move isn't a capture move (quiet move), we store it as a killer move (cutoff move)
                                            // to better sort quiet moves like these in the future, allowing us to achieve a cutoff faster.
                                            // Also make sure we are not saving same move in both of our caches.
                                            KillerMoveTable.ReOrder(plyFromRoot)
                                            KillerMoveTable.[0, plyFromRoot] <- move
                    
                                        // Increment the move that caused a beta cutoff to get a historical heuristic of best quiet moves.
                                        HistoryTable.[board.PieceOnly(move.From), board.Board.ColorToMove, move.To] <- HistoryTable.[board.PieceOnly(move.From), board.Board.ColorToMove, move.To] + historyBonus
                    
                                        // Decrement all other quiet moves to ensure a branch local history heuristic.
                                        let mutable j = 1
                                        while (j < quietMoveCounter) do
                                            let otherMove = moveList.[i - j]
                                            HistoryTable.[board.PieceOnly(otherMove.From), board.Board.ColorToMove, otherMove.To] <- HistoryTable.[board.PieceOnly(otherMove.From), board.Board.ColorToMove, otherMove.To] - historyBonus
                                            j <- j+1

                                    // We had a beta cutoff, hence it's a beta cutoff entry.
                                    transpositionTableEntryType <- MoveTranspositionTableEntryType.BetaCutoff
                                    keepgoing <- false

                                if (rootNode) then SearchEffort.[move.From, move.To] <- this.TotalNodeSearchCount - previousNodeCount
            
                                i <- i+1
                        
                        let bestMove = SearchedMove(&bestMoveSoFar, bestEvaluation)
                        let mutable entry = MoveTranspositionTableEntry(board.Board.ZobristHash, transpositionTableEntryType, bestMove, depth)
                        Table.Value.InsertEntry(board.Board.ZobristHash, &entry)

                        bestEvaluation
    member this.AspirationSearch(board:EngineBoard, depth:int, previousEvaluation:int, bestMove:byref<OrderedMoveEntry>) =
        // Set base window size.
        let mutable alpha = NEG_INFINITY
        let mutable beta = POS_INFINITY

        if (depth > ASPIRATION_DEPTH) then
            // If we're searching deeper than our aspiration depth, then we should modify the window based on our
            // previous evaluation and aspiration size. If the window isn't reasonably correct, it'll get reset later
            // anyways.
            alpha <- previousEvaluation - ASPIRATION_SIZE
            beta <- previousEvaluation + ASPIRATION_SIZE

        let mutable research = 0
        let mutable ans = None
        while (ans.IsNone) do
            // If we're out of time, we should exit the search as fast as possible.
            // NOTE: Due to the nature of this exit (using exceptions to do it as fast as possible), the board state
            // is not reverted. Thus, a cloned board must be provided.
            if (TimeControl.Finished()) then raise (OperationCanceledException())
            // We should reset our window if it's too far gone because the gradual increase isn't working.
            // In the case our alpha is far below our aspiration bound, we should reset it to negative infinity for
            // our research.
            if (alpha < -ASPIRATION_BOUND) then alpha <- NEG_INFINITY
            // In the case our beta is far too above our aspiration bound, we should reset it to positive infinity for
            // our research.
            if (beta > ASPIRATION_BOUND) then beta <- POS_INFINITY
            // Get our best evaluation so far so we can decide whether we need to do a research or not.
            // Researches are reasonably fast thanks to transposition tables.
            let bestEvaluation = this.AbSearch(PvNode, board, 0, depth, alpha, beta)
            //Modify Window

            if (bestEvaluation <= alpha) then
                research <- research+1                
                // If our best evaluation was somehow worse than our alpha, we should resize our window and research.
                alpha <- Math.Max(alpha - research * research * ASPIRATION_DELTA, NEG_INFINITY)
            elif (bestEvaluation >= beta) then
                research <- research+1 
                
                // If our evaluation was somehow better than our beta, we should resize our window and research.
                beta <- Math.Min(beta + research * research * ASPIRATION_DELTA, POS_INFINITY)
                
                // Update our best move in case our evaluation was better than beta.
                // The move we get in future surely can't be worse than this so it's fine to update our best move
                // directly on a beta cutoff.
                bestMove <- PvTable.Get(0);

                // If our evaluation was within our window, we should return the result avoiding any researches.
            else 
                ans <- bestEvaluation|>Some

        ans.Value
    member this.IterativeDeepening(selectedDepth:int) =
        let mutable bestMove = OrderedMoveEntry.Default
        let mutable evaluation = NEG_INFINITY
        try 
            let mutable depth = 1
            let stopwatch = Stopwatch.StartNew()
            let mutable timePreviouslyUpdated = false
            let mutable keepgoing = true
            while (keepgoing && not (TimeControl.Finished()) && depth <= selectedDepth) do
                evaluation <- this.AspirationSearch(Board.Value, depth, evaluation, &bestMove)
                bestMove <- PvTable.Get(0)

                // Try counting nodes to see if we can exit the search early.
                timePreviouslyUpdated <- this.NodeCounting(depth, bestMove, timePreviouslyUpdated)
                
                this.DepthSearchLog(depth, evaluation, stopwatch)
                
                // In the case we are past a certain depth, and are really low on time, it's highly unlikely we'll
                // finish the next depth in time. To save time, we should just exit the search early.
                if (depth > 5 && float(TimeControl.TimeLeft()) <= float(TimeControl.Time) * TIME_TO_DEPTH_THRESHOLD) then keepgoing <- false
                
                depth <- depth+1
        with
            | :? OperationCanceledException -> ()
        
        NNUE.ResetAccumulator()
        bestMove
        