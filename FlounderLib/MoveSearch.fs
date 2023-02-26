namespace FlounderLib
open System
open System.Diagnostics
open System.Text
open System.Runtime.CompilerServices

type MoveSearch =
#if DEBUG
    val mutable TableCutoffCount:int
#endif
    val mutable TotalNodeSearchCount:int
    val mutable SelectiveDepth:int 
    val mutable HistTbl:HistoryTable
    val mutable KillerMvTbl:KillerMoveTable
    val mutable SearchEffort:MoveSearchEffortTable
    val mutable PvTable:PrincipleVariationTable
    val mutable MvSrchStck:MoveSearchStack
    val mutable ReducedTimeMove:OrderedMoveEntry
    val mutable EngBrd:EngineBoard option
    val mutable TimeCntrl:TimeControl
    val mutable MvTrnTbl:MoveTranDict option
    new(board:EngineBoard, table:MoveTranDict, timeControl:TimeControl) =
        {
#if DEBUG
            TableCutoffCount = 0
#endif
            TotalNodeSearchCount = 0
            SelectiveDepth = 0
            HistTbl = HistoryTable.Default
            KillerMvTbl = KillerMoveTable.Default
            SearchEffort = MoveSearchEffortTable.Default
            PvTable = PrincipleVariationTable.Default
            MvSrchStck = MoveSearchStack.Default
            ReducedTimeMove = OrderedMoveEntry.Default
            EngBrd = Some(board)
            TimeCntrl = timeControl
            MvTrnTbl = Some(table)
        }
    member this.PvLine() = 
        let pv = StringBuilder()
        let count = this.PvTable.Count()
        for i = 0 to count-1 do
            let move:OrderedMoveEntry = this.PvTable.Get(i)
            pv.Append(move.From).Append(move.To)|>ignore
            if move.Promotion <> Promotion.None then pv.Append(Promotion.ToStr(move.Promotion))|>ignore
            pv.Append(' ')|>ignore
        pv.ToString().ToLower()
    member this.NodeCounting(depth:int, bestMove:OrderedMoveEntry, itimePreviouslyUpdated:bool) = 
        let mutable timePreviouslyUpdated = itimePreviouslyUpdated
        // This idea is from the Koivisto Engine:
        // The branch being searched the most is likely the best branch as we're having to evaluate it very deeply
        // across depths. Thus it's reasonable to end the search earlier and make the move instantly.
        // Check whether we're past the depth to start reducing our search time with node counting and make sure that
        // we're past the required effort threshold to do this move quickly.
        if depth >= 8 && this.TimeCntrl.TimeLeft() <> 0 && not timePreviouslyUpdated
           && this.SearchEffort.[bestMove.From, bestMove.To] * 100 / this.TotalNodeSearchCount >= 95 then
            timePreviouslyUpdated <- true
            this.TimeCntrl.ChangeTime(this.TimeCntrl.Time / 3)
            this.ReducedTimeMove <- bestMove
        if timePreviouslyUpdated && bestMove <> this.ReducedTimeMove then
            // In the rare case that our previous node count guess was incorrect, give us a little bit more time
            // to see if we can find a better move.
            this.TimeCntrl.ChangeTime(this.TimeCntrl.Time * 3)
        timePreviouslyUpdated
    member this.DepthSearchLog(depth:int, evaluation:int, stopwatch:Stopwatch) =
        let elapSec = float(stopwatch.ElapsedMilliseconds) / 1000.0
        let ratio = int(float(this.TotalNodeSearchCount) / elapSec)
        Console.Write(
            "info depth " + depth.ToString() + " seldepth " + this.SelectiveDepth.ToString() + " score cp " + evaluation.ToString() + " nodes " + 
            this.TotalNodeSearchCount.ToString() + " nps " + ratio.ToString() 
            + " pv " + this.PvLine() + "\n"
        )
    member this.QSearch(isPvNode:bool, board:EngineBoard, plyFromRoot:int, depth:int, ialpha:int, beta:int) =
        let mutable alpha = ialpha
        //// If we're out of time, we should exit the search as fast as possible.
        //// NOTE: Due to the nature of this exit (using exceptions to do it as fast as possible), the board state
        //// is not reverted. Thus, a cloned board must be provided.
        if (this.TimeCntrl.Finished()) then raise (OperationCanceledException())
        if isPvNode then this.SelectiveDepth <- Math.Max(this.SelectiveDepth, plyFromRoot)
        let mutable ans = None 
        if not isPvNode then
            let storedEntry = this.MvTrnTbl.Value.[board.Brd.ZobristHash]
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
                ans <- storedEntry.BestMove.Evaluation|>Some
        if ans.IsSome then 
            ans.Value
        else
            let mutable earlyEval = Evaluation.Relative(board.Brd)
            // In the rare case our evaluation is already too good, we don't need to further evaluate captures any further,
            // as this position is overwhelmingly winning.
            if earlyEval >= beta then ans <- beta|>Some
            if ans.IsSome then ans.Value
            else
                // In the case that our current evaluation is better than our alpha, we need to recalibrate alpha to make sure
                // we don't skip over our already good move.
                if earlyEval > alpha then alpha <- earlyEval
                // Allocate memory on the stack to be used for our move-list.
                let moveSpanarr = Array.zeroCreate<OrderedMoveEntry>(OrderedMoveList.SIZE)//stackalloc OrderedMoveEntry[OrderedMoveList.SIZE];
                let mutable moveSpan = new Span<OrderedMoveEntry>(moveSpanarr)
                let moveList = OrderedMoveList(moveSpan, plyFromRoot, this.KillerMvTbl, this.HistTbl)
                let moveCount = moveList.QSearchMoveGeneration(board.Brd, SearchedMove.Default)
                let mutable bestEvaluation = earlyEval
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
                    let seeEval = see + earlyEval
                    if (seeEval > beta) then 
                        bestEvaluation <- seeEval
                        keepgoing <- false
                    else
                        // Make the move.
                        let mutable rv = board.Move(&move)
                        this.TotalNodeSearchCount <- this.TotalNodeSearchCount+1
                        // Evaluate position by searching deeper and negating the result. An evaluation that's good for
                        // our opponent will obviously be bad for us.
                        let evaluation = -this.QSearch(isPvNode, board, nextPlyFromRoot, nextDepth, -beta, -alpha)
                        // Undo the move.
                        board.UndoMove(&rv)
                        if not (this.HandleEvaluationQ(evaluation,&bestEvaluation,&alpha,beta)) then 
                            keepgoing <- false
                    i <- i+1
                bestEvaluation
    member this.AbSearch(isPvNode:bool, board:EngineBoard, plyFromRoot:int, idepth:int, ialpha:int, ibeta:int) =
        let mutable alpha = ialpha
        let mutable beta = ibeta
        let mutable ans = None
        // If we're out of time, we should exit the search as fast as possible.
        // NOTE: Due to the nature of this exit (using exceptions to do it as fast as possible), the board state
        // is not reverted. Thus, a cloned board must be provided.
        if (this.TimeCntrl.Finished()) then raise (OperationCanceledException())
        if isPvNode then 
            this.PvTable.InitializeLength(plyFromRoot)
            this.SelectiveDepth <- Math.Max(this.SelectiveDepth, plyFromRoot)
        // At depth 0 (or less in the case of reductions etc.), since we may be having a capture train, we should
        // jump into QSearch and evaluate even deeper. In the case of no captures available, QSearch will throw us
        // out instantly.
        let rootNode = plyFromRoot = 0
        if idepth <= 0 then ans <- this.QSearch(isPvNode, board, plyFromRoot, 15, alpha, beta)|>Some
        else
            if not rootNode then
                // We had a three-fold repetition, so return earlier.
                if board.IsRepetition() then ans <- 0|>Some
                else
                    let allPiecesCount = board.Brd.All().Count
                    // If only the kings are left, it's a draw.
                    if allPiecesCount = 2 then ans <- 0|>Some
                    else
                        let knightLeft = board.Brd.All(Piece.Knight, PieceColor.White).ToBool() || board.Brd.All(Piece.Knight, PieceColor.Black).ToBool()
                        // If only the kings and one knight is left, it's a draw.
                        if (allPiecesCount = 3 && knightLeft) then ans <- 0|>Some
                        else
                            let bishopLeft = board.Brd.All(Piece.Bishop, PieceColor.White).ToBool() || board.Brd.All(Piece.Bishop, PieceColor.Black).ToBool()
                            // If only the kings and one bishop is left, it's a draw.
                            if allPiecesCount = 3 && bishopLeft then ans <- 0|>Some
                            else
                                // If we are not at the root, we should check and see if there is a ready mate.
                                // If there is, we shouldn't really care about other moves or slower mates, but instead
                                // we should prune as fast as possible. It's crucial to ensuring we hit high depths.
                                alpha <- Math.Max(alpha, plyFromRoot - 99999999)
                                beta <- Math.Min(beta, 99999999 - plyFromRoot)
                                if (alpha >= beta) then ans <- alpha|>Some
        if ans.IsSome then 
            ans.Value
        else
            let storedEntry = this.MvTrnTbl.Value.[board.Brd.ZobristHash]
            let valid = storedEntry.Type <> MoveTranspositionTableEntryType.Invalid
            let mutable transpositionMove = SearchedMove.Default
            let mutable transpositionHit = false

            if valid then
                // We had a transposition table hit. However, at this point, we don't know if this is a trustworthy
                // transposition hit or not.
                transpositionMove <- storedEntry.BestMove
                transpositionHit <- true
                if not isPvNode && int(storedEntry.Depth) >= idepth then
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
                    if alpha >= beta then
#if DEBUG
                        this.TableCutoffCount<-this.TableCutoffCount+1
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
                let oppositeColor = PieceColor.OppositeColor(board.Brd.ColorToMove)
                let kingSq = board.Brd.KingLoc(board.Brd.ColorToMove).ToSq()
                let mutable inCheck = MoveList.UnderAttack(board.Brd, kingSq, oppositeColor)
                let mutable improving = false
                // We should use the evaluation from our transposition table if we had a hit.
                // As that evaluation isn't truly static and may have been from a previous deep search.
                let positionalEvaluation = if transpositionHit then transpositionMove.Evaluation else Evaluation.Relative(board.Brd)
                // Also store the evaluation to later check if it improved.
                this.MvSrchStck.[plyFromRoot].PositionalEvaluation <- positionalEvaluation
        
                if not isPvNode && not inCheck then
                    // Roughly estimate whether the deeper search improves the position or not.
                    improving <- plyFromRoot >= 2 && positionalEvaluation >= this.MvSrchStck.[plyFromRoot - 2].PositionalEvaluation
                    // If our depth is less than our threshold and our beta is less than mate on each end of the number
                    // line, then attempting reverse futility pruning is safe.
                    // We calculate margined positional evaluation as the difference between the current positional
                    // evaluation and a margin: D * depth + I * improving.
                    // If it is greater or equal than beta, then in most cases than not, it is futile to further evaluate
                    // this tree and hence better to just return early.
                    let improvingInt = if improving then 1 else 0

                    if idepth < 7 && Math.Abs(beta) < 99999999 && positionalEvaluation - 67 * idepth + 76 * improvingInt >= beta then
                        ans <- beta|>Some
                    elif idepth = 1 && positionalEvaluation + 150 < alpha then
                        // If after any move, the positional evaluation of the resulting position with some added threshold is
                        // less than alpha, then the opponent will be able to find at least one move that improves their
                        // position.
                        // Thus, we can avoid trying moves and jump into QSearch to get exact evaluation of the position.
                        ans <- this.QSearch(false, board, plyFromRoot, 15, alpha, beta)|>Some
                    elif not rootNode && idepth > 2 then
                        let evaluation = this.NullMovePrune(board,nextPlyFromRoot,idepth,beta)
                        // In the case our evaluation was better than our beta, we achieved a cutoff here. 
                        if evaluation >= beta then ans <- beta|>Some

                if ans.IsSome then 
                    ans.Value
                else
                    // If we're in check, then it's better to evaluate this position deeper as to get good idea of situation,
                    // avoiding unseen blunders. Due to the number of moves being very less when under check, one shouldn't
                    // be concerned about search explosion.                    
                    let cdepth = if inCheck then idepth + 1 else idepth
                    // Reduce depth if there are no transposition hits and we're at a high enough depth to do it safely.
                    let depth = if (cdepth > 3 && not transpositionHit) then cdepth - 1 else cdepth
                    // Allocate memory on the stack to be used for our move-list.
                    let moveSpanarr = Array.zeroCreate<OrderedMoveEntry>(OrderedMoveList.SIZE)
                    let mutable moveSpan = new Span<OrderedMoveEntry>(moveSpanarr)
                    let moveList = OrderedMoveList(moveSpan, plyFromRoot, this.KillerMvTbl, this.HistTbl)
                    let moveCount = moveList.NormalMoveGeneration(board.Brd, transpositionMove)
                    if moveCount = 0 then
                        // If we had no moves at this depth, we should check if our king is in check. If our king is in check, it
                        // means we lost as nothing can save the king anymore. Otherwise, it's a stalemate where we can't really do
                        // anything but the opponent cannot kill our king either. It isn't a beneficial position or a position
                        // that's bad for us, so returning 0 is fine here.
                        if inCheck then -99999999 + plyFromRoot else 0
                    else
                        let mutable bestEvaluation = -100000000
                        let mutable bestMoveSoFar = OrderedMoveEntry(Square.Na, Square.Na, Promotion.None)
                        let mutable transpositionTableEntryType = MoveTranspositionTableEntryType.AlphaUnchanged
                        // Calculate next iteration variables before getting into the loop.
                        let nextDepth = depth - 1
                        let mutable i = 0
                        let mutable quietMoveCounter = 0
                        let lmpQuietThreshold = 3 + depth * depth
                        let lmp = not rootNode && not inCheck && depth <= 3

                        let mutable keepgoing = true
                        while (i < moveCount && keepgoing) do
                            // We should being the move that's likely to be the best move at this depth to the top. This ensures
                            // that we are searching through the likely best moves first, allowing us to return early.
                            moveList.SortNext(i, moveCount)
                            let previousNodeCount = this.TotalNodeSearchCount
                            let mutable move = moveList.[i]
                            let quietMove = not (board.Brd.All(oppositeColor).[move.To])
                            if quietMove then quietMoveCounter <- quietMoveCounter + 1
                            //Late Move Pruning
                                // If we are past a certain threshold and we have searched the required quiet moves for this depth for
                                // pruning to be relatively safe, we can avoid searching any more moves since the likely best move
                                // will have been determined by now.
                            let lmpTest = not isPvNode && lmp && bestEvaluation > -100000000 && quietMoveCounter > lmpQuietThreshold
                            if lmpTest then keepgoing <- false
                            else
                                // Make the move.
                                let mutable rv = board.Move(&move)
                                this.TotalNodeSearchCount <- this.TotalNodeSearchCount+1
                                let mutable evaluation = 
                                    if i = 0 then
                                        // If we haven't searched any moves, we should do a full depth search without any reductions.
                                        // Without a full depth search beforehand, there's no way to guarantee principle variation search being
                                        // safe.
                                        -this.AbSearch(isPvNode, board, nextPlyFromRoot, nextDepth, -beta, -alpha)
                                    else 
                                        alpha + 1
                                //Principle Variation Search
                                if i > 0 && evaluation > alpha then
                                    this.PvSearch(&evaluation,board,nextPlyFromRoot,nextDepth,alpha,beta)
                                // Undo the move.
                                board.UndoMove(&rv)
                                if not (this.HandleEvaluation(evaluation, move, &bestEvaluation,&bestMoveSoFar,isPvNode,plyFromRoot,&alpha,beta,&transpositionTableEntryType)) then
                                    if quietMove then
                                        this.DoQuiet(plyFromRoot,move,board,depth,quietMoveCounter,moveList,i)
                                    // We had a beta cutoff, hence it's a beta cutoff entry.
                                    transpositionTableEntryType <- MoveTranspositionTableEntryType.BetaCutoff
                                    keepgoing <- false
                                if rootNode then this.SearchEffort.[move.From, move.To] <- this.TotalNodeSearchCount - previousNodeCount
                                i <- i + 1
                        
                        let bestMove = SearchedMove(&bestMoveSoFar, bestEvaluation)
                        let mutable entry = MoveTranDictEntry(transpositionTableEntryType, bestMove, depth)
                        this.MvTrnTbl.Value.InsertEntry(board.Brd.ZobristHash, &entry)

                        bestEvaluation
    member this.AspirationSearch(board:EngineBoard, depth:int, previousEvaluation:int) =
        // Set base window size.
        let mutable alpha = -100000000
        let mutable beta = 100000000
        if depth > 4 then
            // If we're searching deeper than our aspiration depth, then we should modify the window based on our
            // previous evaluation and aspiration size. If the window isn't reasonably correct, it'll get reset later
            // anyways.
            alpha <- previousEvaluation - 16
            beta <- previousEvaluation + 16
        let rec geteval res =
            // If we're out of time, we should exit the search as fast as possible.
            // NOTE: Due to the nature of this exit (using exceptions to do it as fast as possible), the board state
            // is not reverted. Thus, a cloned board must be provided.
            if this.TimeCntrl.Finished() then raise (OperationCanceledException())
            // We should reset our window if it's too far gone because the gradual increase isn't working.
            // In the case our alpha is far below our aspiration bound, we should reset it to negative infinity for
            // our research.
            if (alpha < -3500) then alpha <- -100000000
            // In the case our beta is far too above our aspiration bound, we should reset it to positive infinity for
            // our research.
            if (beta > 3500) then beta <- 100000000
            // Get our best evaluation so far so we can decide whether we need to do a research or not.
            // Researches are reasonably fast thanks to transposition tables.
            let bestEvaluation = this.AbSearch(true, board, 0, depth, alpha, beta)
            //Modify Window
            if bestEvaluation <= alpha then
                let newres = res + 1                
                // If our best evaluation was somehow worse than our alpha, we should resize our window and research.
                alpha <- Math.Max(alpha - newres * newres * 23, -100000000)
                geteval newres
            elif bestEvaluation >= beta then
                let newres = res + 1 
                // If our evaluation was somehow better than our beta, we should resize our window and research.
                beta <- Math.Min(beta + newres * newres * 23, 100000000)
                geteval newres
                // If our evaluation was within our window, we should return the result avoiding any researches.
            else 
                bestEvaluation
        geteval 0
    member this.IterativeDeepening(selectedDepth:int) =
        let mutable bestMove = OrderedMoveEntry.Default
        try 
            let stopwatch = Stopwatch.StartNew()
            let mutable timePreviouslyUpdated = false
            let rec getbm cureval curdepth =
                if not (this.TimeCntrl.Finished() || curdepth > selectedDepth) then
                    let eval = this.AspirationSearch(this.EngBrd.Value, curdepth, cureval)
                    bestMove <- this.PvTable.Get(0)
                    // Try counting nodes to see if we can exit the search early.
                    timePreviouslyUpdated <- this.NodeCounting(curdepth, bestMove, timePreviouslyUpdated)
                    this.DepthSearchLog(curdepth, eval, stopwatch)
                    // In the case we are past a certain depth, and are really low on time, it's highly unlikely we'll
                    // finish the next depth in time. To save time, we should just exit the search early.
                    if not (curdepth > 5 && float(this.TimeCntrl.TimeLeft()) <= float(this.TimeCntrl.Time) * 0.2) then
                        getbm eval (curdepth + 1)
            getbm -100000000 1                    
        with
            | :? OperationCanceledException -> ()
        NNUE.ResetAccumulator()
        bestMove
    member this.DoTest(selectedDepth:int, bm:string) =
        let mutable bestMove = OrderedMoveEntry.Default
        try 
            let stopwatch = Stopwatch.StartNew()
            let mutable timePreviouslyUpdated = false
            let rec getbm cureval curdepth =
                if not (this.TimeCntrl.Finished() || curdepth > selectedDepth) then
                    let eval = this.AspirationSearch(this.EngBrd.Value, curdepth, cureval)
                    bestMove <- this.PvTable.Get(0)
                    // Try counting nodes to see if we can exit the search early.
                    timePreviouslyUpdated <- this.NodeCounting(curdepth, bestMove, timePreviouslyUpdated)
                    this.DepthSearchLog(curdepth, eval, stopwatch)
                    // In the case we are past a certain depth, and are really low on time, it's highly unlikely we'll
                    // finish the next depth in time. To save time, we should just exit the search early.
                    if not (bm = bestMove.ToString()) then
                        getbm eval (curdepth + 1)
            getbm -100000000 1                    
        with
            | :? OperationCanceledException -> ()
        NNUE.ResetAccumulator()
        bestMove
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.HandleEvaluationQ(evaluation:int, bestEvaluation:byref<int>, alpha:byref<int>, beta:int) =
        if evaluation <= bestEvaluation then true
        // If our evaluation was better than our current best evaluation, we should update our evaluation
        // with the new evaluation.
        else
            bestEvaluation <- evaluation
            if evaluation <= alpha then true
            else
                // If our evaluation was better than our alpha (best unavoidable evaluation so far), then we should
                // replace our alpha with our evaluation.
                alpha <- evaluation
                // If the evaluation was better than beta, it means the position was too good. Thus, there
                // is a good chance that the opponent will avoid this path. Hence, there is currently no
                // reason to evaluate it further.
                evaluation < beta
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.NullMovePrune(board:EngineBoard, nextPlyFromRoot:int, idepth:int, beta:int) =        
        // Reduced depth for null move pruning.
        let reducedDepth = idepth - 4 - (idepth / 3 - 1)
        // For null move pruning, we give the turn to the opponent and let them make the move.
        let mutable rv = board.NullMove()
        // Then we evaluate position by searching at a reduced depth using same characteristics as normal
        // search. The idea is that if there are cutoffs, most will be found using this reduced search and we
        // can cutoff this branch earlier.
        // Being reduced, it's not as expensive as the regular search (especially if we can avoid a jump into
        // QSearch).
        let evaluation = -this.AbSearch(false, board, nextPlyFromRoot, reducedDepth, -beta, -beta + 1);
        // Undo the null move so we can get back to original state of the board.
        board.UndoNullMove(rv)
        evaluation
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.PvSearch(evaluation:byref<int>, board:EngineBoard, nextPlyFromRoot:int, nextDepth:int, alpha:int, beta:int) =
        // If we couldn't attempt LMR because it was unsafe or if LMR failed, we should try a null-window
        // search at a normal progressive depth. If this is a research, it'll likely be fast enough to have
        // no impact due to transposition tables.
        evaluation <- -this.AbSearch(false, board, nextPlyFromRoot, nextDepth, -alpha - 1, -alpha)
        if (evaluation > alpha && evaluation < beta) then
            // If our evaluation was good enough to change our alpha but not our beta, it means we're on a
            // principle variation node. Essentially: beta - alpha > 1.
            // This means this is our best move from the search, and it isn't too good to be deemed
            // an unlikely path. Thus, we should evaluate it clearly using a full-window research.
            evaluation <- -this.AbSearch(true, board, nextPlyFromRoot, nextDepth, -beta, -alpha)
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.HandleEvaluation(evaluation:int, move:OrderedMoveEntry, bestEvaluation:byref<int>, bestMoveSoFar:byref<OrderedMoveEntry>, isPvNode:bool, plyFromRoot:int, alpha:byref<int>, beta:int, transpositionTableEntryType:byref<MoveTranspositionTableEntryType>) =
        if (evaluation <= bestEvaluation) then true
        else
            // If our evaluation was better than our current best evaluation, we should update our evaluation
            // with the new evaluation. We should also take into account that it was our best move so far.
            bestEvaluation <- evaluation
            bestMoveSoFar <- move
            if isPvNode then
                // Insert move into PV Table.
                this.PvTable.Insert(plyFromRoot, &bestMoveSoFar)
                // Copy moves from lesser ply to current ply PV Line.
                let mutable nextPly = plyFromRoot + 1
                while (this.PvTable.PlyInitialized(plyFromRoot, nextPly)) do
                    this.PvTable.Copy(plyFromRoot, nextPly)
                    nextPly <- nextPly+1
                // Update our PV Length.
                this.PvTable.UpdateLength(plyFromRoot)
            if evaluation <= alpha then true
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
    [<MethodImpl(MethodImplOptions.AggressiveInlining)>]
    member this.DoQuiet(plyFromRoot:int, move:OrderedMoveEntry, board:EngineBoard,depth:int, quietMoveCounter, moveList:OrderedMoveList, i:int) =
        let historyBonus = depth * depth
        if this.KillerMvTbl.[0, plyFromRoot] <> move then
            // Given this move isn't a capture move (quiet move), we store it as a killer move (cutoff move)
            // to better sort quiet moves like these in the future, allowing us to achieve a cutoff faster.
            // Also make sure we are not saving same move in both of our caches.
            this.KillerMvTbl.ReOrder(plyFromRoot)
            this.KillerMvTbl.[0, plyFromRoot] <- move
        // Increment the move that caused a beta cutoff to get a historical heuristic of best quiet moves.
        this.HistTbl.[board.PieceOnly(move.From), board.Brd.ColorToMove, move.To] <- this.HistTbl.[board.PieceOnly(move.From), board.Brd.ColorToMove, move.To] + historyBonus
        // Decrement all other quiet moves to ensure a branch local history heuristic.
        for j = 1 to quietMoveCounter-1 do
            let otherMove = moveList.[i - j]
            this.HistTbl.[board.PieceOnly(otherMove.From), board.Brd.ColorToMove, otherMove.To] <- this.HistTbl.[board.PieceOnly(otherMove.From), board.Brd.ColorToMove, otherMove.To] - historyBonus
