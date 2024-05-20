namespace FlounderLib
open System
open System.Diagnostics
open System.Text

module Search =
    let Reset() =
        Srch.NodeCount <- 0
        Srch.SelDepth <- 0
        Hist.Clear()
        KillMv.Clear()
        SrchEff.Clear()
        PrincVars.Clear()
        SrchStack.Clear()
        Srch.RedTimeMove <- OrdMove.Default
    let HandleEvalQ(evaluation:int, bestEvaluation:byref<int>, alpha:byref<int>, beta:int) =
        if evaluation <= bestEvaluation then true
        else
            bestEvaluation <- evaluation
            if evaluation <= alpha then true
            else
                alpha <- evaluation
                evaluation < beta
    let rec QSearch(isPvNode:bool, plyFromRoot:int, depth:int, ialpha:int, beta:int) =
        let mutable alpha = ialpha
        if (TimeCntrl.Finished()) then raise (OperationCanceledException())
        if isPvNode then Srch.SelDepth <- Math.Max(Srch.SelDepth, plyFromRoot)
        let mutable ans = None 
        if not isPvNode then
            let storedEntry = TranTable.GetEntry(Brd.ZobristHash)
            if (storedEntry.Hash = Brd.ZobristHash &&
                (storedEntry.Type = Exact ||
                storedEntry.Type = BetaCutoff &&
                storedEntry.BestMove.Score >= beta ||
                storedEntry.Type = AlphaUnchanged &&
                storedEntry.BestMove.Score <= alpha)) then
                ans <- storedEntry.BestMove.Score|>Some
        if ans.IsSome then 
            ans.Value
        else
            let earlyEval = NNUEb.OutputLayer()
            if earlyEval >= beta then ans <- beta|>Some
            if ans.IsSome then ans.Value
            else
                if earlyEval > alpha then alpha <- earlyEval
                let movearr:OrdMoveEntryRec array = Array.zeroCreate OrdMoves.SIZE
                let moveList = OrdMoves.Create(movearr, plyFromRoot)
                let moveCount = OrdMoves.QSearchMoveGeneration(moveList, OrdMove.Default)
                let mutable bestEvaluation = earlyEval
                let nextDepth = depth - 1
                let nextPlyFromRoot = plyFromRoot + 1
                let mutable i = 0
                let mutable keepgoing = true
                while (keepgoing && i < moveCount) do
                    OrdMoves.SortNext(moveList, i, moveCount)
                    let mutable move = OrdMoves.Get(moveList, i)
                    let see = SEE.Approx(move)
                    let seeEval = see + earlyEval
                    if seeEval > beta then 
                        bestEvaluation <- seeEval
                        keepgoing <- false
                    else
                        let mutable rv = EngBoard.Move(move)
                        Srch.NodeCount <- Srch.NodeCount + 1
                        let evaluation = -QSearch(isPvNode, nextPlyFromRoot, nextDepth, -beta, -alpha)
                        EngBoard.UndoMove(rv)
                        if not (HandleEvalQ(evaluation,&bestEvaluation,&alpha,beta)) then 
                            keepgoing <- false
                    i <- i + 1
                bestEvaluation
    let PvLine() = 
        let pv = StringBuilder()
        let count = PrincVars.Count()
        for i = 0 to count-1 do
            let move:OrdMoveEntryRec = PrincVars.Get(i)
            pv.Append(Square.ToStr(move.From)).Append(Square.ToStr(move.To))|>ignore
            if move.Promotion <> PromNone then pv.Append(Promotion.ToStr(move.Promotion))|>ignore
            pv.Append(' ')|>ignore
        pv.ToString().ToLower()
    let NodeCounting(depth:int, bestMove:OrdMoveEntryRec, timePrevUpdated:byref<bool>) = 
        if depth >= 8 && TimeCntrl.TimeLeft() <> 0 && not timePrevUpdated
           && SrchEff.Get(bestMove.From, bestMove.To) * 100 / Srch.NodeCount >= 95 then
            timePrevUpdated <- true
            TimeCntrl.ChangeTime(Tc.Time / 3)
            Srch.RedTimeMove <- bestMove
        if timePrevUpdated && bestMove <> Srch.RedTimeMove then
            TimeCntrl.ChangeTime(Tc.Time * 3)
    let DepthSearchLog(depth:int, evaluation:int, stopwatch:Stopwatch) =
        let elapSec = float(stopwatch.ElapsedMilliseconds) / 1000.0
        let ratio = int(float(Srch.NodeCount) / elapSec)
        Console.Write(
            "info depth " + depth.ToString() + " seldepth " + Srch.SelDepth.ToString() + " score cp " + evaluation.ToString() + " nodes " + 
            Srch.NodeCount.ToString() + " nps " + ratio.ToString() 
            + " pv " + PvLine() + "\n"
        )
    let HandleEval(evaluation:int, move:OrdMoveEntryRec, bestEvaluation:byref<int>, bestMoveSoFar:byref<OrdMoveEntryRec>, isPvNode:bool, plyFromRoot:int, alpha:byref<int>, beta:int, tranType:byref<TranType>) =
        if evaluation <= bestEvaluation then true
        else
            bestEvaluation <- evaluation
            bestMoveSoFar <- move
            if isPvNode then
                PrincVars.Insert(plyFromRoot, bestMoveSoFar)
                let mutable nextPly = plyFromRoot + 1
                while (PrincVars.PlyInitialized(plyFromRoot, nextPly)) do
                    PrincVars.Copy(plyFromRoot, nextPly)
                    nextPly <- nextPly + 1
                PrincVars.UpdateLength(plyFromRoot)
            if evaluation <= alpha then true
            else
                alpha <- evaluation
                tranType <- Exact
                evaluation < beta
    let DoQuiet(plyFromRoot:int, move:OrdMoveEntryRec, depth:int, quietCount, moveList:OrdMovesRec, i:int) =
        let historyBonus = depth * depth
        if KillMv.Get(0, plyFromRoot) <> move then
            KillMv.ReOrder(plyFromRoot)
            KillMv.Set(0, plyFromRoot, move)
        Hist.Set(EngBoard.PieceOnly(move.From), Brd.Stm, move.To, Hist.Get(EngBoard.PieceOnly(move.From), Brd.Stm, move.To) + historyBonus)
        for j = 1 to quietCount - 1 do
            let otherMove = OrdMoves.Get(moveList, i - j)
            Hist.Set(EngBoard.PieceOnly(otherMove.From), Brd.Stm, otherMove.To, Hist.Get(EngBoard.PieceOnly(otherMove.From), Brd.Stm, otherMove.To) - historyBonus)
    let rec AbSearch(isPvNode:bool, plyFromRoot:int, idepth:int, ialpha:int, ibeta:int) =
        let mutable alpha = ialpha
        let mutable beta = ibeta
        let mutable ans = None
        if (TimeCntrl.Finished()) then raise (OperationCanceledException())
        if isPvNode then 
            PrincVars.InitializeLength(plyFromRoot)
            Srch.SelDepth <- Math.Max(Srch.SelDepth, plyFromRoot)
        let rootNode = plyFromRoot = 0
        if idepth <= 0 then ans <- QSearch(isPvNode, plyFromRoot, 15, alpha, beta)|>Some
        else
            if not rootNode then
                if EngBoard.IsRepetition() then ans <- 0|>Some
                else
                    let allPiecesCount = Bits.Count(Brd.Both)
                    if allPiecesCount = 2 then ans <- 0|>Some
                    else
                        let knightLeft = Brd.Pieces[WhiteKnight] <> 0UL || Brd.Pieces[BlackKnight] <> 0UL
                        if (allPiecesCount = 3 && knightLeft) then ans <- 0|>Some
                        else
                            let bishopLeft = Brd.Pieces[WhiteBishop] <> 0UL || Brd.Pieces[BlackBishop] <> 0UL
                            if allPiecesCount = 3 && bishopLeft then ans <- 0|>Some
                            else
                                alpha <- Math.Max(alpha, plyFromRoot - 99999999)
                                beta <- Math.Min(beta, 99999999 - plyFromRoot)
                                if (alpha >= beta) then ans <- alpha|>Some
        if ans.IsSome then 
            ans.Value
        else
            let storedEntry = TranTable.GetEntry(Brd.ZobristHash)
            let valid = storedEntry.Type <> Invalid
            let mutable tranMove = OrdMove.Default
            let mutable tranHit = false
            if valid && storedEntry.Hash = Brd.ZobristHash then
                tranMove <- storedEntry.BestMove
                tranHit <- true
                if not isPvNode && storedEntry.Depth >= idepth then
                    if storedEntry.Type = Exact then
                            ans <- storedEntry.BestMove.Score|>Some
                    elif storedEntry.Type = BetaCutoff then
                            alpha <- Math.Max(alpha, storedEntry.BestMove.Score)
                    elif storedEntry.Type = AlphaUnchanged then
                            beta <- Math.Min(beta, storedEntry.BestMove.Score)
                    if alpha >= beta then
                        ans <- storedEntry.BestMove.Score|>Some
            if ans.IsSome then 
                ans.Value
            else
                let nextPly = plyFromRoot + 1
                let kingSq = if Brd.IsWtm then Brd.WhiteKingLoc else Brd.BlackKingLoc
                let mutable inCheck = MoveList.UnderAttack(kingSq, Brd.Xstm)
                let mutable improving = false
                let eval = if tranHit then tranMove.Score else NNUEb.OutputLayer()
                SrchStack.Set(plyFromRoot, eval)
                if not isPvNode && not inCheck then
                    improving <- plyFromRoot >= 2 && eval >= SrchStack.Get(plyFromRoot - 2)
                    let improvingInt = if improving then 1 else 0
                    if idepth < 7 && Math.Abs(beta) < 99999999 && eval - 67 * idepth + 76 * improvingInt >= beta then
                        ans <- beta|>Some
                    elif not rootNode && idepth > 2 then
                        let evaluation = NullMovePrune(nextPly, idepth, beta)
                        if evaluation >= beta then ans <- beta|>Some
                if ans.IsSome then 
                    ans.Value
                else
                    let cdepth = if inCheck then idepth + 1 else idepth
                    let depth = if (cdepth > 3 && not tranHit) then cdepth - 1 else cdepth
                    let movearr:OrdMoveEntryRec array = Array.zeroCreate OrdMoves.SIZE
                    let moveList = OrdMoves.Create(movearr, plyFromRoot)
                    let moveCount = OrdMoves.NormalMoveGeneration(moveList, tranMove)
                    if moveCount = 0 then
                        if inCheck then -99999999 + plyFromRoot else 0
                    else
                        let mutable bestEvaluation = -100000000
                        let mutable bestMoveSoFar = OrdMove.Default
                        let mutable tranType = AlphaUnchanged
                        let nextDepth = depth - 1
                        let mutable i = 0
                        let mutable quietCount = 0
                        let lmpQuietThreshold = 3 + depth * depth
                        let lmp = not rootNode && not inCheck && depth <= 3
                        let mutable keepgoing = true
                        while (i < moveCount && keepgoing) do
                            OrdMoves.SortNext(moveList, i, moveCount)
                            let previousNodeCount = Srch.NodeCount
                            let mutable move = OrdMoves.Get(moveList, i)
                            let oppBoard = if Brd.IsWtm then Brd.Black else Brd.White
                            let quietMove = not (Bits.IsSet(oppBoard, move.To))
                            if quietMove then quietCount <- quietCount + 1
                            let lmpTest = not isPvNode && lmp && bestEvaluation > -100000000 && quietCount > lmpQuietThreshold
                            if lmpTest then keepgoing <- false
                            else
                                let mutable rv = EngBoard.Move(move)
                                Srch.NodeCount <- Srch.NodeCount + 1
                                let mutable evaluation = 
                                    if i = 0 then
                                        -AbSearch(isPvNode, nextPly, nextDepth, -beta, -alpha)
                                    else 
                                        alpha + 1
                                if i > 0 && evaluation > alpha then
                                    PvSearch(&evaluation, nextPly, nextDepth, alpha, beta)
                                EngBoard.UndoMove(rv)
                                if not (HandleEval(evaluation, move, &bestEvaluation,&bestMoveSoFar,isPvNode,plyFromRoot,&alpha,beta,&tranType)) then
                                    if quietMove then
                                        DoQuiet(plyFromRoot,move,depth,quietCount,moveList,i)
                                    tranType <- BetaCutoff
                                    keepgoing <- false
                                if rootNode then SrchEff.Set(move.From, move.To, Srch.NodeCount - previousNodeCount)
                                i <- i + 1
                        bestMoveSoFar.Score <- bestEvaluation
                        let mutable entry = {Hash=Brd.ZobristHash;Type=tranType;BestMove=bestMoveSoFar;Depth=depth}
                        TranTable.InsertEntry(Brd.ZobristHash, entry)
                        bestEvaluation
    and NullMovePrune(nextPly:int, idepth:int, beta:int) =        
        let reducedDepth = idepth - 4 - (idepth / 3 - 1)
        let mutable rv = EngBoard.NullMove()
        let evaluation = -AbSearch(false, nextPly, reducedDepth, -beta, -beta + 1)
        EngBoard.UndoNullMove(rv)
        evaluation
    and PvSearch(evaluation:byref<int>, nextPly:int, nextDepth:int, alpha:int, beta:int) =
        evaluation <- -AbSearch(false, nextPly, nextDepth, -alpha - 1, -alpha)
        if (evaluation > alpha && evaluation < beta) then
            evaluation <- -AbSearch(true, nextPly, nextDepth, -beta, -alpha)
    let AspirationSearch(depth:int, previousEvaluation:int) =
        let mutable alpha = -100000000
        let mutable beta = 100000000
        if depth > 4 then
            alpha <- previousEvaluation - 16
            beta <- previousEvaluation + 16
        let mutable res = 0
        let mutable bestEvaluation = alpha
        let mutable notfound = true
        while notfound do
            if TimeCntrl.Finished() then raise (OperationCanceledException())
            if (alpha < -3500) then alpha <- -100000000
            if (beta > 3500) then beta <- 100000000
            bestEvaluation <- AbSearch(true, 0, depth, alpha, beta)
            if bestEvaluation <= alpha then
                let newres = res + 1                
                alpha <- Math.Max(alpha - newres * newres * 23, -100000000)
            elif bestEvaluation >= beta then
                let newres = res + 1 
                beta <- Math.Min(beta + newres * newres * 23, 100000000)
            else notfound <- false
        bestEvaluation
    let IterativeDeepening(selectedDepth:int) =
        let mutable bestMove = OrdMove.Default
        try 
            let stopwatch = Stopwatch.StartNew()
            let mutable timePrevUpdated = false
            let rec getbm cureval curdepth =
                if not (TimeCntrl.Finished() || curdepth > selectedDepth) then
                    let eval = AspirationSearch(curdepth, cureval)
                    bestMove <- PrincVars.Get(0)
                    NodeCounting(curdepth, bestMove, &timePrevUpdated)
                    DepthSearchLog(curdepth, eval, stopwatch)
                    if not (curdepth > 5 && float(TimeCntrl.TimeLeft()) <= float(Tc.Time) * 0.2) then
                        getbm eval (curdepth + 1)
            getbm -100000000 1                    
        with
            | :? OperationCanceledException -> ()
        NNUEb.AccIndex<-0
        bestMove
    let DoTest(selectedDepth:int, bm:string) =
        let mutable bestMove = OrdMove.Default
        try 
            let stopwatch = Stopwatch.StartNew()
            let mutable timePrevUpdated = false
            let rec getbm cureval curdepth =
                if not (TimeCntrl.Finished() || curdepth > selectedDepth) then
                    let eval = AspirationSearch(curdepth, cureval)
                    bestMove <- PrincVars.Get(0)
                    NodeCounting(curdepth, bestMove, &timePrevUpdated)
                    DepthSearchLog(curdepth, eval, stopwatch)
                    if not (bm = OrdMove.ToStr(bestMove)) then
                        getbm eval (curdepth + 1)
            getbm -100000000 1                    
        with
            | :? OperationCanceledException -> ()
        NNUEb.AccIndex<-0
        bestMove
            