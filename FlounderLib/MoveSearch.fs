namespace FlounderLib
open System
open System.Diagnostics
open System.Text

module MoveSearch =
    let mutable searchers:Searcher array= [||]
    let Init(engbd:EngineBoard,tmcntrl:TimeControl) =
        searchers <- [|Searcher(engbd,tmcntrl)|]
    let PvLine() = 
        let main = searchers.[0]
        let pv = StringBuilder()
        let count = main.PvTable.Count()
        for i = 0 to count-1 do
            let move:OrderedMoveEntry = main.PvTable.Get(i)
            pv.Append(move.From).Append(move.To)|>ignore
            if move.Promotion <> Promotion.None then pv.Append(Promotion.ToStr(move.Promotion))|>ignore
            pv.Append(' ')|>ignore
        pv.ToString().ToLower()
    let DepthSearchLog(depth:int, evaluation:int, stopwatch:Stopwatch) =
        let main = searchers.[0]
        let elapSec = float(stopwatch.ElapsedMilliseconds) / 1000.0
        let ratio = int(float(main.TotalNodeSearchCount) / elapSec)
        Console.Write(
            "info depth " + depth.ToString() + " seldepth " + main.SelectiveDepth.ToString() + " score cp " + evaluation.ToString() + " nodes " + 
            main.TotalNodeSearchCount.ToString() + " nps " + ratio.ToString() 
            + " pv " + PvLine() + "\n"
        )
    let IterativeDeepening(selectedDepth:int) =
        let main = searchers.[0]
        let mutable bestMove = OrderedMoveEntry.Default
        try 
            let stopwatch = Stopwatch.StartNew()
            let rec getbm cureval curdepth =
                if not (main.TimeCntrl.Finished() || curdepth > selectedDepth) then
                    let eval = main.AspirationSearch(main.EngBrd.Value, curdepth, cureval)
                    bestMove <- main.PvTable.Get(0)
                    DepthSearchLog(curdepth, eval, stopwatch)
                    // In the case we are past a certain depth, and are really low on time, it's highly unlikely we'll
                    // finish the next depth in time. To save time, we should just exit the search early.
                    if not (curdepth > 5 && float(main.TimeCntrl.TimeLeft()) <= float(main.TimeCntrl.Time) * 0.2) then
                        getbm eval (curdepth + 1)
            getbm -100000000 1                    
        with
            | :? OperationCanceledException -> ()
        NNUE.ResetAccumulator()
        bestMove
    let DoTest(selectedDepth:int, bm:string) =
        let main = searchers.[0]
        let mutable bestMove = OrderedMoveEntry.Default
        try 
            let stopwatch = Stopwatch.StartNew()
            let rec getbm cureval curdepth =
                if not (main.TimeCntrl.Finished() || curdepth > selectedDepth) then
                    let eval = main.AspirationSearch(main.EngBrd.Value, curdepth, cureval)
                    bestMove <- main.PvTable.Get(0)
                    DepthSearchLog(curdepth, eval, stopwatch)
                    // In the case we are past a certain depth, and are really low on time, it's highly unlikely we'll
                    // finish the next depth in time. To save time, we should just exit the search early.
                    if not (bm = bestMove.ToString()) then
                        getbm eval (curdepth + 1)
            getbm -100000000 1                    
        with
            | :? OperationCanceledException -> ()
        NNUE.ResetAccumulator()
        bestMove
