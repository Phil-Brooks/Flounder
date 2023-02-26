namespace Flounder
open System
open System.Threading
open System.Threading.Tasks
open System.Diagnostics
open FlounderLib

module Program =
    let RunPerft(board:Board, depth:int) =
        Console.WriteLine("Running PERFT @ depth " + depth.ToString() + ": ")
        let watch = Stopwatch()
        let mutable result = 0uL
        watch.Start()
        result <- Perft.MoveGeneration(board, depth)
        watch.Stop()
        let output = "Searched " + result.ToString("N0") + " nodes (" + watch.ElapsedMilliseconds.ToString() + " ms)."
        Console.WriteLine(output)
module UniversalChessInterface =
    let NAME = "Flounder"
    let AUTHOR = "Phil Brooks"
    let mutable MvTrnsTbl:MoveTranDict option = None
    let mutable MvTrnsTblMb = 16
    let mutable EngBrd:EngineBoard = EngineBoard.Default()
    let mutable Busy = false
    let mutable TmCntrl = FlounderLib.TimeControl(9999999)
    let mutable MvCount = 0
    let HandleSetOption(input:string) =
        if (input.ToLower().Contains("setoption")) then
            let args = input.Split(" ")
            if (args.[2] = "Hash") then
                MvTrnsTblMb <- int(args.[4])
                Busy <- true
                MvTrnsTbl.Value.FreeMemory()
                MvTrnsTbl <- None
                MvTrnsTbl <- Some(MoveTranDict.GenerateTable(MvTrnsTblMb))
                Busy <- false
    let HandleIsReady(input:string) =
        if (input.ToLower().Equals("isready")) then
            Console.WriteLine("readyok")
    let HandleQuit(thread:Thread, input:string) =
        if (input.ToLower().Equals("quit")) then
            MvTrnsTbl.Value.FreeMemory()
            MvTrnsTbl <- None
            UciStdInputThread.Running <- false
            thread.IsBackground <- true
            Environment.Exit(0)
    let HandlePosition(input:string) =
        let args = input.Split(" ")
        if (args.[0].ToLower().Equals("position")) && (args.Length > 1) then
            Busy <- true
            let mutable argsParsed = 1
            if (args.[1].ToLower()="startpos") then
                EngBrd <- EngineBoard.Default()
                argsParsed<-argsParsed+1
            elif (args.[1].ToLower()="fen") then
                let p = args.[2]
                let s = args.[3]
                let c = args.[4]
                let ep = args.[5]
                EngBrd <- EngineBoard.FromFen(p + " " + s + " " + c + " " + ep)
                argsParsed<-argsParsed+7
            else
                failwith("Invalid Position provided.")
            if (args.Length < argsParsed + 1) then
                Busy <- false
            else
                // Once we've loaded the position, we can apply moves.
                if (args.[argsParsed].ToLower().Equals("moves")) then
                    let MoveCount = args.Length - (argsParsed + 1)
                    for i = argsParsed + 1 to args.Length-1 do
                        let from = Enum.Parse<Square>(args.[i].[..1], true)
                        let mto = Enum.Parse<Square>(args.[i].[2..3], true)
                        let mutable promotion = Promotion.None
                        if (args.[i].Length > 4) then
                            promotion <- Promotion.FromChar(args.[i].ToLower().[4])
                        EngBrd.GuiMove(from, mto, promotion)
                Busy <- false
    let HandleDraw(input:string) =
        if (input.ToLower() = "draw" || input.ToLower() = "d") then
            Console.WriteLine(EngBrd.ToString())
    let HandleGo(input:string) =
        let args = input.Split(" ")
        if (args.[0].ToLower().Equals("go")) then
            if (input.ToLower().Contains("perft")) then
                // Just run PERFT.
                Program.RunPerft(EngBrd.Brd, int(args.[2]))
            else
                let maxTime = 999_999_999
                let maxDepth = 63
                let mutable time = maxTime
                let mutable depth = maxDepth
                let mutable movesToGo = -1
                if (args.Length = 1) then
                    TmCntrl <- new FlounderLib.TimeControl(time)
                else
                    let mutable timeSpecified = false
                    let timeForColor = Array.zeroCreate<int>(2)
                    let timeIncForColor = Array.zeroCreate<int>(2)
                    let rec getargs argPosition =
                        if (argPosition < args.Length) then
                            let str = args.[argPosition].ToLower()
                            if str = "wtime" then
                                timeForColor.[0] <- int(args.[argPosition+1])
                                time <- 0
                                getargs (argPosition+2)
                            elif str = "btime" then
                                timeForColor.[1] <- int(args.[argPosition+1])
                                time <- 0
                                getargs (argPosition+2)
                            elif str = "winc" then
                                timeIncForColor.[0] <- int(args.[argPosition+1])
                                time <- 0
                                getargs (argPosition+2)
                            elif str = "binc" then
                                timeIncForColor.[1] <- int(args.[argPosition+1])
                                time <- 0
                                getargs (argPosition+2)
                            elif str = "movestogo" then
                                movesToGo <- int(args.[argPosition+1])
                                time <- 0
                                getargs (argPosition+2)
                            elif str = "depth" then
                                depth <- Math.Min(maxDepth, int(args.[argPosition+1]))
                                getargs (argPosition+2)
                            elif str = "movetime" then
                                time <- int(args.[argPosition+1])
                                timeSpecified <- true
                                getargs (argPosition+2)
                            else
                                getargs (argPosition+1)
                    getargs 1
                    if (time = maxTime || timeSpecified) then TmCntrl <- new FlounderLib.TimeControl(time)
                    else TmCntrl <- FlounderLib.TimeControl(movesToGo, timeForColor, timeIncForColor, EngBrd.Brd.ColorToMove, MvCount)
                let factory = TaskFactory()
                let doSearch() =
                    let search = FlounderLib.MoveSearch(EngBrd.Clone(), MvTrnsTbl.Value, TmCntrl)
                    Busy <- true
                    let bestMove = search.IterativeDeepening(depth)
                    Busy <- false
                    let from = bestMove.From.ToString().ToLower()
                    let mto = bestMove.To.ToString().ToLower()
                    let promotion = if bestMove.Promotion <> Promotion.None then Promotion.ToStr(bestMove.Promotion) else ""
                    Console.WriteLine("bestmove " + from + mto + promotion)
        #if DEBUG
                    Console.WriteLine("TT Count: " + search.TableCutoffCount.ToString())
        #endif
                    MvCount <- MvCount + 1
                factory.StartNew(doSearch, TmCntrl.Token)|>ignore
    let HandleStop(input:string) =
        if (input.ToLower().Equals("stop")) && Busy then
            TmCntrl.ChangeTime(0)
    let Setup() =
        Busy <- false
        UciStdInputThread.CommandReceived.Add(fun (_ ,input) -> HandleSetOption(input))
        UciStdInputThread.CommandReceived.Add(fun (_ ,input) -> HandleIsReady(input))
        UciStdInputThread.CommandReceived.Add(fun (thread ,input) -> HandleQuit(thread, input))
        UciStdInputThread.CommandReceived.Add(fun (_ ,input) -> HandlePosition(input))
        UciStdInputThread.CommandReceived.Add(fun (_ ,input) -> HandleDraw(input))
        UciStdInputThread.CommandReceived.Add(fun (_ ,input) -> HandleGo(input))
        UciStdInputThread.CommandReceived.Add(fun (_ ,input) -> HandleStop(input))
    let LaunchUci() =
        // Initialize default UCI parameters.
        MvTrnsTbl <- Some(MoveTranDict.GenerateTable(MvTrnsTblMb))
        // Provide identification information.
        Console.WriteLine("id name " + NAME)
        Console.WriteLine("id author " + AUTHOR)
        Console.WriteLine("option name Hash type spin default 16 min 4 max 512")
        // Let GUI know engine is ready in UCI mode.
        Console.WriteLine("uciok")
        // Start an input thread.
        let inputThread = Thread(UciStdInputThread.StartAcceptingInput)
        inputThread.Start()
    

