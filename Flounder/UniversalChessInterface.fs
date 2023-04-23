namespace Flounder
open System
open System.Threading
open System.Threading.Tasks
open System.Diagnostics
open FlounderLib

module Program =
    let RunPerft(depth:int) =
        Console.WriteLine("Running PERFT @ depth " + depth.ToString() + ": ")
        let watch = Stopwatch()
        let mutable result = 0uL
        watch.Start()
        result <- Perft.MoveGeneration(depth)
        watch.Stop()
        let output = "Searched " + result.ToString("N0") + " nodes (" + watch.ElapsedMilliseconds.ToString() + " ms)."
        Console.WriteLine(output)
module UniversalChessInterface =
    let NAME = "Flounder"
    let AUTHOR = "Phil Brooks"
    EngBoard.Default()
    let mutable Busy = false
    TimeCntrl.FromTime(9999999)
    let mutable MvCount = 0
    let HandleIsReady(input:string) =
        if (input.ToLower().Equals("isready")) then
            Console.WriteLine("readyok")
    let HandleQuit(thread:Thread, input:string) =
        if (input.ToLower().Equals("quit")) then
            UciStdInputThread.Running <- false
            thread.IsBackground <- true
            Environment.Exit(0)
    let HandlePosition(input:string) =
        let args = input.Split(" ")
        if (args.[0].ToLower().Equals("position")) && (args.Length > 1) then
            Busy <- true
            let mutable argsParsed = 1
            if (args.[1].ToLower()="startpos") then
                EngBoard.Default()
                argsParsed<-argsParsed+1
            elif (args.[1].ToLower()="fen") then
                let p = args.[2]
                let s = args.[3]
                let c = args.[4]
                let ep = args.[5]
                EngBoard.FromFen(p + " " + s + " " + c + " " + ep)
                argsParsed<-argsParsed+7
            else
                failwith("Invalid Position provided.")
            if (args.Length < argsParsed + 1) then
                Busy <- false
            else
                if (args.[argsParsed].ToLower().Equals("moves")) then
                    let MoveCount = args.Length - (argsParsed + 1)
                    for i = argsParsed + 1 to args.Length-1 do
                        let from, mto = Square.FromUci(args.[i])
                        let mutable promotion = PromNone
                        if (args.[i].Length > 4) then
                            promotion <- Promotion.FromChar(args.[i].ToLower().[4])
                        EngBoard.GuiMove(from, mto, promotion)
                Busy <- false
    let HandleDraw(input:string) =
        if (input.ToLower() = "draw" || input.ToLower() = "d") then
            Console.WriteLine(Brd.ToString())
    let HandleGo(input:string) =
        let args = input.Split(" ")
        if (args.[0].ToLower().Equals("go")) then
            if (input.ToLower().Contains("perft")) then
                Program.RunPerft(int(args.[2]))
            else
                let maxTime = 999_999_999
                let maxDepth = 63
                let mutable time = maxTime
                let mutable depth = maxDepth
                let mutable movesToGo = -1
                if (args.Length = 1) then
                    TimeCntrl.FromTime(time)
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
                    if (time = maxTime || timeSpecified) then TimeCntrl.FromTime(time)
                    else TimeCntrl.FromMoves(movesToGo, timeForColor, timeIncForColor, Brd.Stm, MvCount)
                let factory = TaskFactory()
                let doSearch() =
                    Search.Reset()
                    Busy <- true
                    let bestMove = Search.IterativeDeepening(depth)
                    Busy <- false
                    Console.WriteLine("bestmove " + OrdMove.ToStr(bestMove))
                    MvCount <- MvCount + 1
                factory.StartNew(doSearch, Tc.Token)|>ignore
    let HandleStop(input:string) =
        if (input.ToLower().Equals("stop")) && Busy then
            TimeCntrl.ChangeTime(0)
    let Setup() =
        Busy <- false
        UciStdInputThread.CommandReceived.Add(fun (_ ,input) -> HandleIsReady(input))
        UciStdInputThread.CommandReceived.Add(fun (thread ,input) -> HandleQuit(thread, input))
        UciStdInputThread.CommandReceived.Add(fun (_ ,input) -> HandlePosition(input))
        UciStdInputThread.CommandReceived.Add(fun (_ ,input) -> HandleDraw(input))
        UciStdInputThread.CommandReceived.Add(fun (_ ,input) -> HandleGo(input))
        UciStdInputThread.CommandReceived.Add(fun (_ ,input) -> HandleStop(input))
    let LaunchUci() =
        Console.WriteLine("id name " + NAME)
        Console.WriteLine("id author " + AUTHOR)
        Console.WriteLine("uciok")
        let inputThread = Thread(UciStdInputThread.StartAcceptingInput)
        inputThread.Start()
    

