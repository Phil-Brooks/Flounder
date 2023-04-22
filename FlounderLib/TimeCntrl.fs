namespace FlounderLib
open System
open System.Threading

module TimeCntrl =
    let GetCurrentTime() = DateTimeOffset.Now.ToUnixTimeMilliseconds()
    let FromFields(isource,itoken,istarttime, itime) =
        Tc <-
            {
                Source = isource
                Token = itoken
                StartTime = istarttime
                Time = itime
            }
    let FromTime(itime:int) =
        let source = new CancellationTokenSource()
        let mutable token = new CancellationToken()
        let startTime = GetCurrentTime()
        source.CancelAfter(itime)
        token <- source.Token
        FromFields(source,token,startTime,itime)
    let FromMoves(movesToGo:int, timeForColor:int array, timeIncForColor:int array, colorToMove:int, moveCount:int) =
        let BASE_DIV = 20
        let INCREMENT_MOVE_BOUND = 10
        let INCREMENT_DIV = 2
        let DELTA_MOVE_BOUND = 20
        let DELTA_THRESHOLD = 3000
        let DELTA_DIV = 3
        let mutable time = timeForColor[colorToMove] / BASE_DIV
        if (movesToGo <> -1 && movesToGo < BASE_DIV) then
            time <- Math.Max(time, timeForColor[colorToMove] / movesToGo - 100)
        if (moveCount >= INCREMENT_MOVE_BOUND) then time <- time + timeIncForColor[colorToMove] / INCREMENT_DIV
        if (moveCount >= DELTA_MOVE_BOUND) then
            let dTime = timeForColor[colorToMove] - timeForColor[colorToMove^^^1]
            if (dTime >= DELTA_THRESHOLD) then time <- time + dTime / DELTA_DIV
        FromTime(time)
    let Finished() = Tc.Token.IsCancellationRequested
    let ElapsedTime() = int(GetCurrentTime() - Tc.StartTime)
    let TimeLeft() = Tc.Time - ElapsedTime()
    let ChangeTime(itime:int) =
        if (itime - ElapsedTime() <= 0) then
            Tc.Source.Cancel()
        else
            Tc.Time <- itime
            Tc.Source.CancelAfter(TimeLeft())


