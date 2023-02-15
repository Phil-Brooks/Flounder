namespace FlounderLib
open System
open System.Threading

type TimeControl(itime:int) =
    
    static let GetCurrentTime() = DateTimeOffset.Now.ToUnixTimeMilliseconds()
    let Source = new CancellationTokenSource()
    let mutable token:CancellationToken = new CancellationToken()
    let mutable StartTime = 0L
    let mutable time = 0
    
    do
        time <- itime
        Source.CancelAfter(time)
        token <- Source.Token
        StartTime <- GetCurrentTime()
    new(movesToGo:int, timeForColor:ReadOnlySpan<int>, timeIncForColor:ReadOnlySpan<int>, colorToMove:PieceColor, moveCount:int) =
        let BASE_DIV = 20
        let INCREMENT_MOVE_BOUND = 10
        let INCREMENT_DIV = 2
        let DELTA_MOVE_BOUND = 20
        let DELTA_THRESHOLD = 3000
        let DELTA_DIV = 3
        let mutable time = timeForColor[int(colorToMove)] / BASE_DIV
        if (movesToGo <> -1 && movesToGo < BASE_DIV) then
            time <- Math.Max(time, timeForColor[int(colorToMove)] / movesToGo - 100)
        if (moveCount >= INCREMENT_MOVE_BOUND) then time <- time + timeIncForColor[int(colorToMove)] / INCREMENT_DIV
        if (moveCount >= DELTA_MOVE_BOUND) then
            let dTime = timeForColor[int(colorToMove)] - timeForColor[int(PieceColor.OppositeColor(colorToMove))]
            if (dTime >= DELTA_THRESHOLD) then time <- time + dTime / DELTA_DIV
        TimeControl(time)

    member _.Time = time
    member _.Token = token
    member _.Finished() = token.IsCancellationRequested
    member this.TimeLeft() = time - this.ElapsedTime()
    member this.ChangeTime(itime:int) =
        if (itime - this.ElapsedTime() <= 0) then
            Source.Cancel()
        else
            time <- itime
            Source.CancelAfter(this.TimeLeft())
    member _.ElapsedTime() = int(GetCurrentTime() - StartTime)


