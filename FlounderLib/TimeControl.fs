namespace FlounderLib
open System
open System.Threading

module TimeControl =
    let GetCurrentTime() = DateTimeOffset.Now.ToUnixTimeMilliseconds()

type TimeControl =
    struct
        val mutable Source:CancellationTokenSource
        val mutable Token:CancellationToken
        val mutable StartTime:int64
        val mutable Time:int
        new(isource,itoken,istarttime, itime) =
            {
                Source = isource
                Token = itoken
                StartTime = istarttime
                Time = itime
            }
        new(itime:int) =
            let source = new CancellationTokenSource()
            let mutable token = new CancellationToken()
            let startTime = TimeControl.GetCurrentTime()
            source.CancelAfter(itime)
            token <- source.Token
            TimeControl(source,token,startTime,itime)
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
        member this.Finished() = this.Token.IsCancellationRequested
        member this.TimeLeft() = this.Time - this.ElapsedTime()
        member this.ChangeTime(itime:int) =
            if (itime - this.ElapsedTime() <= 0) then
                this.Source.Cancel()
            else
                this.Time <- itime
                this.Source.CancelAfter(this.TimeLeft())
        member this.ElapsedTime() = int(TimeControl.GetCurrentTime() - this.StartTime)
    end


