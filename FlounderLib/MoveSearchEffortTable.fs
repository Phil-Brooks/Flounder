namespace FlounderLib

type MoveSearchEffortTable =
    struct
        val mutable Internal:int array
        new(size) =
            {
                Internal = Array.zeroCreate size
            }
        member this.Item 
            with get(from:int, mto:int) = this.Internal.[from * 64 + mto]
            and set(from:int, mto:int) value = this.Internal.[from * 64 + mto] <- value
        member this.Clear() = this.Internal.Initialize()
    end 
module MoveSearchEffortTable =
    let Default = MoveSearchEffortTable(4096)

