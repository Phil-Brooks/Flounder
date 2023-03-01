namespace FlounderLib

type MoveSearchEffortTable =
    struct
        val mutable Internal:int array
        new(size) =
            {
                Internal = Array.zeroCreate size
            }
        member this.Item 
            with get(from:Square, mto:Square) = this.Internal.[int(from) * 64 + int(mto)]
            and set(from:Square, mto:Square) value = this.Internal.[int(from) * 64 + int(mto)] <- value
    end 
module MoveSearchEffortTable =
    let Default() = MoveSearchEffortTable(4096)

