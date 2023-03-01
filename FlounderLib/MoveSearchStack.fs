namespace FlounderLib

type MoveSearchStackItem = 
    struct
        val mutable PositionalEvaluation:int
    end
type MoveSearchStack =
    struct
        val mutable Internal:MoveSearchStackItem array
        new(size) =
            {
                Internal = Array.zeroCreate size
            }
        member this.Item with get(ply:int):byref<MoveSearchStackItem> = 
            &(this.Internal.[ply])
    end
module MoveSearchStack =
    let Default = MoveSearchStack(128)