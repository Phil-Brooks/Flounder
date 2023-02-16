namespace FlounderLib

type MoveSearchStackItem = 
    struct
        val mutable PositionalEvaluation:int
    end
type MoveSearchStack() =
    let SIZE = 128
    let mutable Internal:MoveSearchStackItem array = Array.zeroCreate SIZE
    member _.Item with get(ply:int):byref<MoveSearchStackItem> = 
        &(Internal.[ply])
    

