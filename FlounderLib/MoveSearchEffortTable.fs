﻿namespace FlounderLib
open System

type MoveSearchEffortTable() =
    let mutable Internal = GC.AllocateUninitializedArray<int>(4096)
    member _.Item 
        with get(from:Square, mto:Square) = Internal.AA(int(from) * 64 + int(mto))
        and set(from:Square, mto:Square) value = Internal.AA(int(from) * 64 + int(mto)) <- value
    
