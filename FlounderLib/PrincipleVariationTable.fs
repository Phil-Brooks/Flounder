namespace FlounderLib
open System

type PrincipleVariationTable() =
    let SIZE = 128
    let Length = Array.zeroCreate SIZE
    let Internal = GC.AllocateUninitializedArray<OrderedMoveEntry>(SIZE * SIZE)

    member _.InitializeLength(ply:int) = Length.[ply] <- ply

    member _.Insert(ply:int, move:byref<OrderedMoveEntry>) = Internal.[ply * SIZE + ply] <- move

    member _.Copy(currentPly:int, nextPly:int) =
        Internal.[currentPly * SIZE + nextPly] <- Internal.[(currentPly + 1) * SIZE + nextPly]

    member _.PlyInitialized(currentPly:int, nextPly:int) = nextPly < Length.[currentPly + 1]

    member _.UpdateLength(ply:int) = Length.[ply] <- Length.[ply + 1]

    member _.Count() = Length.[0]

    member _.Get(plyIndex:int) = &Internal.[plyIndex]
    

