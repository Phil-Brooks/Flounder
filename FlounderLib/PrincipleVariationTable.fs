namespace FlounderLib
open System

type PrincipleVariationTable() =
    let SIZE = 128
    let Length = Array.zeroCreate SIZE
    let Internal = GC.AllocateUninitializedArray<OrderedMoveEntry>(SIZE * SIZE)

    member _.InitializeLength(ply:int) = Length.AA(ply) <- ply

    member _.Insert(ply:int, move:byref<OrderedMoveEntry>) = Internal.AA(ply * SIZE + ply) <- move

    member _.Copy(currentPly:int, nextPly:int) =
        Internal.AA(currentPly * SIZE + nextPly) <- Internal.AA((currentPly + 1) * SIZE + nextPly)

    member _.PlyInitialized(currentPly:int, nextPly:int) = nextPly < Length.AA(currentPly + 1)

    member _.UpdateLength(ply:int) = Length.AA(ply) <- Length.AA(ply + 1)

    member _.Count() = Length.AA(0)

    member _.Get(plyIndex:int) = &Internal.AA(plyIndex)
    

