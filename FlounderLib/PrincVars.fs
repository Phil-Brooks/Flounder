namespace FlounderLib

module PrincVars =
    let Len = Array.create 128 0
    let Arr:OrdMoveEntryRec array = Array.zeroCreate (128 * 128)
    let InitializeLength(ply:int) = Len[ply] <- ply
    let Insert(ply:int, move:OrdMoveEntryRec) = Arr[ply * 128 + ply] <- move
    let Copy(currentPly:int, nextPly:int) =
        Arr[currentPly * 128 + nextPly] <- Arr[(currentPly + 1) * 128 + nextPly]
    let PlyInitialized(currentPly:int, nextPly:int) = nextPly < Len[currentPly + 1]
    let UpdateLength(ply:int) = Len[ply] <- Len[ply + 1]
    let Count() = Len[0]
    let Get(plyIndex:int) = &Arr[plyIndex]
    let Clear() = 
        Len.Initialize()
        Arr.Initialize()
