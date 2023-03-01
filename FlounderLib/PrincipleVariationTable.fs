namespace FlounderLib

type PrincipleVariationTable =
    struct
        val mutable Length:int array
        val mutable Internal:OrderedMoveEntry array
        new(size) =
            {
                Length = Array.zeroCreate size
                Internal = Array.zeroCreate (size * size)
            }
        member this.InitializeLength(ply:int) = this.Length.[ply] <- ply
        member this.Insert(ply:int, move:byref<OrderedMoveEntry>) = this.Internal.[ply * 128 + ply] <- move
        member this.Copy(currentPly:int, nextPly:int) =
            this.Internal.[currentPly * 128 + nextPly] <- this.Internal.[(currentPly + 1) * 128 + nextPly]
        member this.PlyInitialized(currentPly:int, nextPly:int) = nextPly < this.Length.[currentPly + 1]
        member this.UpdateLength(ply:int) = this.Length.[ply] <- this.Length.[ply + 1]
        member this.Count() = this.Length.[0]
        member this.Get(plyIndex:int) = &(this.Internal.[plyIndex])
        member this.Clear() = 
            this.Length.Initialize()
            this.Internal.Initialize()
    end
module PrincipleVariationTable =
    let  Default = PrincipleVariationTable(128)    

