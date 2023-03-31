namespace FlounderLib
open System
open Microsoft.FSharp.NativeInterop

type ZobristPieceKeyTable(random:Random) =
    let mutable Internal:uint64 array = Array.zeroCreate 768
    do
        //let sz = sizeof<uint64>
        //let mem = NativePtr.stackalloc<byte>(sz)
        //let mem2 = mem |> NativePtr.toVoidPtr
        //let mutable buffer = Span<byte>(mem2, sz)
        let bufferSpanarr = Array.zeroCreate<byte>(sizeof<uint64>)
        let mutable buffer = new Span<byte>(bufferSpanarr)
        let mutable i = 0
        while (i < Internal.Length) do
            random.NextBytes(buffer)
            Internal.[i] <- BitConverter.ToUInt64(buffer)
            i <- i + 1

    member _.Item 
        with get(piece:Piece, color:int, sq:Square) = 
            Internal.[color * 384 + int(piece) * 64 + int(sq)]
 