namespace FlounderLib
open System

type ZobristPieceKeyTable(random:Random) =
    let mutable Internal:uint64 array = Array.zeroCreate 768
    do
        let bufferSpanarr = Array.zeroCreate<byte>(sizeof<uint64>)
        let mutable buffer = new Span<byte>(bufferSpanarr)
        let mutable i = 0
        while (i < Internal.Length) do
            random.NextBytes(buffer)
            Internal.[i] <- BitConverter.ToUInt64(buffer)
            i <- i + 1

    member _.Item 
        with get(piece:Piece, color:PieceColor, sq:Square) = 
            Internal.AA(int(color) * 384 + int(piece) * 64 + int(sq))
 