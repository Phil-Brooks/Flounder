namespace FlounderLib
open System

module Zobrist =
    let ZOBRIST_SEED = 462279021
    let Random = Random(ZOBRIST_SEED)
    let PieceKeys = ZobristPieceKeyTable(Random)
    let CastlingKeys:uint64 array = Array.zeroCreate 16
    let EnPassantKeys :uint64 array = Array.zeroCreate 64
    let mutable TurnKey = 0UL

    let Setup() =
        let bufferSpanarr = Array.zeroCreate<byte>(sizeof<uint64>)
        let mutable buffer = new Span<byte>(bufferSpanarr)
        for i = 0 to CastlingKeys.Length-1 do
            Random.NextBytes(buffer)
            CastlingKeys.[i] <- BitConverter.ToUInt64(buffer)
        for i = 0 to EnPassantKeys.Length-1 do
            Random.NextBytes(buffer)
            EnPassantKeys.[i] <- BitConverter.ToUInt64(buffer)
        Random.NextBytes(buffer)
        TurnKey <- BitConverter.ToUInt64(buffer)
    let HashPiece(zobristHash:byref<uint64>, piece:Piece, color:int, sq:Square) = 
        zobristHash <- zobristHash ^^^ PieceKeys.[piece, color, sq]
    let HashCastlingRights(zobristHash:byref<uint64>, wk:int, wq:int, bk:int, bq:int) = 
        zobristHash <- zobristHash ^^^ CastlingKeys.[wk ||| wq ||| bk ||| bq]
    let FlipTurnInHash(zobristHash:byref<uint64>) =
        zobristHash <- zobristHash ^^^ TurnKey
    let HashEp(zobristHash:byref<uint64>, ep:Square) = 
        zobristHash <- zobristHash ^^^ EnPassantKeys.[int(ep)]
   