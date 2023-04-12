namespace FlounderLib
open System

module Zobrist =
    let rnd = new Random(123456)
    let ZobristPieces =
        let ans:uint64 array array = Array.zeroCreate 12
        for i = 0 to 11 do
            ans[i] <- Array.zeroCreate 64
            for j = 0 to 63 do
                ans[i][j] <- uint64(rnd.NextInt64())
        ans
    let ZobristEpKeys =
        let ans:uint64 array = Array.zeroCreate 64
        for i = 0 to 63 do
            ans[i] <- uint64(rnd.NextInt64())
        ans
    let ZobristCastleKeys =
        let ans:uint64 array = Array.zeroCreate 16
        for i = 0 to 15 do
            ans[i] <- uint64(rnd.NextInt64())
        ans
    let ZobristSideKey = uint64(rnd.NextInt64())
    let HashPiece(zobristHash:byref<uint64>, cpc:int, sq:int) = 
        zobristHash <- zobristHash ^^^ ZobristPieces[cpc][sq]
    let HashEp(zobristHash:byref<uint64>, ep:int) = 
        zobristHash <- zobristHash ^^^ ZobristEpKeys[ep]
    let HashCastlingRights(zobristHash:byref<uint64>, wk:int, wq:int, bk:int, bq:int) = 
        zobristHash <- zobristHash ^^^ ZobristCastleKeys[wk ||| wq ||| bk ||| bq]
    let FlipTurnInHash(zobristHash:byref<uint64>) =
        zobristHash <- zobristHash ^^^ ZobristSideKey
    let Hash(map:BoardRec) =
        let mutable zobristHash = 0UL
        for cpc = WhitePawn to BlackKing do
            let psbb = map.Pieces[cpc]
            let sqarr = Bits.ToArray(psbb)
            Array.iter (fun sq -> zobristHash <- zobristHash ^^^ ZobristPieces[cpc][sq]) sqarr
        if not map.IsWtm then zobristHash <- zobristHash ^^^ ZobristSideKey
        if map.EnPassantTarget <> Na then zobristHash <- zobristHash ^^^ ZobristEpKeys[map.EnPassantTarget]
        zobristHash <- zobristHash ^^^ ZobristCastleKeys[map.WhiteKCastle ||| map.WhiteQCastle ||| map.BlackKCastle ||| map.BlackQCastle]
        zobristHash
