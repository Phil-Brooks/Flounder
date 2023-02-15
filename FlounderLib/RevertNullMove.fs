namespace FlounderLib
open System.Runtime.CompilerServices

[<IsByRefLike; Struct>]
type RevertNullMove(enPassantTarget:Square) =
    member _.EnPassantTarget:Square = enPassantTarget
    static member FromBitBoardMap(map:byref<BitBoardMap>) =
        RevertNullMove(map.EnPassantTarget)