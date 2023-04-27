namespace FlounderLib
open System.Threading

[<AutoOpen>]
module Types =
    let VersionNo = "0.4.5.8"

    // The type of piece.
    let WhitePawn = 0
    let BlackPawn = 1
    let WhiteKnight = 2
    let BlackKnight = 3
    let WhiteBishop = 4
    let BlackBishop = 5
    let WhiteRook = 6
    let BlackRook = 7
    let WhiteQueen = 8
    let BlackQueen = 9
    let WhiteKing = 10
    let BlackKing = 11
    let EmptyColPc = 12
    let ColPcChars = "PpNnBbRrQqKk."
    
    // The type of piece.
    let Pawn = 0
    let Knight = 1
    let Bishop = 2
    let Rook = 3
    let Queen = 4
    let King = 5
    let EmptyPc = 6
    let PcChars = "PNBRQK."

    // The color of the piece.
    let White = 0
    let Black = 1
    let Both = 2

    // Squares on a chess board.
    // Na is if it's no square on the board.
    let A1 = 0 
    let B1 = 1 
    let C1 = 2 
    let D1 = 3 
    let E1 = 4 
    let F1 = 5 
    let G1 = 6 
    let H1 = 7
    let A2 = 8 
    let B2 = 9 
    let C2 = 10 
    let D2 = 11 
    let E2 = 12 
    let F2 = 13 
    let G2 = 14 
    let H2 = 15
    let A3 = 16 
    let B3 = 17 
    let C3 = 18 
    let D3 = 19 
    let E3 = 20 
    let F3 = 21 
    let G3 = 22 
    let H3 = 23
    let A4 = 24 
    let B4 = 25 
    let C4 = 26 
    let D4 = 27 
    let E4 = 28 
    let F4 = 29 
    let G4 = 30 
    let H4 = 31
    let A5 = 32 
    let B5 = 33 
    let C5 = 34 
    let D5 = 35 
    let E5 = 36 
    let F5 = 37 
    let G5 = 38 
    let H5 = 39
    let A6 = 40 
    let B6 = 41 
    let C6 = 42 
    let D6 = 43 
    let E6 = 44 
    let F6 = 45 
    let G6 = 46 
    let H6 = 47
    let A7 = 48 
    let B7 = 49 
    let C7 = 50 
    let D7 = 51 
    let E7 = 52 
    let F7 = 53 
    let G7 = 54 
    let H7 = 55
    let A8 = 56 
    let B8 = 57 
    let C8 = 58 
    let D8 = 59 
    let E8 = 60 
    let F8 = 61 
    let G8 = 62 
    let H8 = 63
    let Na = 64

    let PromNone = 0
    let PromKnight = 1
    let PromBishop = 2
    let PromRook = 3
    let PromQueen = 4
    let PromChars = ".nbrq."
    
    type BoardRec =
        {
            mutable IsWtm:bool
            mutable Stm:int
            mutable Xstm:int
            Pieces:uint64 array
            Squares:int array
            mutable WhiteKingLoc:int
            mutable BlackKingLoc:int
            mutable White:uint64
            mutable Black:uint64
            mutable Both:uint64
            mutable WhiteKCastle:int
            mutable WhiteQCastle:int
            mutable BlackKCastle:int
            mutable BlackQCastle:int
            mutable EnPassantTarget:int
            mutable ZobristHash:uint64
        }

    type MoveRec =
        {
            WhiteKCastle:int
            WhiteQCastle:int
            BlackKCastle:int
            BlackQCastle:int
            EnPassantTarget:int
            mutable Promotion:bool
            mutable EnPassant:bool
            mutable From:int
            mutable To:int
            mutable CapturedPiece:int
            mutable SecondaryFrom:int
            mutable SecondaryTo:int
        }

    type NNUEinRec =
        {
            InputWeights:int16 array
            InputBiases:int16 array
            OutputWeights:int16 array
            OutputBias:int
        }

    type AccKingStateRec =
        {
            AccKsValues:int16 array
            Pcs:uint64 array
        }

    let KING_BUCKETS = 
        [|
            15; 15; 14; 14; 14; 14; 15; 15; 
            15; 15; 14; 14; 14; 14; 15; 15; 
            13; 13; 12; 12; 12; 12; 13; 13; 
            13; 13; 12; 12; 12; 12; 13; 13; 
            11; 10; 9;  8;  8;  9;  10; 11; 
            11; 10; 9;  8;  8;  9;  10; 11; 
            7;  6;  5;  4;  4;  5;  6;  7;  
            3;  2;  1;  0;  0;  1;  2;  3 
        |]

    type DeltaRec =
        {   
            mutable r:int
            mutable a:int
            rem:int array
            add:int array
        }

    type OrdMoveEntryRec =
        {
            From: int
            To: int
            Promotion: int
            mutable Score: int
        }

    type TranType =
        | Exact
        | BetaCutoff
        | AlphaUnchanged
        | Invalid

    type TranEntryRec =
        {
            Hash: uint64
            Type: TranType
            BestMove: OrdMoveEntryRec
            Depth: int
        }

    type TranTableRec =
        {
            mutable HashFilter:int
            mutable Internal:TranEntryRec array
        }

    type MoveListRec =
        {
            From:int
            Hv:uint64
            D:uint64
            C:uint64
            Moves:uint64
            Count:int
            Promotion:bool
        }

    type OrdMovesRec =
        {
            mutable Internal:OrdMoveEntryRec array
            mutable KillerMoveOne:OrdMoveEntryRec
            mutable KillerMoveTwo:OrdMoveEntryRec
        }

    type TimeControl =
        {
            mutable Source:CancellationTokenSource
            mutable Token:CancellationToken
            mutable StartTime:int64
            mutable Time:int
        }

    type SearchRec =
        {
            mutable NodeCount:int
            mutable SelDepth:int 
            mutable RedTimeMove:OrdMoveEntryRec
        }

module Piece =
    let ToStr(pc:int) = PcChars[pc].ToString()

module ColPiece =
    let ToPcCol(colpc:int) =
        let pc = colpc/2
        let col = if pc=EmptyPc then 2 else colpc%2
        pc,col
    let FromPcCol(piece:int,color:int) =
        if color = 2||piece=EmptyPc then EmptyColPc
        else piece*2 + color
    let ToStr(pc:int) = ColPcChars[pc].ToString()

module Square =
    let FromStr(sq:string) =
        let f = int(sq[0] - 'a')
        let r = int(sq[1] - '1')
        r * 8 + f
    let FromUci(uci:string) =
        FromStr(uci[..1]), FromStr(uci[2..3])
    let ToFile(sq:int) = sq%8
    let ToRank(sq:int) = sq/8
    let ToStr(sq:int) =
        let r = ToRank(sq)
        let f = ToFile(sq)
        let num = (r + 1).ToString()
        let ltr = ("abcdefgh"[f]).ToString()
        ltr + num

module Promotion =
    let ToStr(prm:int) = PromChars[prm].ToString()
    let FromChar(ch:char) =
        if ch = 'n' then PromKnight
        elif ch = 'b' then PromBishop
        elif ch = 'r' then PromRook
        elif ch = 'q' then PromQueen
        else PromNone

module Delta =
    let Default() =
        {   
            r = 0
            a = 0
            rem = Array.zeroCreate 32
            add = Array.zeroCreate 32
        }

