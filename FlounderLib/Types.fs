namespace FlounderLib
open System.Threading

[<AutoOpen>]
module Types =
    let VersionNo = "0.5.0.2"

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
    let A8 = 0 
    let B8 = 1 
    let C8 = 2 
    let D8 = 3 
    let E8 = 4 
    let F8 = 5 
    let G8 = 6 
    let H8 = 7
    let A7 = 8 
    let B7 = 9 
    let C7 = 10 
    let D7 = 11 
    let E7 = 12 
    let F7 = 13 
    let G7 = 14 
    let H7 = 15
    let A6 = 16 
    let B6 = 17 
    let C6 = 18 
    let D6 = 19 
    let E6 = 20 
    let F6 = 21 
    let G6 = 22 
    let H6 = 23
    let A5 = 24 
    let B5 = 25 
    let C5 = 26 
    let D5 = 27 
    let E5 = 28 
    let F5 = 29 
    let G5 = 30 
    let H5 = 31
    let A4 = 32 
    let B4 = 33 
    let C4 = 34 
    let D4 = 35 
    let E4 = 36 
    let F4 = 37 
    let G4 = 38 
    let H4 = 39
    let A3 = 40 
    let B3 = 41 
    let C3 = 42 
    let D3 = 43 
    let E3 = 44 
    let F3 = 45 
    let G3 = 46 
    let H3 = 47
    let A2 = 48 
    let B2 = 49 
    let C2 = 50 
    let D2 = 51 
    let E2 = 52 
    let F2 = 53 
    let G2 = 54 
    let H2 = 55
    let A1 = 56 
    let B1 = 57 
    let C1 = 58 
    let D1 = 59 
    let E1 = 60 
    let F1 = 61 
    let G1 = 62 
    let H1 = 63
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
        let r = 8 - int(sq[1] - '0')
        r * 8 + f
    let FromUci(uci:string) =
        FromStr(uci[..1]), FromStr(uci[2..3])
    let ToFile(sq:int) = sq%8
    let ToRank(sq:int) = sq/8
    let ToStr(sq:int) =
        let r = ToRank(sq)
        let f = ToFile(sq)
        let num = (8 - r).ToString()
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

