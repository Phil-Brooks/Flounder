namespace FlounderLib
open System

module OrdMoves =
    let Create(memory:OrdMoveEntryRec array, ply:int) =
        {
            Internal = memory
            KillerMoveOne =  KillMv.Get(0, ply)
            KillerMoveTwo =  KillMv.Get(1, ply)
        }
    let SIZE = 128
    let MvvLvaTable =
        [|
            [| 2005; 2004; 2003; 2002; 2001; 2000 |];
            [| 3005; 3004; 3003; 3002; 3001; 3000 |];
            [| 4005; 4004; 4003; 4002; 4001; 4000 |];
            [| 5005; 5004; 5003; 5002; 5001; 5000 |];
            [| 6005; 6004; 6003; 6002; 6001; 6000 |];
            [| 7005; 7004; 7003; 7002; 7001; 7000 |]
        |]
    let MvvLva(attacker:int, victim:int) = MvvLvaTable.[victim].[attacker]
    let ScoreMove(oms:OrdMovesRec, pieceToMove:int, move:OrdMoveEntryRec, tableMove:OrdMoveEntryRec) =
        let PRIORITY = Int32.MaxValue
        if (move.From = tableMove.From && move.To = tableMove.To && move.Promotion = tableMove.Promotion) then PRIORITY - 1
        elif move.Promotion <> PromNone then PRIORITY - 8 + move.Promotion
        else
            let pto = Brd.Squares[move.To]/2
            if pto <> EmptyPc then 
                let pfrom = Brd.Squares[move.From]/2
                MvvLva(pfrom, pto) * 10000
            elif move.From = oms.KillerMoveOne.From && move.To = oms.KillerMoveOne.To && move.Promotion = oms.KillerMoveOne.Promotion then 900000
            elif move.From = oms.KillerMoveTwo.From && move.To = oms.KillerMoveTwo.To && move.Promotion = oms.KillerMoveTwo.Promotion then 800000
            else Hist.Get(pieceToMove, Brd.Stm, move.To)
    let NormalMoveGeneration(oms:OrdMovesRec, transpositionMove:OrdMoveEntryRec) =
        let kingSq = if Brd.IsWtm then Brd.WhiteKingLoc else Brd.BlackKingLoc
        let (hv, d) = MoveList.PinBitBoards(kingSq, Brd.Stm, Brd.Xstm)
        let checks, doubleChecked = MoveList.CheckBitBoard(kingSq, Brd.Xstm)
        let mutable i = 0
        if not doubleChecked then
            let fromarr = Bits.ToArray(Brd.Pieces[Brd.Stm])
            for from in fromarr do
                let moveList = MoveList.NotDouble(from, Pawn, hv, d, checks)
                let movearr = Bits.ToArray(moveList.Moves)
                for move in movearr do
                    if (moveList.Promotion) then
                        for p = PromKnight to PromQueen do
                            oms.Internal[i] <- OrdMove.Create(from, move, p)
                            oms.Internal[i].Score <- ScoreMove(oms, Pawn, oms.Internal[i], transpositionMove)
                            i <- i + 1
                    else
                        oms.Internal[i] <- OrdMove.Create(from, move, PromNone)
                        oms.Internal[i].Score <- ScoreMove(oms, Pawn, oms.Internal[i], transpositionMove)
                        i <- i + 1
            for piece in [|Rook;Knight;Bishop;Queen|] do
                let fromarr = Bits.ToArray(Brd.Pieces[piece*2 + Brd.Stm])
                for from in fromarr do
                    let moveList = MoveList.NotDouble(from, piece, hv, d, checks)
                    let movearr = Bits.ToArray(moveList.Moves)
                    for move in movearr do
                        oms.Internal[i] <- OrdMove.Create(from, move, PromNone)
                        oms.Internal[i].Score <- ScoreMove(oms, piece, oms.Internal[i], transpositionMove)
                        i <- i + 1
        let fromarr = Bits.ToArray(Brd.Pieces[King*2 + Brd.Stm])
        for from in fromarr do
            let moveList = MoveList.NotDouble(from, King, hv, d, checks)
            let movearr = Bits.ToArray(moveList.Moves)
            for move in movearr do
                oms.Internal[i] <- OrdMove.Create(from, move, PromNone)
                oms.Internal[i].Score <- ScoreMove(oms, King, oms.Internal[i], transpositionMove)
                i <- i + 1
        i
    let QSearchMoveGeneration(oms:OrdMovesRec, transpositionMove:OrdMoveEntryRec) =
        let opposite = if Brd.IsWtm then Brd.Black else Brd.White
        let kingSq = if Brd.IsWtm then Brd.WhiteKingLoc else Brd.BlackKingLoc
        let (hv, d) = MoveList.PinBitBoards(kingSq, Brd.Stm, Brd.Xstm)
        let (checks, doubleChecked) = MoveList.CheckBitBoard(kingSq, Brd.Xstm)
        let mutable i = 0
        if not doubleChecked then
            let fromarr = Bits.ToArray(Brd.Pieces[Brd.Stm])
            for from in fromarr do
                let moveList = MoveList.ForPawns(from, hv, d, checks)
                let movearr = Bits.ToArray(moveList.Moves)
                for move in movearr do
                    if moveList.Promotion then
                        for p = PromKnight to PromQueen do
                            oms.Internal[i] <- OrdMove.Create(from, move, p)
                            oms.Internal[i].Score <- ScoreMove(oms, Pawn, oms.Internal[i], transpositionMove)
                            i <- i + 1
                    else 
                        oms.Internal[i] <- OrdMove.Create(from, move, PromNone)
                        oms.Internal[i].Score <- ScoreMove(oms, Pawn, oms.Internal[i], transpositionMove)
                        i <- i + 1
            for piece in [|Rook;Knight;Bishop;Queen|] do
                let fromarr = Bits.ToArray(Brd.Pieces[piece*2 + Brd.Stm])
                for from in fromarr do
                    let moveList = MoveList.NotDouble(from, piece, hv, d, checks)
                    let movearr = Bits.ToArray((moveList.Moves &&& opposite))
                    for move in movearr do
                        oms.Internal[i] <- OrdMove.Create(from, move, PromNone)
                        oms.Internal[i].Score <- ScoreMove(oms, piece, oms.Internal[i], transpositionMove)
                        i <- i + 1
        let fromarr = Bits.ToArray(Brd.Pieces[King*2 + Brd.Stm])
        for from in fromarr do
            let moveList = MoveList.NotDouble(from, King, hv, d, checks)
            let movearr = Bits.ToArray((moveList.Moves &&& opposite))
            for move in movearr do
                oms.Internal[i] <- OrdMove.Create(from, move, PromNone)
                oms.Internal[i].Score <- ScoreMove(oms, King, oms.Internal[i], transpositionMove)
                i <- i + 1                    
        i
    let Get(oms:OrdMovesRec, i:int) = oms.Internal[i]
    let Swap(oms:OrdMovesRec, firstIndex:int, secondIndex:int) = 
        let nf = oms.Internal[secondIndex]
        let ns = oms.Internal[firstIndex]
        oms.Internal[firstIndex] <- nf
        oms.Internal[secondIndex] <- ns
    let SortNext(oms:OrdMovesRec, sorted:int, maxSelection:int) =
        let mutable index = sorted
        for i = 1 + sorted to maxSelection - 1 do
            if oms.Internal[i].Score > oms.Internal[index].Score then index <- i
        Swap(oms, index, sorted)
