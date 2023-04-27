namespace FlounderLib

module Perft =
    let D1 = 20uL
    let D2 = 400uL
    let D3 = 8902uL
    let D4 = 197281uL
    let D5 = 4865609uL
    let D6 = 119060324uL
    let D7 = 3195901860uL
    Brd <- Board.Default()
    let rec MoveGeneration(depth) =
        let colored = if Brd.IsWtm then Brd.White else Brd.Black
        let kingSq = if Brd.IsWtm then Brd.WhiteKingLoc else Brd.BlackKingLoc
        let (hv, d) = MoveList.PinBitBoards(kingSq, Brd.Stm, Brd.Xstm)
        let (checks, doubleChecked) = MoveList.CheckBitBoard(kingSq, Brd.Xstm)
        if depth = 1 then
            let mutable tot = 0UL
            let sqs = Bits.ToArray(colored)
            for sq in sqs do
                let moveList = MoveList.Double(sq, hv, d, checks, doubleChecked)
                tot <- tot + if moveList.Promotion then uint64(moveList.Count * 4) else uint64(moveList.Count)
            tot
        else
            let nextDepth = depth - 1
            let mutable tot = 0UL
            let sqs = Bits.ToArray(colored)
            for sq in sqs do
                let moveList = MoveList.Double(sq, hv, d, checks, doubleChecked)
                let mvs = Bits.ToArray(moveList.Moves)
                for mv in mvs do
                    let mutable rv = Board.Move(sq, mv, PromNone)
                    if rv.Promotion then
                        Board.UndoMove(rv)
                        for pr = PromKnight to PromQueen do 
                            rv <- Board.Move(sq, mv, pr)
                            tot <- tot + (MoveGeneration(nextDepth))
                            Board.UndoMove(rv)
                    else 
                        tot <- tot + MoveGeneration(nextDepth)
                        Board.UndoMove(rv)
            tot
    let Depth1() = (D1, MoveGeneration(1))
    let Depth2() = (D2, MoveGeneration(2))
    let Depth3() = (D3, MoveGeneration(3))
    let Depth4() = (D4, MoveGeneration(4))
    let Depth5() = (D5, MoveGeneration(5))
    let Depth6() = (D6, MoveGeneration(6))
    let Depth7() = (D7, MoveGeneration(7))
