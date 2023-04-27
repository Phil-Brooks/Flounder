namespace FlounderTest
open NUnit.Framework
open FsUnit
open FlounderLib

module MoveList =
    let mutable board = Board.Default()
    let epfen = "rnbqkbnr/pp2pppp/8/2ppP3/8/8/PPPP1PPP/RNBQKBNR w KQkq d6 0 3"
    let mutable epboard = Board.FromFen(epfen)
    let prmfen = "rnb1kb1r/ppP1pppp/5n2/2p5/8/8/PPPP1PPP/RNBQKBNR w KQkq - 0 5"
    let mutable prmboard = Board.FromFen(prmfen)

    [<SetUp>]
    let Setup () =
        ()

    [<Test>]
    let CountKnightMovesAtB1() =
        EngBoard.Default()
        let moveList = MoveList.ForSq(B1)
        moveList.Count |> should equal 2

    [<Test>]
    let CountPawnMovesAtA2() =
        EngBoard.Default()
        let moveList = MoveList.ForSq(A2)
        moveList.Count |> should equal 2
        let movearr = Bits.ToArray(moveList.Moves)
        movearr[0]|> should equal A3
        movearr[1]|> should equal A4

    [<Test>]
    let CountKnightMovesAtA3() =
        let mutable usebd = Board.Default()
        Brd <- usebd
        let res = Board.Move(B1, A3, PromNone)
        let res = Board.Move(B8, A6, PromNone)
        let moveList = MoveList.ForSq(A3)
        moveList.Count |> should equal 3
        let movearr = Bits.ToArray(moveList.Moves)
        movearr[0]|> should equal B1
        movearr[1]|> should equal C4

    [<Test>]
    let CountRookMovesAtA3() =
        let mutable usebd = Board.Default()
        Brd <- usebd
        let res = Board.Move(A1, A3, PromNone)
        let res = Board.Move(B8, C6, PromNone)
        let moveList = MoveList.ForSq(A3)
        moveList.Count |> should equal 11
        let movearr = Bits.ToArray(moveList.Moves)
        movearr[0]|> should equal B3
        movearr[1]|> should equal C3

    [<Test>]
    let CountRookMovesAtA1() =
        EngBoard.Default()
        let moveList = MoveList.ForSq(A1)
        moveList.Count |> should equal 0

    [<Test>]
    let CountBishopMovesAtC3() =
        let mutable usebd = Board.Default()
        Brd <- usebd
        let res = Board.Move(C1, C3, PromNone)
        let res = Board.Move(B8, C6, PromNone)
        let moveList = MoveList.ForSq(C3)
        moveList.Count |> should equal 6
        let movearr = Bits.ToArray(moveList.Moves)
        movearr[0]|> should equal B4
        movearr[1]|> should equal D4

    [<Test>]
    let CountQueenMovesAtC3() =
        EngBoard.Default()
        let res = Board.Move(D1, C3, PromNone)
        let res = Board.Move(B8, A6, PromNone)
        let moveList = MoveList.ForSq(C3)
        moveList.Count |> should equal 17
        let movearr = Bits.ToArray(moveList.Moves)
        movearr[0]|> should equal A3
        movearr[1]|> should equal B3

    [<Test>]
    let CountKingMovesAtC3() =
        EngBoard.Default()
        let res = Board.Move(E1, C3, PromNone)
        let res = Board.Move(A7, A6, PromNone)
        let moveList = MoveList.ForSq(C3)
        moveList.Count |> should equal 5
        let movearr = Bits.ToArray(moveList.Moves)
        movearr[0]|> should equal B3
        movearr[1]|> should equal D3

    [<Test>]
    let CountKingMovesAtE1a() =
        EngBoard.Default()
        let res = Board.Move(F1, F3, PromNone)
        let res = Board.Move(G1, G3, PromNone)
        let moveList = MoveList.ForSq(E1)
        moveList.Count |> should equal 2
        let movearr = Bits.ToArray(moveList.Moves)
        movearr[0]|> should equal F1
        movearr[1]|> should equal G1

    [<Test>]
    let CountPawnMovesEP() =
        Brd <- epboard
        let moveList = MoveList.ForSq(E5)
        moveList.Count |> should equal 2
        let movearr = Bits.ToArray(moveList.Moves)
        movearr[0]|> should equal D6
        movearr[1]|> should equal E6

    [<Test>]
    let CountPawnMovesPRM() =
        Brd <- prmboard
        let moveList = MoveList.ForSq(C7)
        moveList.Count |> should equal 1
        let movearr = Bits.ToArray(moveList.Moves)
        movearr[0]|> should equal B8
        moveList.Promotion |> should equal true

    [<Test>]
    let CountPawnCapturesAtAB6() =
        let mutable usebd = Board.Default()
        let res = Board.Move(B2, B6, PromNone)
        // Generate pins and check bitboards.
        let stm = if board.IsWtm then 0 else 1
        let xstm = if board.IsWtm then 1 else 0
        let kingSq = if stm = White then board.WhiteKingLoc else board.BlackKingLoc
        Brd <- board
        let (hv, d) = MoveList.PinBitBoards(kingSq, stm, xstm)
        let (checks, doubleChecked) = MoveList.CheckBitBoard(kingSq, xstm)
        let moveList = MoveList.ForPawns(B6, hv, d, checks)
        moveList.Count |> should equal 2
        let movearr = Bits.ToArray(moveList.Moves)
        movearr[0]|> should equal A7
        movearr[1]|> should equal C7
