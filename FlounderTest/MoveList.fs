namespace FlounderTest
open NUnit.Framework
open FsUnit
open FlounderLib

module MoveList =
    let board = Board.Default()
    let epfen = "rnbqkbnr/pp2pppp/8/2ppP3/8/8/PPPP1PPP/RNBQKBNR w KQkq d6 0 3"
    let epboard = Board.FromFen(epfen)
    let prmfen = "rnb1kb1r/ppP1pppp/5n2/2p5/8/8/PPPP1PPP/RNBQKBNR w KQkq - 0 5"
    let prmboard = Board.FromFen(prmfen)

    [<SetUp>]
    let Setup () =
        AttackTable1.SetUp()

    [<Test>]
    let CountKnightMovesAtB1() =
        let moveList = MoveList(board, B1)
        moveList.Count |> should equal 2

    [<Test>]
    let CountPawnMovesAtA2() =
        let moveList = MoveList(board, A2)
        moveList.Count |> should equal 2
        let movearr = Bits.ToArray(moveList.Moves)
        movearr[0]|> should equal A4
        movearr[1]|> should equal A3

    [<Test>]
    let CountKnightMovesAtA3() =
        let usebd = Board.Default()
        let res = usebd.Move(B1, A3)
        let moveList = MoveList(usebd, A3)
        moveList.Count |> should equal 3
        let movearr = Bits.ToArray(moveList.Moves)
        movearr[0]|> should equal B5
        movearr[1]|> should equal C4

    [<Test>]
    let CountRookMovesAtA3() =
        let usebd = Board.Default()
        let res = usebd.Move(A1, A3)
        let moveList = MoveList(usebd, A3)
        moveList.Count |> should equal 11
        let movearr = Bits.ToArray(moveList.Moves)
        movearr[0]|> should equal A7
        movearr[1]|> should equal A6

    [<Test>]
    let CountRookMovesAtA1() =
        let moveList = MoveList(board, A1)
        moveList.Count |> should equal 0

    [<Test>]
    let CountBishopMovesAtC3() =
        let usebd = Board.Default()
        let res = usebd.Move(C1, C3)
        let moveList = MoveList(usebd, C3)
        moveList.Count |> should equal 6
        let movearr = Bits.ToArray(moveList.Moves)
        movearr[0]|> should equal G7
        movearr[1]|> should equal F6

    [<Test>]
    let CountQueenMovesAtC3() =
        let usebd = Board.Default()
        let res = usebd.Move(D1, C3)
        let moveList = MoveList(usebd, C3)
        moveList.Count |> should equal 17
        let movearr = Bits.ToArray(moveList.Moves)
        movearr[0]|> should equal C7
        movearr[1]|> should equal G7

    [<Test>]
    let CountKingMovesAtC3() =
        let usebd = Board.Default()
        let res = usebd.Move(E1, C3)
        let moveList = MoveList(usebd, C3)
        moveList.Count |> should equal 5
        let movearr = Bits.ToArray(moveList.Moves)
        movearr[0]|> should equal B4
        movearr[1]|> should equal C4

    [<Test>]
    let CountKingMovesAtE1a() =
        let usebd = Board.Default()
        let res = usebd.Move(F1, F3)
        let res = usebd.Move(G1, G3)
        let moveList = MoveList(usebd, E1)
        moveList.Count |> should equal 2
        let movearr = Bits.ToArray(moveList.Moves)
        movearr[0]|> should equal F1
        movearr[1]|> should equal G1

    [<Test>]
    let CountPawnMovesEP() =
        let moveList = MoveList(epboard, E5)
        moveList.Count |> should equal 2
        let movearr = Bits.ToArray(moveList.Moves)
        movearr[0]|> should equal D6
        movearr[1]|> should equal E6

    [<Test>]
    let CountPawnMovesPRM() =
        let moveList = MoveList(prmboard, C7)
        moveList.Count |> should equal 1
        let movearr = Bits.ToArray(moveList.Moves)
        movearr[0]|> should equal B8
        moveList.Promotion |> should equal true

    [<Test>]
    let CountPawnCapturesAtAB6() =
        let usebd = Board.Default()
        let res = usebd.Move(B2, B6)
        // Generate pins and check bitboards.
        let stm = if board.Map.IsWtm then 0 else 1
        let xstm = if board.Map.IsWtm then 1 else 0
        let kingSq = Bits.ToInt(board.KingLoc(stm))
        let (hv, d) = MoveList.PinBitBoards(board, kingSq, stm, xstm)
        let (checks, doubleChecked) = MoveList.CheckBitBoard(board, kingSq, xstm)
        let moveList = MoveList(board, B6, hv, d, checks)
        moveList.Count |> should equal 2
        let movearr = Bits.ToArray(moveList.Moves)
        movearr[0]|> should equal A7
        movearr[1]|> should equal C7
