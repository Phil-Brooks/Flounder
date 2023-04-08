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
        let mutable moves = moveList.Moves.GetEnumerator()
        let mutable move = moves.Current
        move|> should equal A4
        moves.MoveNext()|>ignore
        move <- moves.Current
        move|> should equal A3

    [<Test>]
    let CountKnightMovesAtA3() =
        let usebd = board.Clone()
        let res = usebd.Move(B1, A3)
        let moveList = MoveList(usebd, A3)
        moveList.Count |> should equal 3
        let mutable moves = moveList.Moves.GetEnumerator()
        let mutable move = moves.Current
        move|> should equal B5
        moves.MoveNext()|>ignore
        move <- moves.Current
        move|> should equal C4

    [<Test>]
    let CountRookMovesAtA3() =
        let usebd = board.Clone()
        let res = usebd.Move(A1, A3)
        let moveList = MoveList(usebd, A3)
        moveList.Count |> should equal 11
        let mutable moves = moveList.Moves.GetEnumerator()
        let mutable move = moves.Current
        move|> should equal A7
        moves.MoveNext()|>ignore
        move <- moves.Current
        move|> should equal A6

    [<Test>]
    let CountRookMovesAtA1() =
        let moveList = MoveList(board, A1)
        moveList.Count |> should equal 0

    [<Test>]
    let CountBishopMovesAtC3() =
        let usebd = board.Clone()
        let res = usebd.Move(C1, C3)
        let moveList = MoveList(usebd, C3)
        moveList.Count |> should equal 6
        let mutable moves = moveList.Moves.GetEnumerator()
        let mutable move = moves.Current
        move|> should equal G7
        moves.MoveNext()|>ignore
        move <- moves.Current
        move|> should equal F6

    [<Test>]
    let CountQueenMovesAtC3() =
        let usebd = board.Clone()
        let res = usebd.Move(D1, C3)
        let moveList = MoveList(usebd, C3)
        moveList.Count |> should equal 17
        let mutable moves = moveList.Moves.GetEnumerator()
        let mutable move = moves.Current
        move|> should equal C7
        moves.MoveNext()|>ignore
        move <- moves.Current
        move|> should equal G7

    [<Test>]
    let CountKingMovesAtC3() =
        let usebd = board.Clone()
        let res = usebd.Move(E1, C3)
        let moveList = MoveList(usebd, C3)
        moveList.Count |> should equal 5
        let mutable moves = moveList.Moves.GetEnumerator()
        let mutable move = moves.Current
        move|> should equal B4
        moves.MoveNext()|>ignore
        move <- moves.Current
        move|> should equal C4

    [<Test>]
    let CountKingMovesAtE1a() =
        let usebd = board.Clone()
        let res = usebd.Move(F1, F3)
        let res = usebd.Move(G1, G3)
        let moveList = MoveList(usebd, E1)
        moveList.Count |> should equal 2
        let mutable moves = moveList.Moves.GetEnumerator()
        let mutable move = moves.Current
        move|> should equal F1
        moves.MoveNext()|>ignore
        move <- moves.Current
        move|> should equal G1

    [<Test>]
    let CountPawnMovesEP() =
        let moveList = MoveList(epboard, E5)
        moveList.Count |> should equal 2
        let mutable moves = moveList.Moves.GetEnumerator()
        let mutable move = moves.Current
        move|> should equal D6
        moves.MoveNext()|>ignore
        move <- moves.Current
        move|> should equal E6

    [<Test>]
    let CountPawnMovesPRM() =
        let moveList = MoveList(prmboard, C7)
        moveList.Count |> should equal 1
        let mutable moves = moveList.Moves.GetEnumerator()
        let mutable move = moves.Current
        move|> should equal B8
        moveList.Promotion |> should equal true

    [<Test>]
    let CountPawnCapturesAtAB6() =
        let usebd = board.Clone()
        let res = usebd.Move(B2, B6)
        // Generate pins and check bitboards.
        let kingSq = board.KingLoc(board.Map.stm).ToSq()
        let (hv, d) = MoveList.PinBitBoards(board, kingSq, board.Map.stm, board.Map.xstm)
        let (checks, doubleChecked) = MoveList.CheckBitBoard(board, kingSq, board.Map.xstm)
        let moveList = MoveList(board, B6, hv, d, checks)
        moveList.Count |> should equal 2
        let mutable moves = moveList.Moves.GetEnumerator()
        let mutable move = moves.Current
        move|> should equal A7
        moves.MoveNext()|>ignore
        move <- moves.Current
        move|> should equal C7
