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
        let moveList = MoveList(board, Square.B1)
        moveList.Count |> should equal 2

    [<Test>]
    let CountPawnMovesAtA2() =
        let moveList = MoveList(board, Square.A2)
        moveList.Count |> should equal 2
        let mutable moves = moveList.Moves.GetEnumerator()
        let mutable move = moves.Current
        move|> should equal Square.A4
        moves.MoveNext()|>ignore
        move <- moves.Current
        move|> should equal Square.A3

    [<Test>]
    let CountKnightMovesAtA3() =
        let usebd = board.Clone()
        let res = usebd.Move(Square.B1, Square.A3)
        let moveList = MoveList(usebd, Square.A3)
        moveList.Count |> should equal 3
        let mutable moves = moveList.Moves.GetEnumerator()
        let mutable move = moves.Current
        move|> should equal Square.B5
        moves.MoveNext()|>ignore
        move <- moves.Current
        move|> should equal Square.C4

    [<Test>]
    let CountRookMovesAtA3() =
        let usebd = board.Clone()
        let res = usebd.Move(Square.A1, Square.A3)
        let moveList = MoveList(usebd, Square.A3)
        moveList.Count |> should equal 11
        let mutable moves = moveList.Moves.GetEnumerator()
        let mutable move = moves.Current
        move|> should equal Square.A7
        moves.MoveNext()|>ignore
        move <- moves.Current
        move|> should equal Square.A6

    [<Test>]
    let CountRookMovesAtA1() =
        let moveList = MoveList(board, Square.A1)
        moveList.Count |> should equal 0

    [<Test>]
    let CountBishopMovesAtC3() =
        let usebd = board.Clone()
        let res = usebd.Move(Square.C1, Square.C3)
        let moveList = MoveList(usebd, Square.C3)
        moveList.Count |> should equal 6
        let mutable moves = moveList.Moves.GetEnumerator()
        let mutable move = moves.Current
        move|> should equal Square.G7
        moves.MoveNext()|>ignore
        move <- moves.Current
        move|> should equal Square.F6

    [<Test>]
    let CountQueenMovesAtC3() =
        let usebd = board.Clone()
        let res = usebd.Move(Square.D1, Square.C3)
        let moveList = MoveList(usebd, Square.C3)
        moveList.Count |> should equal 17
        let mutable moves = moveList.Moves.GetEnumerator()
        let mutable move = moves.Current
        move|> should equal Square.C7
        moves.MoveNext()|>ignore
        move <- moves.Current
        move|> should equal Square.G7

    [<Test>]
    let CountKingMovesAtC3() =
        let usebd = board.Clone()
        let res = usebd.Move(Square.E1, Square.C3)
        let moveList = MoveList(usebd, Square.C3)
        moveList.Count |> should equal 5
        let mutable moves = moveList.Moves.GetEnumerator()
        let mutable move = moves.Current
        move|> should equal Square.B4
        moves.MoveNext()|>ignore
        move <- moves.Current
        move|> should equal Square.C4

    [<Test>]
    let CountKingMovesAtE1a() =
        let usebd = board.Clone()
        let res = usebd.Move(Square.F1, Square.F3)
        let res = usebd.Move(Square.G1, Square.G3)
        let moveList = MoveList(usebd, Square.E1)
        moveList.Count |> should equal 2
        let mutable moves = moveList.Moves.GetEnumerator()
        let mutable move = moves.Current
        move|> should equal Square.F1
        moves.MoveNext()|>ignore
        move <- moves.Current
        move|> should equal Square.G1

    [<Test>]
    let CountPawnMovesEP() =
        let moveList = MoveList(epboard, Square.E5)
        moveList.Count |> should equal 2
        let mutable moves = moveList.Moves.GetEnumerator()
        let mutable move = moves.Current
        move|> should equal Square.D6
        moves.MoveNext()|>ignore
        move <- moves.Current
        move|> should equal Square.E6

    [<Test>]
    let CountPawnMovesPRM() =
        let moveList = MoveList(prmboard, Square.C7)
        moveList.Count |> should equal 1
        let mutable moves = moveList.Moves.GetEnumerator()
        let mutable move = moves.Current
        move|> should equal Square.B8
        moveList.Promotion |> should equal true

    [<Test>]
    let CountPawnCapturesAtAB6() =
        let usebd = board.Clone()
        let res = usebd.Move(Square.B2, Square.B6)
        let oppositeColor = PieceColor.OppositeColor(board.ColorToMove)
        // Generate pins and check bitboards.
        let kingSq = board.KingLoc(board.ColorToMove).ToSq()
        let (hv, d) = MoveList.PinBitBoards(board, kingSq, board.ColorToMove, oppositeColor)
        let (checks, doubleChecked) = MoveList.CheckBitBoard(board, kingSq, oppositeColor)
        let moveList = MoveList(board, Square.B6, hv, d, checks)
        moveList.Count |> should equal 2
        let mutable moves = moveList.Moves.GetEnumerator()
        let mutable move = moves.Current
        move|> should equal Square.A7
        moves.MoveNext()|>ignore
        move <- moves.Current
        move|> should equal Square.C7
