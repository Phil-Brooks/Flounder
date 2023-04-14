namespace FlounderTest
open NUnit.Framework
open FsUnit
open FlounderLib

module BitBoardMap =

    let Map = Board.Default()
    
    [<SetUp>]
    let Setup () =
        ()

    [<Test>]
    let GetWhitePawn() =
        WhitePawn |> should equal (Map.Squares[A2])

    [<Test>]
    let GetWhiteRook() =
        WhiteRook |> should equal (Map.Squares[A1])

    [<Test>]
    let GetWhiteQueen() =
        WhiteQueen |> should equal (Map.Squares[D1])

    [<Test>]
    let GetWhiteKing() =
        WhiteKing |> should equal (Map.Squares[E1])

    [<Test>]
    let GetBlackPawn() =
        BlackPawn |> should equal (Map.Squares[A7])

    [<Test>]
    let GetBlackRook() =
        BlackRook |> should equal (Map.Squares[A8])

    [<Test>]
    let GetBlackQueen() =
        BlackQueen |> should equal (Map.Squares[D8])

    [<Test>]
    let GetBlackKing() =
        BlackKing |> should equal (Map.Squares[E8])

    [<Test>]
    let MoveWhitePawn() =
        let mutable useMap = Board.Default()
        Board.BaseMove(&useMap, A2, A4)
        WhitePawn |> should equal (useMap.Squares[A4])

    [<Test>]
    let MoveWhitePawnInEnemy() =
        let mutable useMap = Board.Default()
        Board.BaseMove(&useMap, A2, A7)
        WhitePawn |> should equal (useMap.Squares[A7])
        Bits.Count(useMap.Pieces[BlackPawn]) |> should equal 7

    [<Test>]
    let RemoveWhitePawn() =
        let mutable useMap = Board.Default()
        Board.Empty(&useMap, A2)
        EmptyColPc  |> should equal (useMap.Squares[A2])

    [<Test>]
    let MoveKnightToA3() =
        let mutable useMap = Board.Default()
        Board.BaseMove(&useMap, B1, A3)
        WhiteKnight |> should equal (useMap.Squares[A3])

    [<Test>]
    let AddWhitePawn() =
        let mutable useMap = Board.Default()
        Board.InsertPiece(&useMap, WhitePawn,A4)
        WhitePawn |> should equal (useMap.Squares[A4])

    [<Test>]
    let Elements() =
        Map.IsWtm|>should equal true
        Map.Pieces.Length|> should equal 12
        Bits.Count(Map.Pieces.[10]) |> should equal 1
        Bits.Count(Map.Pieces.[7]) |> should equal 2
        Bits.Count(Map.Pieces.[1]) |> should equal 8

        Map.Squares.Length |> should equal 64
        Map.Squares.[0] |> should equal BlackRook
        Map.Squares.[63] |> should equal WhiteRook
        Map.Squares.[8] |> should equal BlackPawn

        Bits.Count(Map.White) |> should equal 16
        Bits.Count(Map.Black) |> should equal 16
        Bits.Count(Map.Both) |> should equal 32
        Map.BlackKCastle |> should equal 0x4
        Map.BlackQCastle |> should equal 0x8
        Map.WhiteKCastle |> should equal 0x1
        Map.WhiteQCastle |> should equal 0x2
        Map.EnPassantTarget |> should equal Na
        Map.ZobristHash |> should equal 2506267901269816621UL

    [<Test>]
    let ItemE1() =
        let pc = Map.Squares[E1]
        pc |> should equal WhiteKing

    [<Test>]
    let ItemW() =
        let ans = Bits.Count(Map.White)
        ans |> should equal 16

    [<Test>]
    let ItemWP() =
        let ans = Bits.Count(Map.Pieces[WhitePawn])
        ans |> should equal 8

    [<Test>]
    let PieceOnlyE1() =
        let ans = Map.Squares.[E1]/2
        ans |> should equal King

    [<Test>]
    let ColorOnlyE1() =
        let ans = Map.Squares.[E1]%2
        ans |> should equal 0
