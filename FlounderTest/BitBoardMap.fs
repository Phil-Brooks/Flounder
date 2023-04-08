namespace FlounderTest
open NUnit.Framework
open FsUnit
open FlounderLib

module BitBoardMap =

    let Map = BitBoardMap("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR", "w", "KQkq", "-")
    
    [<SetUp>]
    let Setup () =
        ()

    [<Test>]
    let Clone() =
        Map |> should equal Map

    [<Test>]
    let GetWhitePawn() =
        WhitePawn |> should equal Map.[Square.A2]

    [<Test>]
    let GetWhiteRook() =
        WhiteRook |> should equal Map.[Square.A1]

    [<Test>]
    let GetWhiteQueen() =
        WhiteQueen |> should equal Map.[Square.D1]

    [<Test>]
    let GetWhiteKing() =
        WhiteKing |> should equal Map.[Square.E1]

    [<Test>]
    let GetBlackPawn() =
        BlackPawn |> should equal Map.[Square.A7]

    [<Test>]
    let GetBlackRook() =
        BlackRook |> should equal Map.[Square.A8]

    [<Test>]
    let GetBlackQueen() =
        BlackQueen |> should equal Map.[Square.D8]

    [<Test>]
    let GetBlackKing() =
        BlackKing |> should equal Map.[Square.E8]

    [<Test>]
    let MoveWhitePawn() =
        let useMap = Map.Copy()
        useMap.Move(Square.A2, Square.A4)
        WhitePawn |> should equal useMap.[Square.A4]

    [<Test>]
    let MoveWhitePawnInEnemy() =
        let useMap = Map.Copy()
        useMap.Move(Square.A2, Square.A7)
        WhitePawn |> should equal useMap.[Square.A7]
        useMap.[Piece.Pawn, 1].Count |> should equal 7

    [<Test>]
    let RemoveWhitePawn() =
        let useMap = Map.Copy()
        useMap.Empty(Square.A2)
        EmptyColPc  |> should equal useMap.[Square.A2]
        let fen = useMap.GenerateBoardFen()
        fen |> should equal "rnbqkbnr/pppppppp/8/8/8/8/1PPPPPPP/RNBQKBNR"

    [<Test>]
    let MoveKnightToA3() =
        let useMap = Map.Copy()
        useMap.Move(Square.B1, Square.A3)
        WhiteKnight |> should equal useMap.[Square.A3]

    [<Test>]
    let AddWhitePawn() =
        let useMap = Map.Copy()
        useMap.InsertPiece(WhitePawn,Square.A4)
        WhitePawn |> should equal useMap.[Square.A4]

    [<Test>]
    let ConfirmBoardState() =
        let fen = Map.GenerateBoardFen()
        fen |> should equal "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"

    [<Test>]
    let Elements() =
        Map.stm|>should equal 0
        Map.Pieces.Length|> should equal 12
        Map.Pieces.[10].Count |> should equal 1
        Map.Pieces.[7].Count |> should equal 2
        Map.Pieces.[1].Count |> should equal 8

        Map.PiecesAndColors.Length |> should equal 64
        Map.PiecesAndColors.[0] |> should equal BlackRook
        Map.PiecesAndColors.[63] |> should equal WhiteRook
        Map.PiecesAndColors.[8] |> should equal BlackPawn

        Map.White.Count |> should equal 16
        Map.Black.Count |> should equal 16
        Map.BlackKCastle |> should equal 0x4
        Map.BlackQCastle |> should equal 0x8
        Map.WhiteKCastle |> should equal 0x1
        Map.WhiteQCastle |> should equal 0x2
        Map.EnPassantTarget |> should equal Square.Na
        Map.ZobristHash |> should equal 608795416857759544UL

    [<Test>]
    let ItemE1() =
        let pc = Map.[Square.E1]
        pc |> should equal WhiteKing

    [<Test>]
    let ItemW() =
        let ans = Map.[0].Count
        ans |> should equal 16

    [<Test>]
    let ItemWP() =
        let ans = Map.[Piece.Pawn, 0].Count
        ans |> should equal 8

    [<Test>]
    let PieceOnlyE1() =
        let ans = Map.PieceOnly(Square.E1)
        ans |> should equal Piece.King

    [<Test>]
    let ColorOnlyE1() =
        let ans = Map.ColorOnly(Square.E1)
        ans |> should equal 0
