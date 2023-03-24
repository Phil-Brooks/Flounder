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
        (Piece.Pawn, PieceColor.White) |> should equal Map.[Square.A2]

    [<Test>]
    let GetWhiteRook() =
        (Piece.Rook, PieceColor.White) |> should equal Map.[Square.A1]

    [<Test>]
    let GetWhiteQueen() =
        (Piece.Queen, PieceColor.White) |> should equal Map.[Square.D1]

    [<Test>]
    let GetWhiteKing() =
        (Piece.King, PieceColor.White) |> should equal Map.[Square.E1]

    [<Test>]
    let GetBlackPawn() =
        (Piece.Pawn, PieceColor.Black) |> should equal Map.[Square.A7]

    [<Test>]
    let GetBlackRook() =
        (Piece.Rook, PieceColor.Black) |> should equal Map.[Square.A8]

    [<Test>]
    let GetBlackQueen() =
        (Piece.Queen, PieceColor.Black) |> should equal Map.[Square.D8]

    [<Test>]
    let GetBlackKing() =
        (Piece.King, PieceColor.Black) |> should equal Map.[Square.E8]

    [<Test>]
    let MoveWhitePawn() =
        let useMap = Map.Copy()
        useMap.Move(Square.A2, Square.A4)
        (Piece.Pawn, PieceColor.White) |> should equal useMap.[Square.A4]

    [<Test>]
    let MoveWhitePawnInEnemy() =
        let useMap = Map.Copy()
        useMap.Move(Square.A2, Square.A7)
        (Piece.Pawn, PieceColor.White) |> should equal useMap.[Square.A7]
        useMap.[Piece.Pawn, PieceColor.Black].Count |> should equal 7

    [<Test>]
    let RemoveWhitePawn() =
        let useMap = Map.Copy()
        useMap.Empty(Square.A2)
        (Piece.Empty, PieceColor.None) |> should equal useMap.[Square.A2]
        let fen = useMap.GenerateBoardFen()
        fen |> should equal "rnbqkbnr/pppppppp/8/8/8/8/1PPPPPPP/RNBQKBNR"

    [<Test>]
    let MoveKnightToA3() =
        let useMap = Map.Copy()
        useMap.Move(Square.B1, Square.A3)
        (Piece.Knight, PieceColor.White) |> should equal useMap.[Square.A3]

    [<Test>]
    let AddWhitePawn() =
        let useMap = Map.Copy()
        useMap.InsertPiece(Piece.Pawn,PieceColor.White,Square.A4)
        (Piece.Pawn, PieceColor.White) |> should equal useMap.[Square.A4]

    [<Test>]
    let ConfirmBoardState() =
        let fen = Map.GenerateBoardFen()
        fen |> should equal "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR"

    [<Test>]
    let Elements() =
        Map.Bb.Length|> should equal 2
        Map.Bb.[0].Length |> should equal 6
        Map.Bb.[0].[5].Count |> should equal 1
        Map.Bb.[1].[3].Count |> should equal 2
        Map.Bb.[1].[0].Count |> should equal 8

        Map.PiecesAndColors.Length |> should equal 64
        Map.PiecesAndColors.[0] |> should equal ColPiece.BlackRook
        Map.PiecesAndColors.[63] |> should equal ColPiece.WhiteRook
        Map.PiecesAndColors.[8] |> should equal ColPiece.BlackPawn

        Map.White.Count |> should equal 16
        Map.Black.Count |> should equal 16
        Map.ColorToMove |> should equal PieceColor.White
        Map.BlackKCastle |> should equal 0x4
        Map.BlackQCastle |> should equal 0x8
        Map.WhiteKCastle |> should equal 0x1
        Map.WhiteQCastle |> should equal 0x2
        Map.EnPassantTarget |> should equal Square.Na
        Map.ZobristHash |> should equal 608795416857759544UL

    [<Test>]
    let ItemE1() =
        let (pc,col) = Map.[Square.E1]
        pc |> should equal Piece.King
        col |> should equal PieceColor.White

    [<Test>]
    let ItemW() =
        let ans = Map.[PieceColor.White].Count
        ans |> should equal 16

    [<Test>]
    let ItemWP() =
        let ans = Map.[Piece.Pawn, PieceColor.White].Count
        ans |> should equal 8

    [<Test>]
    let PieceOnlyE1() =
        let ans = Map.PieceOnly(Square.E1)
        ans |> should equal Piece.King

    [<Test>]
    let ColorOnlyE1() =
        let ans = Map.ColorOnly(Square.E1)
        ans |> should equal PieceColor.White
