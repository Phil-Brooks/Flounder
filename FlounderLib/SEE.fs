namespace FlounderLib

module SEE =
    let Internal = [| 82;477;337;365;1025;0;0 |]
    let inline Approximate(board:EngineBoard, move:OrderedMoveEntry) =
        let from = board.PieceOnly(move.From)
        let mutable mto = board.PieceOnly(move.To)
        // In case of En Passant, we set the target piece to a pawn.
        if (from = Piece.Pawn && move.To = board.Brd.EnPassantTarget) then mto <- Piece.Pawn
        let mutable value = Internal.[int(mto)]
        if (move.Promotion <> Promotion.None) then
            // In the case of a promotion, increment with the difference of the promotion and pawn.
            value <- value + Internal.[int(move.Promotion)] - Internal.[int(Piece.Pawn)]
        value - Internal.[int(from)]
