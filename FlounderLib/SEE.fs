namespace FlounderLib

module SEE =
    let Internal = [| 82;337;365;477;1025;0;0 |]
    let inline Approximate(board:EngineBoard, move:OrderedMoveEntry) =
        let from = board.PieceOnly(move.From)
        let mutable mto = board.PieceOnly(move.To)
        // In case of En Passant, we set the target piece to a pawn.
        if (from = Pawn && move.To = board.Brd.EnPassantTarget) then mto <- Pawn
        let mutable value = Internal.[mto]
        if move.Promotion <> PromNone then
            // In the case of a promotion, increment with the difference of the promotion and pawn.
            value <- value + Internal.[move.Promotion] - Internal.[Pawn]
        value - Internal.[from]
