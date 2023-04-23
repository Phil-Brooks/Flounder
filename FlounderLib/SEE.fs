namespace FlounderLib

module SEE =
    let Vals = [| 82;337;365;477;1025;0;0 |]
    let Approx(move:OrdMoveEntryRec) =
        let pfrom = EngBoard.PieceOnly(move.From)
        let mutable pto = EngBoard.PieceOnly(move.To)
        if pfrom = Pawn && move.To = Brd.EnPassantTarget then pto <- Pawn
        let mutable value = Vals[pto]
        if move.Promotion <> PromNone then
            value <- value + Vals[move.Promotion] - Vals[Pawn]
        value - Vals[pfrom]
