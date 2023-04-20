namespace FlounderLib

module OrdMove =
    let Default = 
        {
            From = Na
            To = Na
            Promotion = PromNone
            Score = 0
        }
    let Create(from, mto, prom) =
        {
            From = from
            To = mto
            Promotion = prom
            Score = 0
        }
    let ToStr(ome:OrdMoveEntryRec) =
        let from = Square.ToStr(ome.From)
        let mto = Square.ToStr(ome.To)
        let promotion = if ome.Promotion <> PromNone then Promotion.ToStr(ome.Promotion) else ""
        from + mto + promotion
