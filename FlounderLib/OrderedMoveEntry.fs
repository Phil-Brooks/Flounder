namespace FlounderLib

type OrderedMoveEntry =
    struct
        val From: Square
        val To: Square
        val Promotion: Promotion
        val mutable Score: int
        new(from, mto, promotion) =
            {
                From = from
                To = mto
                Promotion = promotion
                Score = 0
            }
        override this.ToString() =
            let from = this.From.ToString().ToLower()
            let mto = this.To.ToString().ToLower()
            let promotion = if this.Promotion <> Promotion.None then Promotion.ToStr(this.Promotion) else ""
            from + mto + promotion
            
    end
module OrderedMoveEntry =
    let Default() = OrderedMoveEntry(Square.Na, Square.Na, Promotion.None)
