namespace FlounderLib

type OrderedMoveEntry =
    struct
        val From: int
        val To: int
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
            let from = Square.ToStr(this.From)
            let mto = Square.ToStr(this.To)
            let promotion = if this.Promotion <> Promotion.None then Promotion.ToStr(this.Promotion) else ""
            from + mto + promotion
            
    end
module OrderedMoveEntry =
    let Default = OrderedMoveEntry(Na, Na, Promotion.None)
