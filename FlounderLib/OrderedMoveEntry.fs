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
    end
module OrderedMoveEntry =
    let Default = OrderedMoveEntry(Square.Na, Square.Na, Promotion.None)