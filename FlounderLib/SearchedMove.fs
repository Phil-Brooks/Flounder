namespace FlounderLib

type SearchedMove =
    struct
        val From: Square
        val To: Square
        val Promotion: Promotion
        val Evaluation: int
        new(move:byref<OrderedMoveEntry>, evaluation:int ) = 
            { 
                From = move.From
                To = move.To
                Promotion = move.Promotion
                Evaluation = evaluation
            }
        new(from, mto, promotion, evaluation) =
            {
                From = from
                To = mto
                Promotion = promotion
                Evaluation = evaluation
            }
    end
module SearchedMove =
    let Default = SearchedMove(Square.Na, Square.Na, Promotion.None, 0)