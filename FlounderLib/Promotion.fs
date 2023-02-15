namespace FlounderLib

type Promotion =
    |None = 0uy
    |Knight = 2uy
    |Bishop = 3uy
    |Rook = 1uy
    |Queen = 4uy

module Promotion =
    let ToUciNotation(promotion:Promotion) =
        let notation = promotion.ToString().[0].ToString().ToLower()
        if promotion = Promotion.Knight then "n" else notation

