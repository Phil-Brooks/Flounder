namespace FlounderLib

module Evaluation =
    let NNUE(board:Board) =
        Evaluate(board.ColorToMove)
    let Relative(board) =
        NNUE(board)

