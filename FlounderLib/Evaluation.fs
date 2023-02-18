namespace FlounderLib

module Evaluation =
    let NNUE(board:Board) =
        NNUE.Evaluate(board.ColorToMove)
    let Relative(board) =
        NNUE(board)

