namespace FlounderLib

module Evaluation1 =
    let NNEvaluation(board:Board) =
        Evaluation.NNUE.Evaluate(board.ColorToMove)
    let RelativeEvaluation(board) =
        NNEvaluation(board)

