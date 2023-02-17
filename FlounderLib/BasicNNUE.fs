namespace FlounderLib
open System
open System.Runtime.CompilerServices

[<Serializable>]
type BasicNNUE =
    {
        Output:int array
        mutable CurrentAccumulator:int
        FeatureWeight:int16 array
        FlippedFeatureWeight:int16 array
        FeatureBias:int16 array
        OutWeight:int16 array
        OutBias:int16 array
        WhitePOV:int16 array
        BlackPOV:int16 array
        AccumulatorA:int16 array array
        AccumulatorB:int16 array array
        Flatten:int16 array
    }
    member this.ResetAccumulator() = this.CurrentAccumulator <- 0
    member this.PushAccumulator() =
        let A = this.AccumulatorA.[this.CurrentAccumulator]
        let targetA = this.AccumulatorA.[this.CurrentAccumulator+1]
        let B = this.AccumulatorB.[this.CurrentAccumulator]
        let targetB = this.AccumulatorB.[this.CurrentAccumulator+1]
        let HIDDEN = 256
        let size = uint(HIDDEN * Unsafe.SizeOf<int16>())
        Unsafe.CopyBlockUnaligned(
            &Unsafe.As<int16, byte>(&targetA.[0]), 
            &Unsafe.As<int16, byte>(&A.[0]), 
            size
        )
        Unsafe.CopyBlockUnaligned(
            &Unsafe.As<int16, byte>(&targetB.[0]), 
            &Unsafe.As<int16, byte>(&B.[0]), 
            size
        )
        this.CurrentAccumulator <- this.CurrentAccumulator + 1
    member this.PullAccumulator() = this.CurrentAccumulator <- this.CurrentAccumulator - 1
    member this.RefreshAccumulator(board:Board) =
        let colorStride = 64 * 6
        let pieceStride = 64
        Array.Clear(this.WhitePOV)
        Array.Clear(this.BlackPOV)
        for color in [PieceColor.White;PieceColor.Black] do
            for ipiece in [Piece.Pawn;Piece.Rook;Piece.Knight;Piece.Bishop;Piece.Queen;Piece.King] do
                let mutable piece = ipiece
                let mutable whiteIterator = board.All(piece, color).GetEnumerator()
                let mutable blackIterator = board.All(piece, color).GetEnumerator()
                let originalPiece = piece
                if (piece = Piece.Rook) then piece <- Piece.Bishop
                elif (piece = Piece.Knight) then piece <- Piece.Rook
                elif (piece = Piece.Bishop) then piece <- Piece.Knight
                let mutable sq = whiteIterator.Current
                while (whiteIterator.MoveNext()) do
                    let index = int(color) * colorStride + int(piece) * pieceStride + int(sq)
                    this.WhitePOV.[index] <- 1s
                    sq <- whiteIterator.Current
                sq <- blackIterator.Current
                while (blackIterator.MoveNext()) do
                    let index = int(PieceColor.OppositeColor(color)) * colorStride + int(piece) * pieceStride + (int(sq) ^^^ 56)
                    this.BlackPOV.[index] <- 1s
                    sq <- blackIterator.Current
                piece <- originalPiece
        let accumulatorA = this.AccumulatorA.[this.CurrentAccumulator]
        let accumulatorB = this.AccumulatorB.[this.CurrentAccumulator]
        NN.Forward(this.WhitePOV, this.FeatureWeight, accumulatorA)
        NN.Forward(this.BlackPOV, this.FeatureWeight, accumulatorB)
    member this.EfficientlyUpdateAccumulator(piece:Piece, color:PieceColor, from:Square, mto:Square) =
        let HIDDEN = 256
        let colorStride = 64 * 6
        let pieceStride = 64
        let nnPiece = NN.PieceToNN(piece)
        let opPieceStride = int(nnPiece) * pieceStride
        let whiteIndexFrom = int(color) * colorStride + opPieceStride + int(from)
        let blackIndexFrom = int(PieceColor.OppositeColor(color)) * colorStride + opPieceStride + (int(from) ^^^ 56)
        let whiteIndexTo = int(color) * colorStride + opPieceStride + int(mto)
        let blackIndexTo = int(PieceColor.OppositeColor(color)) * colorStride + opPieceStride + (int(mto) ^^^ 56)
        let accumulatorA = this.AccumulatorA.[this.CurrentAccumulator]
        let accumulatorB = this.AccumulatorB.[this.CurrentAccumulator]
        this.WhitePOV.[whiteIndexFrom] <- 0s
        this.BlackPOV.[blackIndexFrom] <- 0s
        this.WhitePOV.[whiteIndexTo] <- 1s
        this.BlackPOV.[blackIndexTo] <- 1s
        NN.SubtractAndAddToAll(accumulatorA, this.FlippedFeatureWeight, whiteIndexFrom * HIDDEN, whiteIndexTo * HIDDEN)
        NN.SubtractAndAddToAll(accumulatorB, this.FlippedFeatureWeight, blackIndexFrom * HIDDEN, blackIndexTo * HIDDEN)
    member this.EfficientlyUpdateAccumulator(isActivate:bool, piece:Piece, color:PieceColor, sq:Square) =
        let HIDDEN = 256
        let colorStride = 64 * 6
        let pieceStride = 64
        let nnPiece = NN.PieceToNN(piece)
        let opPieceStride = int(nnPiece) * pieceStride
        let whiteIndex = int(color) * colorStride + opPieceStride + int(sq)
        let blackIndex = int(PieceColor.OppositeColor(color)) * colorStride + opPieceStride + (int(sq) ^^^ 56)
        let accumulatorA = this.AccumulatorA.[this.CurrentAccumulator]
        let accumulatorB = this.AccumulatorB.[this.CurrentAccumulator]
        if isActivate then
            this.WhitePOV.[whiteIndex] <- 1s
            this.BlackPOV.[blackIndex] <- 1s
            NN.AddToAll(accumulatorA, accumulatorB, this.FlippedFeatureWeight, whiteIndex * HIDDEN, blackIndex * HIDDEN)
        else 
            this.WhitePOV.[whiteIndex] <- 0s
            this.BlackPOV.[blackIndex] <- 0s
            NN.SubtractFromAll(accumulatorA, accumulatorB, this.FlippedFeatureWeight, whiteIndex * HIDDEN, blackIndex * HIDDEN)
    member this.Evaluate(colorToMove:PieceColor) =
        let HIDDEN = 256
        let QA = 255
        let QB = 64
        let QAB = QA * QB
        let CR_MIN = 0s
        let CR_MAX = int16(1 * QA)
        let SCALE = 400

#if DEBUG
        let mutable firstOffset = 0
        let mutable secondOffset = 256
        if (colorToMove = PieceColor.Black) then
            firstOffset <- 256
            secondOffset <- 0
#endif
        let accumulatorA = this.AccumulatorA.[this.CurrentAccumulator]
        let accumulatorB = this.AccumulatorB.[this.CurrentAccumulator]

#if RELEASE
        if (colorToMove = PieceColor.White) then
            NN.ClippedReLUFlattenAndForward(accumulatorA, accumulatorB, this.FeatureBias, this.OutWeight, this.Output, CR_MIN, CR_MAX, HIDDEN)
        else
            NN.ClippedReLUFlattenAndForward(accumulatorB, accumulatorA, this.FeatureBias, this.OutWeight, this.Output, CR_MIN, CR_MAX, HIDDEN)
#endif
        
#if DEBUG
        NN.ClippedReLU(accumulatorA, this.FeatureBias, this.Flatten, CR_MIN, CR_MAX, firstOffset)
        NN.ClippedReLU(accumulatorB, this.FeatureBias, this.Flatten, CR_MIN, CR_MAX, secondOffset)
        NN.Forward1(this.Flatten, this.OutWeight, this.Output)
#endif
        (this.Output.[0] + int(this.OutBias.[0])) * SCALE / QAB
