namespace FlounderLib
open System.Reflection
open System.IO
open System
open System.Runtime.CompilerServices

[<AutoOpen>]
module NNUE_Load =
    let NNUE = 
        let NNUE_FILE = "FlounderLib.BasicNNUEf"
        let HASH = "0334adf934"
        let loadtxt str =
            let pref = NNUE_FILE + "-" + HASH + "-"
            let resource = pref + str //e.g "FeatureBias.txt" 
            use stream = Assembly.GetExecutingAssembly().GetManifestResourceStream(resource)
            use reader = new StreamReader(stream)
            let nl = Environment.NewLine
            reader.ReadToEnd().Split(nl.ToString(), StringSplitOptions.RemoveEmptyEntries)
            |>Array.map(fun s -> int16(s))
        let fb = loadtxt "FeatureBias.txt" 
        let fw = loadtxt "FeatureWeight.txt" 
        let ffw = loadtxt "FlippedFeatureWeight.txt" 
        let ob = loadtxt "OutBias.txt"
        let ow = loadtxt "OutWeight.txt" 
        let nnue =
            let acca = 
                let ans = Array.zeroCreate 128
                ans|>Array.map (fun a -> Array.zeroCreate 256)
            let accb = 
                let ans = Array.zeroCreate 128
                ans|>Array.map (fun a -> Array.zeroCreate 256)
            {
                Output = Array.zeroCreate 1
                CurrentAccumulator = 0
                FeatureWeight = fw
                FlippedFeatureWeight = ffw
                FeatureBias = fb
                OutWeight = ow
                OutBias = ob
                WhitePOV = Array.zeroCreate 768 
                BlackPOV = Array.zeroCreate 768
                AccumulatorA = acca
                AccumulatorB = accb
                Flatten = Array.zeroCreate 512
            }
        nnue
    let ResetAccumulator() = NNUE.CurrentAccumulator <- 0
    let PushAccumulator() =
        let A = NNUE.AccumulatorA.[NNUE.CurrentAccumulator]
        let targetA = NNUE.AccumulatorA.[NNUE.CurrentAccumulator+1]
        let B = NNUE.AccumulatorB.[NNUE.CurrentAccumulator]
        let targetB = NNUE.AccumulatorB.[NNUE.CurrentAccumulator+1]
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
        NNUE.CurrentAccumulator <- NNUE.CurrentAccumulator + 1
    let PullAccumulator() = NNUE.CurrentAccumulator <- NNUE.CurrentAccumulator - 1
    let RefreshAccumulator(map:BitBoardMap) =
        let colorStride = 64 * 6
        let pieceStride = 64
        Array.Clear(NNUE.WhitePOV)
        Array.Clear(NNUE.BlackPOV)
        for color in Cols do
            for ipiece in Pcs do
                let mutable piece = ipiece
                let mutable whiteIterator = map.[piece, color].GetEnumerator()
                let mutable blackIterator = map.[piece, color].GetEnumerator()
                let originalPiece = piece
                if (piece = Piece.Rook) then piece <- Piece.Bishop
                elif (piece = Piece.Knight) then piece <- Piece.Rook
                elif (piece = Piece.Bishop) then piece <- Piece.Knight
                let mutable sq = whiteIterator.Current
                while (whiteIterator.MoveNext()) do
                    let index = int(color) * colorStride + int(piece) * pieceStride + int(sq)
                    NNUE.WhitePOV.[index] <- 1s
                    sq <- whiteIterator.Current
                sq <- blackIterator.Current
                while (blackIterator.MoveNext()) do
                    let index = int(PieceColor.OppositeColor(color)) * colorStride + int(piece) * pieceStride + (int(sq) ^^^ 56)
                    NNUE.BlackPOV.[index] <- 1s
                    sq <- blackIterator.Current
                piece <- originalPiece
        let accumulatorA = NNUE.AccumulatorA.[NNUE.CurrentAccumulator]
        let accumulatorB = NNUE.AccumulatorB.[NNUE.CurrentAccumulator]
        NN.Forward(NNUE.WhitePOV, NNUE.FeatureWeight, accumulatorA)
        NN.Forward(NNUE.BlackPOV, NNUE.FeatureWeight, accumulatorB)
    let EfficientlyUpdateAccumulatorPc(piece:Piece, color:PieceColor, from:Square, mto:Square) =
        let HIDDEN = 256
        let colorStride = 64 * 6
        let pieceStride = 64
        let nnPiece = NN.PieceToNN(piece)
        let opPieceStride = int(nnPiece) * pieceStride
        let whiteIndexFrom = int(color) * colorStride + opPieceStride + int(from)
        let blackIndexFrom = int(PieceColor.OppositeColor(color)) * colorStride + opPieceStride + (int(from) ^^^ 56)
        let whiteIndexTo = int(color) * colorStride + opPieceStride + int(mto)
        let blackIndexTo = int(PieceColor.OppositeColor(color)) * colorStride + opPieceStride + (int(mto) ^^^ 56)
        let accumulatorA = NNUE.AccumulatorA.[NNUE.CurrentAccumulator]
        let accumulatorB = NNUE.AccumulatorB.[NNUE.CurrentAccumulator]
        NNUE.WhitePOV.[whiteIndexFrom] <- 0s
        NNUE.BlackPOV.[blackIndexFrom] <- 0s
        NNUE.WhitePOV.[whiteIndexTo] <- 1s
        NNUE.BlackPOV.[blackIndexTo] <- 1s
        NN.SubtractAndAddToAll(accumulatorA, NNUE.FlippedFeatureWeight, whiteIndexFrom * HIDDEN, whiteIndexTo * HIDDEN)
        NN.SubtractAndAddToAll(accumulatorB, NNUE.FlippedFeatureWeight, blackIndexFrom * HIDDEN, blackIndexTo * HIDDEN)
    let EfficientlyUpdateAccumulator(isActivate:bool, piece:Piece, color:PieceColor, sq:Square) =
        let HIDDEN = 256
        let colorStride = 64 * 6
        let pieceStride = 64
        let nnPiece = NN.PieceToNN(piece)
        let opPieceStride = int(nnPiece) * pieceStride
        let whiteIndex = int(color) * colorStride + opPieceStride + int(sq)
        let blackIndex = int(PieceColor.OppositeColor(color)) * colorStride + opPieceStride + (int(sq) ^^^ 56)
        let accumulatorA = NNUE.AccumulatorA.[NNUE.CurrentAccumulator]
        let accumulatorB = NNUE.AccumulatorB.[NNUE.CurrentAccumulator]
        if isActivate then
            NNUE.WhitePOV.[whiteIndex] <- 1s
            NNUE.BlackPOV.[blackIndex] <- 1s
            NN.AddToAll(accumulatorA, accumulatorB, NNUE.FlippedFeatureWeight, whiteIndex * HIDDEN, blackIndex * HIDDEN)
        else 
            NNUE.WhitePOV.[whiteIndex] <- 0s
            NNUE.BlackPOV.[blackIndex] <- 0s
            NN.SubtractFromAll(accumulatorA, accumulatorB, NNUE.FlippedFeatureWeight, whiteIndex * HIDDEN, blackIndex * HIDDEN)
    let Evaluate(colorToMove:PieceColor) =
        let HIDDEN = 256
        let QA = 255
        let QB = 64
        let QAB = QA * QB
        let CR_MIN = 0s
        let CR_MAX = int16(1 * QA)
        let SCALE = 400
        let accumulatorA = NNUE.AccumulatorA.[NNUE.CurrentAccumulator]
        let accumulatorB = NNUE.AccumulatorB.[NNUE.CurrentAccumulator]
        if (colorToMove = PieceColor.White) then
            NN.ClippedReLUFlattenAndForward(accumulatorA, accumulatorB, NNUE.FeatureBias, NNUE.OutWeight, NNUE.Output, CR_MIN, CR_MAX, HIDDEN)
        else
            NN.ClippedReLUFlattenAndForward(accumulatorB, accumulatorA, NNUE.FeatureBias, NNUE.OutWeight, NNUE.Output, CR_MIN, CR_MAX, HIDDEN)
        (NNUE.Output.[0] + int(NNUE.OutBias.[0])) * SCALE / QAB
    
    
    
