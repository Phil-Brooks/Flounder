namespace FlounderLib
open System.Reflection
open System.IO
open System
open System.Runtime.CompilerServices

type NNUEinB =
    {
        InputWeights:int16 array
        InputBiases:int16 array
        OutputWeights:int16 array
        OutputBias:int
    }
type Accumulator =
    {
        AccValues:int16 array array
    }
type AccumulatorKingState =
    {
        AccKsValues:int16 array
        Pcs:BitBoard array
    }
module NNUEb =
    let NNUEin = 
        let NNUE_FILE = "FlounderLib.berserk"
        let HASH = "e3f526b26f50"
        let resource = NNUE_FILE + "-" + HASH + ".nn"
        use stream = Assembly.GetExecutingAssembly().GetManifestResourceStream(resource)
        use reader = new BinaryReader(stream)
        let iwlen = 9437184
        let iw = Array.zeroCreate iwlen
        for i = 0 to iwlen-1 do
            iw.[i] <- reader.ReadInt16()
        let iblen = 768
        let ib = Array.zeroCreate iblen
        for i = 0 to iblen-1 do
            ib.[i] <- reader.ReadInt16()
        let owlen = 1536
        let ow = Array.zeroCreate owlen
        for i = 0 to owlen-1 do
            ow.[i] <- reader.ReadInt16()
        let ob = reader.ReadInt32()
        let nnue =
            {
                InputWeights = iw
                InputBiases = ib
                OutputWeights = ow
                OutputBias = ob
            }
        nnue
    let Accumulators:Accumulator array =
        let ans = Array.zeroCreate 252
        for i = 0 to 251 do
            let accs = 
                let ans = Array.zeroCreate 2
                ans|>Array.map (fun a -> Array.zeroCreate 768)
            let acci =
                {
                    AccValues = accs
                }
            ans.[i] <- acci
        ans
    let RefreshTable:AccumulatorKingState array =
        let ans = Array.zeroCreate 64
        for i = 0 to 63 do
            let acci =
                {
                    AccKsValues = Array.zeroCreate 768
                    Pcs = Array.zeroCreate 12
                }
            ans.[i] <- acci
        ans
    let ResetRefreshTable() =
        for i = 0 to 63 do
            let acci =
                {
                    AccKsValues = Array.copy NNUEin.InputBiases
                    Pcs = Array.zeroCreate 12
                }
            RefreshTable.[i] <- acci
    let ResetAccumulator(map:BitBoardMap,perspective:PieceColor) =
        let adds = Array.create 32 0
        let kingSq = map.[Piece.King, perspective]
        ()


    //let ResetAccumulator() = NNUEout.CurrentAccumulator <- 0
    //let PushAccumulator() =
    //    let A = NNUEout.AccumulatorA.[NNUEout.CurrentAccumulator]
    //    let targetA = NNUEout.AccumulatorA.[NNUEout.CurrentAccumulator+1]
    //    let B = NNUEout.AccumulatorB.[NNUEout.CurrentAccumulator]
    //    let targetB = NNUEout.AccumulatorB.[NNUEout.CurrentAccumulator+1]
    //    let HIDDEN = 256
    //    let size = uint(HIDDEN * Unsafe.SizeOf<int16>())
    //    Unsafe.CopyBlockUnaligned(
    //        &Unsafe.As<int16, byte>(&targetA.[0]), 
    //        &Unsafe.As<int16, byte>(&A.[0]), 
    //        size
    //    )
    //    Unsafe.CopyBlockUnaligned(
    //        &Unsafe.As<int16, byte>(&targetB.[0]), 
    //        &Unsafe.As<int16, byte>(&B.[0]), 
    //        size
    //    )
    //    NNUEout.CurrentAccumulator <- NNUEout.CurrentAccumulator + 1
    //let PullAccumulator() = NNUEout.CurrentAccumulator <- NNUEout.CurrentAccumulator - 1
    //let RefreshAccumulator(map:BitBoardMap) =
    //    let colorStride = 64 * 6
    //    let pieceStride = 64
    //    Array.Clear(NNUEout.WhitePOV)
    //    Array.Clear(NNUEout.BlackPOV)
    //    for color in Cols do
    //        for ipiece in Pcs do
    //            let mutable piece = ipiece
    //            let mutable whiteIterator = map.[piece, color].GetEnumerator()
    //            let mutable blackIterator = map.[piece, color].GetEnumerator()
    //            let originalPiece = piece
    //            if (piece = Piece.Rook) then piece <- Piece.Bishop
    //            elif (piece = Piece.Knight) then piece <- Piece.Rook
    //            elif (piece = Piece.Bishop) then piece <- Piece.Knight
    //            let mutable sq = whiteIterator.Current
    //            while (whiteIterator.MoveNext()) do
    //                let index = int(color) * colorStride + int(piece) * pieceStride + int(sq)
    //                NNUEout.WhitePOV.[index] <- 1s
    //                sq <- whiteIterator.Current
    //            sq <- blackIterator.Current
    //            while (blackIterator.MoveNext()) do
    //                let index = int(PieceColor.OppositeColor(color)) * colorStride + int(piece) * pieceStride + (int(sq) ^^^ 56)
    //                NNUEout.BlackPOV.[index] <- 1s
    //                sq <- blackIterator.Current
    //            piece <- originalPiece
    //    let accumulatorA = NNUEout.AccumulatorA.[NNUEout.CurrentAccumulator]
    //    let accumulatorB = NNUEout.AccumulatorB.[NNUEout.CurrentAccumulator]
    //    NN.Forward(NNUEout.WhitePOV, NNUEin.FeatureWeight, accumulatorA)
    //    NN.Forward(NNUEout.BlackPOV, NNUEin.FeatureWeight, accumulatorB)
    //let EfficientlyUpdateAccumulatorPc(piece:Piece, color:PieceColor, from:Square, mto:Square) =
    //    let HIDDEN = 256
    //    let colorStride = 64 * 6
    //    let pieceStride = 64
    //    let nnPiece = NN.PieceToNN(piece)
    //    let opPieceStride = int(nnPiece) * pieceStride
    //    let whiteIndexFrom = int(color) * colorStride + opPieceStride + int(from)
    //    let blackIndexFrom = int(PieceColor.OppositeColor(color)) * colorStride + opPieceStride + (int(from) ^^^ 56)
    //    let whiteIndexTo = int(color) * colorStride + opPieceStride + int(mto)
    //    let blackIndexTo = int(PieceColor.OppositeColor(color)) * colorStride + opPieceStride + (int(mto) ^^^ 56)
    //    let accumulatorA = NNUEout.AccumulatorA.[NNUEout.CurrentAccumulator]
    //    let accumulatorB = NNUEout.AccumulatorB.[NNUEout.CurrentAccumulator]
    //    NNUEout.WhitePOV.[whiteIndexFrom] <- 0s
    //    NNUEout.BlackPOV.[blackIndexFrom] <- 0s
    //    NNUEout.WhitePOV.[whiteIndexTo] <- 1s
    //    NNUEout.BlackPOV.[blackIndexTo] <- 1s
    //    NN.SubtractAndAddToAll(accumulatorA, NNUEin.FlippedFeatureWeight, whiteIndexFrom * HIDDEN, whiteIndexTo * HIDDEN)
    //    NN.SubtractAndAddToAll(accumulatorB, NNUEin.FlippedFeatureWeight, blackIndexFrom * HIDDEN, blackIndexTo * HIDDEN)
    //let EfficientlyUpdateAccumulator(isActivate:bool, piece:Piece, color:PieceColor, sq:Square) =
    //    let HIDDEN = 256
    //    let colorStride = 64 * 6
    //    let pieceStride = 64
    //    let nnPiece = NN.PieceToNN(piece)
    //    let opPieceStride = int(nnPiece) * pieceStride
    //    let whiteIndex = int(color) * colorStride + opPieceStride + int(sq)
    //    let blackIndex = int(PieceColor.OppositeColor(color)) * colorStride + opPieceStride + (int(sq) ^^^ 56)
    //    let accumulatorA = NNUEout.AccumulatorA.[NNUEout.CurrentAccumulator]
    //    let accumulatorB = NNUEout.AccumulatorB.[NNUEout.CurrentAccumulator]
    //    if isActivate then
    //        NNUEout.WhitePOV.[whiteIndex] <- 1s
    //        NNUEout.BlackPOV.[blackIndex] <- 1s
    //        NN.AddToAll(accumulatorA, accumulatorB, NNUEin.FlippedFeatureWeight, whiteIndex * HIDDEN, blackIndex * HIDDEN)
    //    else 
    //        NNUEout.WhitePOV.[whiteIndex] <- 0s
    //        NNUEout.BlackPOV.[blackIndex] <- 0s
    //        NN.SubtractFromAll(accumulatorA, accumulatorB, NNUEin.FlippedFeatureWeight, whiteIndex * HIDDEN, blackIndex * HIDDEN)
    //let Evaluate(colorToMove:PieceColor) =
    //    let HIDDEN = 256
    //    let QA = 255
    //    let QB = 64
    //    let QAB = QA * QB
    //    let CR_MIN = 0s
    //    let CR_MAX = int16(1 * QA)
    //    let SCALE = 400
    //    let accumulatorA = NNUEout.AccumulatorA.[NNUEout.CurrentAccumulator]
    //    let accumulatorB = NNUEout.AccumulatorB.[NNUEout.CurrentAccumulator]
    //    if (colorToMove = PieceColor.White) then
    //        NN.ClippedReLUFlattenAndForward(accumulatorA, accumulatorB, NNUEin.FeatureBias, NNUEin.OutWeight, NNUEout.Output, CR_MIN, CR_MAX, HIDDEN)
    //    else
    //        NN.ClippedReLUFlattenAndForward(accumulatorB, accumulatorA, NNUEin.FeatureBias, NNUEin.OutWeight, NNUEout.Output, CR_MIN, CR_MAX, HIDDEN)
    //    (NNUEout.Output.[0] + int(NNUEin.OutBias.[0])) * SCALE / QAB
    
    
    
