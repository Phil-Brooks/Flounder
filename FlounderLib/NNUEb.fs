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
    let mutable AccIndex = 0
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

    //NB need to change the from and mto if Black
    let MoveRequiresRefresh(piece:ColPiece, from:int, mto:int) =
        let pc,_ = ColPiece.ToPcCol(piece)
        if pc<>Piece.King then false
        elif (from &&& 4) <> (mto &&& 4) then true
        else KING_BUCKETS.[from]<>KING_BUCKETS.[mto]
    let FeatureIdx(colpc:ColPiece, sq:Square, kingsq:Square, view:PieceColor) =
        let oP = 6 * ((int(colpc) ^^^ int(view)) &&& 0x1) + int(colpc)/2
        let oK = (7 * if (int(kingsq) &&& 4) = 0 then 1 else 0) ^^^ (56 * int(view)) ^^^ int(kingsq)
        let oSq = (7 * if (int(kingsq) &&& 4) = 0 then 1 else 0) ^^^ (56 * int(view)) ^^^ int(sq)
        KING_BUCKETS.[oK] * 12 * 64 + oP * 64 + oSq
    let ApplySubSubAdd(src:int16 array, f1:int, f2:int, f3:int, view:PieceColor) =
        let regs:int16 array = Array.zeroCreate 16
        for c = 0 to 768/16-1 do
            let unrollOffset = c * 16
            for i = 0 to 15 do
                regs.[i] <- src.[i+unrollOffset]
            let o1 = f1 * 768 + unrollOffset
            for i = 0 to 15 do
                regs.[i] <- regs.[i] - NNUEin.InputWeights.[o1 + i]
            let o2 = f2 * 768 + unrollOffset
            for i = 0 to 15 do
                regs.[i] <- regs.[i] - NNUEin.InputWeights.[o2 + i]
            let o3 = f3 * 768 + unrollOffset
            for i = 0 to 15 do
                regs.[i] <- regs.[i] + NNUEin.InputWeights.[o3 + i]
            for i = 0 to 15 do
                Accumulators.[AccIndex].AccValues.[int(view)].[unrollOffset+i] <- regs.[i]
    let ApplySubAdd(src:int16 array, f1:int, f2:int, view:PieceColor) =
        let regs:int16 array = Array.zeroCreate 16
        for c = 0 to 768/16-1 do
            let unrollOffset = c * 16
            for i = 0 to 15 do
                regs.[i] <- src.[i+unrollOffset]
            let o1 = f1 * 768 + unrollOffset
            for i = 0 to 15 do
                regs.[i] <- regs.[i] - NNUEin.InputWeights.[o1 + i]
            let o2 = f2 * 768 + unrollOffset
            for i = 0 to 15 do
                regs.[i] <- regs.[i] + NNUEin.InputWeights.[o2 + i]
            for i = 0 to 15 do
                Accumulators.[AccIndex].AccValues.[int(view)].[unrollOffset+i] <- regs.[i]
    let ApplyUpdates(map:BitBoardMap, move:RevertMove, view:PieceColor) =
        let captured = ColPiece.FromPcCol(move.CapturedPiece,move.CapturedColor)
        let prev = Accumulators.[AccIndex-1].AccValues.[int(view)]
        let king = map.[Piece.King, view].ToSq()
        let movingSide = move.ColorToMove
        let colpcto = ColPiece.FromPcCol(map.[move.To])
        let colpcfrom =
            if move.Promotion then ColPiece.FromPcCol(Piece.Pawn,movingSide)
            else colpcto
        let from = FeatureIdx(colpcfrom, move.From, king, view)
        let mto = FeatureIdx(colpcto, move.To, king, view)
        //IsCas
        if move.SecondaryFrom<>Square.Na then
            let colpcrook =  ColPiece.FromPcCol(Piece.Rook,movingSide)
            let rookFrom = FeatureIdx(colpcrook, move.SecondaryFrom, king, view)
            let rookTo = FeatureIdx(colpcrook, move.SecondaryTo, king, view)
            //ApplySubSubAddAdd(output, prev, from, rookFrom, to, rookTo)
            ()
        //IsCap
        elif move.CapturedPiece<>Piece.Empty then
            let capSq = if move.EnPassant then move.EnPassantTarget else move.To
            let capturedTo = FeatureIdx(captured, capSq, king, view)
            ApplySubSubAdd(prev, from, capturedTo, mto, view)
            ()
        else
            ApplySubAdd(prev, from, mto, view)    

    let ApplyDelta(src:int16 array, delta:Delta, perspective) =
        let regs:int16 array = Array.zeroCreate 16
        for c = 0 to 768/16-1 do
            let unrollOffset = c * 16
            for i = 0 to 15 do
                regs.[i] <- src.[i+unrollOffset]
            for r = 0 to delta.r-1 do
                let offset = delta.rem.[r] * 768 + unrollOffset
                for i = 0 to 15 do
                    regs.[i] <- regs.[i] - NNUEin.InputWeights.[offset + i]
            for a = 0 to delta.a-1 do
                let offset = delta.add.[a] * 768 + unrollOffset
                for i = 0 to 15 do
                    regs.[i] <- regs.[i] + NNUEin.InputWeights.[offset + i]
            for i = 0 to 15 do
                Accumulators.[AccIndex].AccValues.[int(perspective)].[unrollOffset+i] <- regs.[i]
    let ResetAccumulator(map:BitBoardMap,perspective:PieceColor) =
        let delta= Delta.Default()
        let kingSq = map.[Piece.King, perspective].ToSq()
        let occupied = ~~~(map.[PieceColor.None])
        let mutable sqIterator = occupied.GetEnumerator()
        let mutable sq = sqIterator.Current
        while (sqIterator.MoveNext()) do
            let colpc = map.PiecesAndColors.[int(sq)]
            delta.add.[delta.a] <- FeatureIdx(colpc,sq,kingSq,perspective)
            sq <- sqIterator.Current
            delta.a <- delta.a + 1
        let src = Array.copy NNUEin.InputBiases
        ApplyDelta(src,delta,perspective)
    let RefreshAccumulator(map:BitBoardMap,perspective:PieceColor) =
        let delta = Delta.Default()
        let kingSq = map.[Piece.King, perspective].ToSq()
        let pBucket = if perspective = PieceColor.White then 0 else 32
        let kingBucket = KING_BUCKETS.[int(kingSq) ^^^ (56 * int(perspective))] + 16 * (Square.ToFile(kingSq) >>> 3)
        let state = RefreshTable.[pBucket + kingBucket]
        for pc in ColPcs do
            let curr = map.Pieces.[int(pc)]
            let prev = state.Pcs.[int(pc)] 
            let rem = prev &&& ~~~curr
            let add = curr &&& ~~~prev
            let mutable remIterator = rem.GetEnumerator()
            let mutable sq = remIterator.Current
            while (remIterator.MoveNext()) do
                delta.rem.[delta.r] <- FeatureIdx(pc,sq,kingSq,perspective)
                sq <- remIterator.Current
                delta.r <- delta.r + 1
            let mutable addIterator = add.GetEnumerator()
            let mutable sq = addIterator.Current
            while (addIterator.MoveNext()) do
                delta.add.[delta.a] <- FeatureIdx(pc,sq,kingSq,perspective)
                sq <- addIterator.Current
                delta.a <- delta.a + 1
            state.Pcs.[int(pc)] <- curr
        ApplyDelta(state.AccKsValues, delta, perspective)

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
    
    
    
