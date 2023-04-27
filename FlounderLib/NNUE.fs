namespace FlounderLib
open System.Reflection
open System.IO
open System
open System.Runtime.CompilerServices

type NNUEin =
    {
        FeatureWeight:int16 array
        FlippedFeatureWeight:int16 array
        FeatureBias:int16 array
        OutWeight:int16 array
        OutBias:int16 array
    }
type NNUEout =
    {
        Output:int array
        mutable CurrentAccumulator:int
        WhitePOV:int16 array
        BlackPOV:int16 array
        AccumulatorA:int16 array array
        AccumulatorB:int16 array array
        Flatten:int16 array
    }
module NNUE =
    let NNUEin = 
        let NNUE_FILE = "FlounderLib.BasicNNUEf"
        let HASH = "0334adf934"
        let loadtxt str =
            let pref = NNUE_FILE + "-" + HASH + "-"
            let resource = pref + str //e.g "FeatureBias.txt" 
            use stream = Assembly.GetExecutingAssembly().GetManifestResourceStream(resource)
            use reader = new StreamReader(stream)
            reader.ReadToEnd().Split(",", StringSplitOptions.RemoveEmptyEntries)
            |>Array.map(fun s -> int16(s))
        let fb = loadtxt "FeatureBias.txt" 
        let fw = loadtxt "FeatureWeight.txt" 
        let ffw = loadtxt "FlippedFeatureWeight.txt" 
        let ob = loadtxt "OutBias.txt"
        let ow = loadtxt "OutWeight.txt" 
        let nnue =
            {
                FeatureWeight = fw
                FlippedFeatureWeight = ffw
                FeatureBias = fb
                OutWeight = ow
                OutBias = ob
            }
        nnue
    let NNUEout = 
        let acca = 
            let ans = Array.zeroCreate 128
            ans|>Array.map (fun a -> Array.zeroCreate 256)
        let accb = 
            let ans = Array.zeroCreate 128
            ans|>Array.map (fun a -> Array.zeroCreate 256)
        {
            Output = Array.zeroCreate 1
            CurrentAccumulator = 0
            WhitePOV = Array.zeroCreate 768 
            BlackPOV = Array.zeroCreate 768
            AccumulatorA = acca
            AccumulatorB = accb
            Flatten = Array.zeroCreate 512
        }
    let ResetAccumulator() = NNUEout.CurrentAccumulator <- 0
    let PushAccumulator() =
        let A = NNUEout.AccumulatorA.[NNUEout.CurrentAccumulator]
        let targetA = NNUEout.AccumulatorA.[NNUEout.CurrentAccumulator+1]
        let B = NNUEout.AccumulatorB.[NNUEout.CurrentAccumulator]
        let targetB = NNUEout.AccumulatorB.[NNUEout.CurrentAccumulator+1]
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
        NNUEout.CurrentAccumulator <- NNUEout.CurrentAccumulator + 1
    let PullAccumulator() = NNUEout.CurrentAccumulator <- NNUEout.CurrentAccumulator - 1
    let RefreshAccumulator() =
        let colorStride = 64 * 6
        let pieceStride = 64
        Array.Clear(NNUEout.WhitePOV)
        Array.Clear(NNUEout.BlackPOV)
        for pc = WhitePawn to BlackKing do
            let sqarr = Bits.ToArray(Brd.Pieces[pc])
            let color = pc%2
            let oppositeColor = color ^^^ 1
            let piece = pc/2
            for sq in sqarr do
                let index = color * colorStride + piece * pieceStride + sq
                NNUEout.WhitePOV[index] <- 1s
            for sq in sqarr do
                let index = oppositeColor * colorStride + piece * pieceStride + (sq ^^^ 56)
                NNUEout.BlackPOV.[index] <- 1s
        let accumulatorA = NNUEout.AccumulatorA.[NNUEout.CurrentAccumulator]
        let accumulatorB = NNUEout.AccumulatorB.[NNUEout.CurrentAccumulator]
        NN.Forward(NNUEout.WhitePOV, NNUEin.FeatureWeight, accumulatorA)
        NN.Forward(NNUEout.BlackPOV, NNUEin.FeatureWeight, accumulatorB)
    let EfficientlyUpdateAccumulatorPc(piece:int, color:int, from:int, mto:int) =
        let HIDDEN = 256
        let colorStride = 64 * 6
        let pieceStride = 64
        let opPieceStride = piece * pieceStride
        let whiteIndexFrom = color * colorStride + opPieceStride + from
        let blackIndexFrom = (color ^^^ 1) * colorStride + opPieceStride + (from ^^^ 56)
        let whiteIndexTo = color * colorStride + opPieceStride + mto
        let blackIndexTo = (color ^^^ 1) * colorStride + opPieceStride + (mto ^^^ 56)
        let accumulatorA = NNUEout.AccumulatorA.[NNUEout.CurrentAccumulator]
        let accumulatorB = NNUEout.AccumulatorB.[NNUEout.CurrentAccumulator]
        NNUEout.WhitePOV.[whiteIndexFrom] <- 0s
        NNUEout.BlackPOV.[blackIndexFrom] <- 0s
        NNUEout.WhitePOV.[whiteIndexTo] <- 1s
        NNUEout.BlackPOV.[blackIndexTo] <- 1s
        NN.SubtractAndAddToAll(accumulatorA, NNUEin.FlippedFeatureWeight, whiteIndexFrom * HIDDEN, whiteIndexTo * HIDDEN)
        NN.SubtractAndAddToAll(accumulatorB, NNUEin.FlippedFeatureWeight, blackIndexFrom * HIDDEN, blackIndexTo * HIDDEN)
    let EfficientlyUpdateAccumulator(isActivate:bool, piece:int, color:int, sq:int) =
        let HIDDEN = 256
        let colorStride = 64 * 6
        let pieceStride = 64
        let opPieceStride = int(piece) * pieceStride
        let whiteIndex = color * colorStride + opPieceStride + sq
        let blackIndex = (color ^^^ 1) * colorStride + opPieceStride + (sq ^^^ 56)
        let accumulatorA = NNUEout.AccumulatorA.[NNUEout.CurrentAccumulator]
        let accumulatorB = NNUEout.AccumulatorB.[NNUEout.CurrentAccumulator]
        if isActivate then
            NNUEout.WhitePOV.[whiteIndex] <- 1s
            NNUEout.BlackPOV.[blackIndex] <- 1s
            NN.AddToAll(accumulatorA, accumulatorB, NNUEin.FlippedFeatureWeight, whiteIndex * HIDDEN, blackIndex * HIDDEN)
        else 
            NNUEout.WhitePOV.[whiteIndex] <- 0s
            NNUEout.BlackPOV.[blackIndex] <- 0s
            NN.SubtractFromAll(accumulatorA, accumulatorB, NNUEin.FlippedFeatureWeight, whiteIndex * HIDDEN, blackIndex * HIDDEN)
    let Evaluate(colorToMove:int) =
        let HIDDEN = 256
        let QA = 255
        let QB = 64
        let QAB = QA * QB
        let CR_MIN = 0s
        let CR_MAX = int16(1 * QA)
        let SCALE = 400
        let accumulatorA = NNUEout.AccumulatorA.[NNUEout.CurrentAccumulator]
        let accumulatorB = NNUEout.AccumulatorB.[NNUEout.CurrentAccumulator]
        if (colorToMove = White) then
            NN.ClippedReLUFlattenAndForward(accumulatorA, accumulatorB, NNUEin.FeatureBias, NNUEin.OutWeight, NNUEout.Output, CR_MIN, CR_MAX, HIDDEN)
        else
            NN.ClippedReLUFlattenAndForward(accumulatorB, accumulatorA, NNUEin.FeatureBias, NNUEin.OutWeight, NNUEout.Output, CR_MIN, CR_MAX, HIDDEN)
        (NNUEout.Output.[0] + int(NNUEin.OutBias.[0])) * SCALE / QAB
    
    
    
