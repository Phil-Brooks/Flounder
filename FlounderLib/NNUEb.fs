﻿namespace FlounderLib
open System.Reflection
open System.IO
open System

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
    let MoveRequiresRefresh(piece:int, from:int, mto:int) =
        let pc,_ = ColPiece.ToPcCol(piece)
        if pc<>Piece.King then false
        elif (from &&& 4) <> (mto &&& 4) then true
        else KING_BUCKETS.[from]<>KING_BUCKETS.[mto]
    let FeatureIdx(colpc:int, sq:Square, kingsq:Square, view:int) =
        let oP = 6 * ((int(colpc) ^^^ view) &&& 0x1) + int(colpc)/2
        let oK = (7 * if (int(kingsq) &&& 4) = 0 then 1 else 0) ^^^ (56 * view) ^^^ int(kingsq)
        let oSq = (7 * if (int(kingsq) &&& 4) = 0 then 1 else 0) ^^^ (56 * view) ^^^ int(sq)
        KING_BUCKETS.[oK] * 12 * 64 + oP * 64 + oSq
    let ApplySubSubAddAdd(src:int16 array, f1:int, f2:int, f3:int, f4:int, view:int) =
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
            let o4 = f4 * 768 + unrollOffset
            for i = 0 to 15 do
                regs.[i] <- regs.[i] + NNUEin.InputWeights.[o4 + i]
            for i = 0 to 15 do
                Accumulators.[AccIndex].AccValues.[view].[unrollOffset+i] <- regs.[i]
    let ApplySubSubAdd(src:int16 array, f1:int, f2:int, f3:int, view:int) =
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
                Accumulators.[AccIndex].AccValues.[view].[unrollOffset+i] <- regs.[i]
    let ApplySubAdd(src:int16 array, f1:int, f2:int, view:int) =
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
                Accumulators.[AccIndex].AccValues.[view].[unrollOffset+i] <- regs.[i]
    let ApplyUpdates(map:BitBoardMap, move:RevertMove, view:int) =
        let captured = 
            if move.EnPassant then (if map.stm=1 then BlackPawn else WhitePawn)
            else ColPiece.FromPcCol(move.CapturedPiece,move.CapturedColor)
        let prev = Accumulators.[AccIndex-1].AccValues.[int(view)]
        let king = map.[Piece.King, view].ToSq()
        let movingSide = move.ColorToMove
        let colpcto = map.[move.To]
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
            ApplySubSubAddAdd(prev, from, rookFrom, mto, rookTo, view)
        //IsCap
        elif captured <> EmptyColPc then
            let capSq = 
                if move.EnPassant && movingSide=0 then Square.FromInt(int(move.To) + 8)
                elif move.EnPassant then Square.FromInt(int(move.To) - 8)
                else move.To
            let capturedTo = FeatureIdx(captured, capSq, king, view)
            ApplySubSubAdd(prev, from, capturedTo, mto, view)
        else
            ApplySubAdd(prev, from, mto, view)    
    let ApplyDelta(src:int16 array, delta:Delta, perspective:int) =
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
                Accumulators.[AccIndex].AccValues.[perspective].[unrollOffset+i] <- regs.[i]
    let ResetAccumulator(map:BitBoardMap,perspective:int) =
        let delta= Delta.Default()
        let kingSq = map.[Piece.King, perspective].ToSq()
        let occupied = ~~~(map.[2])
        let mutable sqIterator = occupied.GetEnumerator()
        let mutable sq = sqIterator.Current
        while (sqIterator.MoveNext()) do
            let colpc = map.PiecesAndColors.[int(sq)]
            delta.add.[delta.a] <- FeatureIdx(colpc,sq,kingSq,perspective)
            sq <- sqIterator.Current
            delta.a <- delta.a + 1
        let src = Array.copy NNUEin.InputBiases
        ApplyDelta(src,delta,perspective)
    let RefreshAccumulator(map:BitBoardMap,perspective:int) =
        let delta = Delta.Default()
        let kingSq = map.[Piece.King, perspective].ToSq()
        let pBucket = if perspective = 0 then 0 else 32
        let kingBucket = KING_BUCKETS.[int(kingSq) ^^^ (56 * perspective)] + (if Square.ToFile(kingSq) > 3 then 16 else 0)
        let state = RefreshTable.[pBucket + kingBucket]
        for pc = WhitePawn to BlackKing do
            let curr = map.Pieces.[pc]
            let prev = state.Pcs.[pc] 
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
        RefreshTable.[pBucket + kingBucket] <- {state with AccKsValues = Array.copy Accumulators.[AccIndex].AccValues.[int(perspective)]}
    let DoUpdate(map:BitBoardMap, move:RevertMove) =
        let from = 
            if map.stm=1 then int(move.From)
            else int(move.From)^^^56
        let mto = 
            if map.stm=1 then int(move.To)
            else int(move.To)^^^56
        let colpcto = map.[move.To]
        let colpcfrom =
            if move.Promotion then ColPiece.FromPcCol(Piece.Pawn,map.xstm)
            else colpcto
        if MoveRequiresRefresh(colpcfrom, from, mto) then
            RefreshAccumulator(map, map.xstm)
            ApplyUpdates(map, move, map.stm)
        else
            ApplyUpdates(map, move, 0)
            ApplyUpdates(map, move, 1)
    let OutputLayer(stm:int) =
        let mutable result = NNUEin.OutputBias
        for c = 0 to 767 do
           result <- result + Math.Max(int(Accumulators.[AccIndex].AccValues.[stm].[c]), 0) * int(NNUEin.OutputWeights.[c])
        let xstm = stm^^^1
        for c = 0 to 767 do
           result <- result + Math.Max(int(Accumulators.[AccIndex].AccValues.[xstm].[c]), 0) * int(NNUEin.OutputWeights.[c + 768])
        result/8192