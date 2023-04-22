namespace FlounderLib
open System.Reflection
open System.IO
open System

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
            iw[i] <- int(reader.ReadInt16())
        let iblen = 768
        let ib = Array.zeroCreate iblen
        for i = 0 to iblen-1 do
            ib[i] <- int(reader.ReadInt16())
        let owlen = 1536
        let ow = Array.zeroCreate owlen
        for i = 0 to owlen-1 do
            ow[i] <- int(reader.ReadInt16())
        let ob = reader.ReadInt32()
        let nnue =
            {
                InputWeights = iw
                InputBiases = ib
                OutputWeights = ow
                OutputBias = ob
            }
        nnue
    let Accumulators:int array array array =
        let ans = Array.zeroCreate 252
        for i = 0 to 251 do
            let accs = 
                let ans = Array.zeroCreate 2
                ans|>Array.map (fun a -> Array.zeroCreate 768)
            ans[i] <- accs
        ans
    let mutable AccIndex = 0
    let RefreshTable:AccKingStateRec array =
        let ans = Array.zeroCreate 64
        for i = 0 to 63 do
            let acci =
                {
                    AccKsValues = Array.zeroCreate 768
                    Pcs = Array.zeroCreate 12
                }
            ans[i] <- acci
        ans
    let ResetRefreshTable() =
        for i = 0 to 63 do
            let acci =
                {
                    AccKsValues = Array.copy NNUEin.InputBiases
                    Pcs = Array.zeroCreate 12
                }
            RefreshTable[i] <- acci
    let MoveRequiresRefresh(piece:int, from:int, mto:int) =
        let pc = piece/2
        if pc <> King then false
        elif (from &&& 4) <> (mto &&& 4) then true
        else KING_BUCKETS[from] <> KING_BUCKETS[mto]
    let FeatureIdx(colpc:int, sq:int, kingsq:int, view:int) =
        let oP = 6 * ((colpc ^^^ view) &&& 0x1) + colpc/2
        let oK = (7 * if (kingsq &&& 4) = 0 then 1 else 0) ^^^ (56 * view) ^^^ kingsq
        let oSq = (7 * if (kingsq &&& 4) = 0 then 1 else 0) ^^^ (56 * view) ^^^ sq
        KING_BUCKETS[oK] * 12 * 64 + oP * 64 + oSq
    let ApplySubSubAddAdd(src:int array, f1:int, f2:int, f3:int, f4:int, view:int) =
        let regs:int array = Array.zeroCreate 16
        for c = 0 to 768/16-1 do
            let unrollOffset = c * 16
            for i = 0 to 15 do
                regs[i] <- src[i+unrollOffset]
            let o1 = f1 * 768 + unrollOffset
            for i = 0 to 15 do
                regs[i] <- regs[i] - NNUEin.InputWeights[o1 + i]
            let o2 = f2 * 768 + unrollOffset
            for i = 0 to 15 do
                regs[i] <- regs[i] - NNUEin.InputWeights[o2 + i]
            let o3 = f3 * 768 + unrollOffset
            for i = 0 to 15 do
                regs[i] <- regs[i] + NNUEin.InputWeights[o3 + i]
            let o4 = f4 * 768 + unrollOffset
            for i = 0 to 15 do
                regs[i] <- regs[i] + NNUEin.InputWeights[o4 + i]
            for i = 0 to 15 do
                Accumulators.[AccIndex].[view].[unrollOffset+i] <- regs[i]
    let ApplySubSubAdd(src:int array, f1:int, f2:int, f3:int, view:int) =
        let regs:int array = Array.zeroCreate 16
        for c = 0 to 768/16-1 do
            let unrollOffset = c * 16
            for i = 0 to 15 do
                regs[i] <- src[i+unrollOffset]
            let o1 = f1 * 768 + unrollOffset
            for i = 0 to 15 do
                regs[i] <- regs[i] - NNUEin.InputWeights[o1 + i]
            let o2 = f2 * 768 + unrollOffset
            for i = 0 to 15 do
                regs[i] <- regs[i] - NNUEin.InputWeights[o2 + i]
            let o3 = f3 * 768 + unrollOffset
            for i = 0 to 15 do
                regs[i] <- regs[i] + NNUEin.InputWeights[o3 + i]
            for i = 0 to 15 do
                Accumulators.[AccIndex].[view].[unrollOffset+i] <- regs[i]
    let ApplySubAdd(src:int array, f1:int, f2:int, view:int) =
        let regs:int array = Array.zeroCreate 16
        for c = 0 to 768/16-1 do
            let unrollOffset = c * 16
            for i = 0 to 15 do
                regs[i] <- src[i+unrollOffset]
            let o1 = f1 * 768 + unrollOffset
            for i = 0 to 15 do
                regs[i] <- regs[i] - NNUEin.InputWeights[o1 + i]
            let o2 = f2 * 768 + unrollOffset
            for i = 0 to 15 do
                regs[i] <- regs[i] + NNUEin.InputWeights[o2 + i]
            for i = 0 to 15 do
                Accumulators.[AccIndex].[view].[unrollOffset+i] <- regs[i]
    let ApplyUpdates(move:MoveRec, view:int) =
        let captured = 
            if move.EnPassant then Brd.Stm
            else move.CapturedPiece
        let prev = Accumulators.[AccIndex-1].[view]
        let king = Bits.ToInt(if view = White then Brd.Pieces[WhiteKing] else Brd.Pieces[BlackKing])
        let movingSide = Brd.Xstm
        let colpcto = Brd.Squares[move.To]
        let colpcfrom =
            if move.Promotion then movingSide
            else colpcto
        let from = FeatureIdx(colpcfrom, move.From, king, view)
        let mto = FeatureIdx(colpcto, move.To, king, view)
        //IsCas
        if move.SecondaryFrom <> Na then
            let colpcrook =  ColPiece.FromPcCol(Rook,movingSide)
            let rookFrom = FeatureIdx(colpcrook, move.SecondaryFrom, king, view)
            let rookTo = FeatureIdx(colpcrook, move.SecondaryTo, king, view)
            ApplySubSubAddAdd(prev, from, rookFrom, mto, rookTo, view)
        //IsCap
        elif captured <> EmptyColPc then
            let capSq = 
                if move.EnPassant && movingSide = White then move.To + 8
                elif move.EnPassant then move.To - 8
                else move.To
            let capturedTo = FeatureIdx(captured, capSq, king, view)
            ApplySubSubAdd(prev, from, capturedTo, mto, view)
        else
            ApplySubAdd(prev, from, mto, view)    
    let ApplyDelta(src:int array, delta:DeltaRec, perspective:int) =
        let regs:int array = Array.zeroCreate 16
        for c = 0 to 768/16-1 do
            let unrollOffset = c * 16
            for i = 0 to 15 do
                regs[i] <- src[i+unrollOffset]
            for r = 0 to delta.r-1 do
                let offset = delta.rem[r] * 768 + unrollOffset
                for i = 0 to 15 do
                    regs[i] <- regs[i] - NNUEin.InputWeights[offset + i]
            for a = 0 to delta.a-1 do
                let offset = delta.add[a] * 768 + unrollOffset
                for i = 0 to 15 do
                    regs[i] <- regs[i] + NNUEin.InputWeights[offset + i]
            for i = 0 to 15 do
                Accumulators.[AccIndex].[perspective].[unrollOffset+i] <- regs[i]
    let ResetAccumulator(perspective:int) =
        let mutable delta = Delta.Default()
        let kingSq = Bits.ToInt(if perspective = White then Brd.Pieces[WhiteKing] else Brd.Pieces[BlackKing])
        let occupied = Brd.Both
        let sqarr = Bits.ToArray(occupied)
        for sq in sqarr do
            let colpc = Brd.Squares[sq]
            delta.add[delta.a] <- FeatureIdx(colpc,sq,kingSq,perspective)
            delta.a <- delta.a + 1
        let src = Array.copy NNUEin.InputBiases
        ApplyDelta(src,delta,perspective)
    let RefreshAccumulator(perspective:int) =
        let mutable delta = Delta.Default()
        let kingSq = Bits.ToInt(if perspective = White then Brd.Pieces[WhiteKing] else Brd.Pieces[BlackKing])
        let pBucket = if perspective = White then 0 else 32
        let kingBucket = KING_BUCKETS[kingSq ^^^ (56 * perspective)] + (if Square.ToFile(kingSq) > 3 then 16 else 0)
        let state = RefreshTable[pBucket + kingBucket]
        for pc = WhitePawn to BlackKing do
            let curr = Brd.Pieces[pc]
            let prev = state.Pcs[pc] 
            let rem = prev &&& ~~~curr
            let add = curr &&& ~~~prev
            let sqarr = Bits.ToArray(rem)
            for sq in sqarr do
                delta.rem[delta.r] <- FeatureIdx(pc,sq,kingSq,perspective)
                delta.r <- delta.r + 1
            let sqarr = Bits.ToArray(add)
            for sq in sqarr do
                delta.add[delta.a] <- FeatureIdx(pc,sq,kingSq,perspective)
                delta.a <- delta.a + 1
            state.Pcs[pc] <- curr
        ApplyDelta(state.AccKsValues, delta, perspective)
        RefreshTable[pBucket + kingBucket] <- {state with AccKsValues = Array.copy Accumulators.[AccIndex].[perspective]}
    let DoUpdate(move:MoveRec) =
        let from = 
            if not Brd.IsWtm then move.From
            else move.From ^^^ 56
        let mto = 
            if not Brd.IsWtm then move.To
            else move.To ^^^ 56
        let colpcto = Brd.Squares[move.To]
        let colpcfrom =
            if move.Promotion then ColPiece.FromPcCol(Pawn,Brd.Xstm)
            else colpcto
        if MoveRequiresRefresh(colpcfrom, from, mto) then
            RefreshAccumulator(Brd.Xstm)
            ApplyUpdates(move, Brd.Stm)
        else
            ApplyUpdates(move, White)
            ApplyUpdates(move, Black)
    let OutputLayer() =
        let mutable result = NNUEin.OutputBias
        for c = 0 to 767 do
           result <- result + Math.Max(Accumulators.[AccIndex].[Brd.Stm].[c], 0) * NNUEin.OutputWeights[c]
        for c = 0 to 767 do
           result <- result + Math.Max(Accumulators.[AccIndex].[Brd.Xstm].[c], 0) * NNUEin.OutputWeights[c + 768]
        result/8192