namespace FlounderLib
open System.Reflection
open System.IO
open System
open System.Numerics
open System.Runtime.Intrinsics
open System.Runtime.Intrinsics.X86

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
            iw[i] <- reader.ReadInt16()
        let iblen = 768
        let ib = Array.zeroCreate iblen
        for i = 0 to iblen-1 do
            ib[i] <- reader.ReadInt16()
        let owlen = 1536
        let ow = Array.zeroCreate owlen
        for i = 0 to owlen-1 do
            ow[i] <- reader.ReadInt16()
        let ob = reader.ReadInt32()
        let nnue =
            {
                InputWeights = iw
                InputBiases = ib
                OutputWeights = ow
                OutputBias = ob
            }
        nnue
    let Accumulators:int16 array array array =
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
    let ApplySubSubAddAdd(src:int16 array, f1:int, f2:int, f3:int, f4:int, view:int) =
        let o1 = f1 * 768
        let o2 = f2 * 768
        let o3 = f3 * 768
        let o4 = f4 * 768
        let regs = Accumulators.[AccIndex].[view]
        let chunkSize = Vector<int16>.Count
        let rec fast (i:int) =
            if i > 768 - chunkSize then slow i
            else
                let VecS = Vector(src, i)
                let Vec1 = Vector(NNUEin.InputWeights, o1 + i)
                let Vec2 = Vector(NNUEin.InputWeights, o2 + i)
                let Vec3 = Vector(NNUEin.InputWeights, o3 + i)
                let Vec4 = Vector(NNUEin.InputWeights, o4 + i)
                let VecAns = VecS - Vec1 - Vec2 + Vec3 + Vec4
                VecAns.CopyTo(regs, i)
                fast (i + chunkSize)
        and slow (i:int) =
            if i < 768 then
                regs[i] <- src[i] - NNUEin.InputWeights[o1 + i] - NNUEin.InputWeights[o2 + i] + NNUEin.InputWeights[o3 + i] + NNUEin.InputWeights[o4 + i]
                slow (i + 1)
        fast 0
    let ApplySubSubAdd(src:int16 array, f1:int, f2:int, f3:int, view:int) =
        let o1 = f1 * 768
        let o2 = f2 * 768
        let o3 = f3 * 768
        let regs = Accumulators.[AccIndex].[view]
        let chunkSize = Vector<int16>.Count
        let rec fast (i:int) =
            if i > 768 - chunkSize then slow i
            else
                let VecS = Vector(src, i)
                let Vec1 = Vector(NNUEin.InputWeights, o1 + i)
                let Vec2 = Vector(NNUEin.InputWeights, o2 + i)
                let Vec3 = Vector(NNUEin.InputWeights, o3 + i)
                let VecAns = VecS - Vec1 - Vec2 + Vec3
                VecAns.CopyTo(regs, i)
                fast (i + chunkSize)
        and slow (i:int) =
            if i < 768 then
                regs[i] <- src[i] - NNUEin.InputWeights[o1 + i] - NNUEin.InputWeights[o2 + i] + NNUEin.InputWeights[o3 + i]
                slow (i + 1)
        fast 0
    let ApplySubAdd(src:int16 array, f1:int, f2:int, view:int) =
        let o1 = f1 * 768
        let o2 = f2 * 768
        let regs = Accumulators.[AccIndex].[view]
        let chunkSize = Vector<int16>.Count
        let rec fast (i:int) =
            if i > 768 - chunkSize then slow i
            else
                let VecS = Vector(src, i)
                let Vec1 = Vector(NNUEin.InputWeights, o1 + i)
                let Vec2 = Vector(NNUEin.InputWeights, o2 + i)
                let VecAns = VecS - Vec1 + Vec2
                VecAns.CopyTo(regs, i)
                fast (i + chunkSize)
        and slow (i:int) =
            if i < 768 then
                regs[i] <- src[i] - NNUEin.InputWeights[o1 + i] + NNUEin.InputWeights[o2 + i]
                slow (i + 1)
        fast 0
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
    let ApplyDelta(src:int16 array, delta:DeltaRec, perspective:int) =
        let regs = Accumulators.[AccIndex].[perspective]
        for i = 0 to 767 do
            regs[i] <- src[i]
            for r = 0 to delta.r-1 do
                let offset = delta.rem[r] * 768
                regs[i] <- regs[i] - NNUEin.InputWeights[offset + i]
            for a = 0 to delta.a-1 do
                let offset = delta.add[a] * 768
                regs[i] <- regs[i] + NNUEin.InputWeights[offset + i]
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
    let MultiplyAddAdjacent(a:Vector<int16>, b:Vector<int16>) =
        let SoftwareFallback() =
            failwith "No"
            let a1,a2 = Vector.Widen(a)
            let b1,b2 = Vector.Widen(b)
            let c1 = a1 * b1
            let c2 = a2 * b2
            c1 + c2
        if (Avx.IsSupported) then
            if (Avx2.IsSupported) then
                let one = a.AsVector256()
                let two = b.AsVector256()
                Avx2.MultiplyAddAdjacent(one, two).AsVector()
            else
                SoftwareFallback()
        else
            SoftwareFallback()
    let OutputLayer() =
        let mutable result = NNUEin.OutputBias
        let AccS = Accumulators.[AccIndex].[Brd.Stm]
        let AccX = Accumulators.[AccIndex].[Brd.Xstm]
        let chunkSize = Vector<int16>.Count
        let rec fast (i:int) =
            if i > 768 - chunkSize then slow i
            else
                let VecS = Vector.Max(Vector(AccS, i), Vector.Zero)
                let VecX = Vector.Max(Vector(AccX, i), Vector.Zero)
                let VecWtS = Vector(NNUEin.OutputWeights, i)
                let VecWtX = Vector(NNUEin.OutputWeights, i + 768)
                let VecAnsS = MultiplyAddAdjacent(VecS, VecWtS)
                let VecAnsX = MultiplyAddAdjacent(VecX, VecWtX)
                let VecAns = VecAnsS + VecAnsX
                result <- result + Vector.Sum(VecAns)
                fast (i + chunkSize)
        and slow (i:int) =
            if i < 768 then
                result <- result + int(Math.Max(AccS[i], 0s)) * int(NNUEin.OutputWeights[i])
                                 + int(Math.Max(AccX[i], 0s)) * int(NNUEin.OutputWeights[i + 768])
                slow (i + 1)
        fast 0
        result/8192
