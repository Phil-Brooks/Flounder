namespace FlounderLib
open System.Numerics
open System.Runtime.CompilerServices
open System
open System.Runtime.Intrinsics
open System.Runtime.Intrinsics.X86

module VSize =
    let Int = Vector<int>.Count
    let Short = Vector<int16>.Count

module NN =
    let inline SoftwareFallback(a:Vector<int16>, b:Vector<int16>) =
        let a1,a2 = Vector.Widen(a)
        let b1,b2 = Vector.Widen(b)
        a1 * b1 + a2 * b2
    let MultiplyAddAdjacent(a:Vector<int16>, b:Vector<int16>) =
        if (Avx.IsSupported) then
            if (Avx2.IsSupported) then
                let one = a.AsVector256()
                let two = b.AsVector256()
                Avx2.MultiplyAddAdjacent(one, two).AsVector()
            else
                SoftwareFallback(a,b)
        else
            SoftwareFallback(a,b)
    let Clamp(value:Vector<int16>, min:byref<Vector<int16>>, max:byref<Vector<int16>>) =
        Vector.Max(min, Vector.Min(max, value))
    let ToArray(vector:Vector<int16>, array:int16 array, offset:int) =
        Unsafe.WriteUnaligned(&Unsafe.As<int16, byte>(&array.[offset]), vector)
    let LoadVector(values:int16 array, index:int) =
        Unsafe.ReadUnaligned<Vector<int16>>(&Unsafe.As<int16, byte>(&values.[index]))
    let UNROLL = 4
    let Forward(input:int16 array, weight:int16 array, output:int16 array) =
        let offset = 0
        let inputSize = input.Length
        let loopSize = inputSize / VSize.Short / UNROLL
        let outputSize = output.Length
        let mutable weightStride = 0
        for i = 0 to outputSize-1 do
            let mutable sum = Vector<int16>.Zero
            let mutable vectorIndex = 0
            for j = 0 to loopSize-1 do
                let unrolledIndex = vectorIndex + VSize.Short
                let unrolledIndex2 = unrolledIndex + VSize.Short
                let unrolledIndex3 = unrolledIndex2 + VSize.Short
                let iVec = LoadVector(input, vectorIndex)
                let wVec = LoadVector(weight, weightStride + vectorIndex)
                sum <- sum + iVec * wVec
                let iVec2 = LoadVector(input, unrolledIndex)
                let wVec2 = LoadVector(weight, weightStride + unrolledIndex)
                sum <- sum + iVec2 * wVec2
                let iVec3 = LoadVector(input, unrolledIndex2)
                let wVec3 = LoadVector(weight, weightStride + unrolledIndex2)
                sum <- sum + iVec3 * wVec3
                let iVec4 = LoadVector(input, unrolledIndex3)
                let wVec4 = LoadVector(weight, weightStride + unrolledIndex3)
                sum <- sum + iVec4 * wVec4
                vectorIndex <- unrolledIndex3 + VSize.Short
            output.[offset + i] <- Vector.Sum(sum)
            weightStride <- weightStride + inputSize
    let ClippedReLUFlattenAndForward(inputA:int16 array, inputB:int16 array, bias:int16 array, weight:int16 array,output:int array, min:int16, max:int16, separationIndex:int) =
        let offset = 0
        let inputSize = inputA.Length + inputB.Length
        let loopSize = inputSize / VSize.Short / UNROLL
        let outputSize = output.Length
        let mutable weightStride = 0
        let mutable minVec = Vector<int16>(min)
        let mutable maxVec = Vector<int16>(max)
        let InputReference(index) = if index < separationIndex then inputA else inputB
        let RelativeIndex(index) = if index < separationIndex then index else index - separationIndex
        for i = 0 to outputSize-1 do
            let mutable sum = Vector<int>.Zero
            let mutable vectorIndex = 0
            for j = 0 to loopSize-1 do
                let input = InputReference(vectorIndex)
                let unrolledIndex = vectorIndex + VSize.Short
                let unrolledIndex2 = unrolledIndex + VSize.Short
                let unrolledIndex3 = unrolledIndex2 + VSize.Short
                let rIndex = RelativeIndex(vectorIndex)
                let unrolledRIndex = rIndex + VSize.Short
                let unrolledRIndex2 = unrolledRIndex + VSize.Short
                let unrolledRIndex3 = unrolledRIndex2 + VSize.Short
                let iVec = LoadVector(input, rIndex)
                let bVec = LoadVector(bias,rIndex)
                let wVec = LoadVector(weight, weightStride + vectorIndex)
                let clamped = Clamp(iVec + bVec, &minVec, &maxVec)
                sum <- sum + MultiplyAddAdjacent(clamped, wVec)
                let iVec2 = LoadVector(input, unrolledRIndex)
                let bVec2 = LoadVector(bias,unrolledRIndex)
                let wVec2 = LoadVector(weight, weightStride + unrolledIndex)
                let clamped2 = Clamp(iVec2 + bVec2, &minVec, &maxVec)
                sum <- sum + MultiplyAddAdjacent(clamped2, wVec2)
                let iVec3 = LoadVector(input, unrolledRIndex2)
                let bVec3 = LoadVector(bias,unrolledRIndex2)
                let wVec3 = LoadVector(weight, weightStride + unrolledIndex2)
                let clamped3 = Clamp(iVec3 + bVec3, &minVec, &maxVec)
                sum <- sum + MultiplyAddAdjacent(clamped3, wVec3)
                let iVec4 = LoadVector(input, unrolledRIndex3)
                let bVec4 = LoadVector(bias,unrolledRIndex3)
                let wVec4 = LoadVector(weight, weightStride + unrolledIndex3)
                let clamped4 = Clamp(iVec4 + bVec4, &minVec, &maxVec)
                sum <- sum + MultiplyAddAdjacent(clamped4, wVec4)
                vectorIndex <- unrolledIndex3 + VSize.Short
            output.[offset + i] <- Vector.Sum(sum)
            weightStride <- weightStride + inputSize
    let AddToAll(inputA:int16 array, inputB:int16 array, delta:int16 array, offsetA:int, offsetB:int) =
        let size = inputA.Length
        let loopSize = size / VSize.Short / UNROLL
        let mutable vectorIndex = 0
        for i = 0 to loopSize-1 do
            let unrolledIndex = vectorIndex + VSize.Short
            let unrolledIndex2 = unrolledIndex + VSize.Short
            let unrolledIndex3 = unrolledIndex2 + VSize.Short
            let iAVec = LoadVector(inputA, vectorIndex)
            let dAVec = LoadVector(delta, offsetA + vectorIndex)
            let rAVec = iAVec + dAVec
            ToArray(rAVec,inputA, vectorIndex)
            let iAVec2 = LoadVector(inputA, unrolledIndex)
            let dAVec2 = LoadVector(delta, offsetA + unrolledIndex)
            let rAVec2 = iAVec2 + dAVec2
            ToArray(rAVec2,inputA, unrolledIndex)
            let iAVec3 = LoadVector(inputA, unrolledIndex2)
            let dAVec3 = LoadVector(delta, offsetA + unrolledIndex2)
            let rAVec3 = iAVec3 + dAVec3
            ToArray(rAVec3,inputA, unrolledIndex2)
            let iAVec4 = LoadVector(inputA, unrolledIndex3)
            let dAVec4 = LoadVector(delta, offsetA + unrolledIndex3)
            let rAVec4 = iAVec4 + dAVec4
            ToArray(rAVec4,inputA, unrolledIndex3)
            let iBVec = LoadVector(inputB,vectorIndex)
            let dBVec = LoadVector(delta, offsetB + vectorIndex)
            let rBVec = iBVec + dBVec
            ToArray(rBVec,inputB, vectorIndex)
            let iBVec2 = LoadVector(inputB,unrolledIndex)
            let dBVec2 = LoadVector(delta, offsetB + unrolledIndex)
            let rBVec2 = iBVec2 + dBVec2
            ToArray(rBVec2,inputB, unrolledIndex)
            let iBVec3 = LoadVector(inputB,unrolledIndex2)
            let dBVec3 = LoadVector(delta, offsetB + unrolledIndex2)
            let rBVec3 = iBVec3 + dBVec3
            ToArray(rBVec3,inputB, unrolledIndex2)
            let iBVec4 = LoadVector(inputB,unrolledIndex3)
            let dBVec4 = LoadVector(delta, offsetB + unrolledIndex3)
            let rBVec4 = iBVec4 + dBVec4
            ToArray(rBVec4,inputB, unrolledIndex3)
            vectorIndex <- unrolledIndex3 + VSize.Short
    let SubtractFromAll(inputA:int16 array, inputB:int16 array, delta:int16 array, offsetA:int, offsetB:int) =
        let size = inputA.Length
        let loopSize = size / VSize.Short / UNROLL
        let mutable vectorIndex = 0
        for i = 0 to loopSize-1 do
            let unrolledIndex = vectorIndex + VSize.Short
            let unrolledIndex2 = unrolledIndex + VSize.Short
            let unrolledIndex3 = unrolledIndex2 + VSize.Short
            let iAVec = LoadVector(inputA, vectorIndex)
            let dAVec = LoadVector(delta, offsetA + vectorIndex)
            let rAVec = iAVec - dAVec
            ToArray(rAVec,inputA, vectorIndex)
            let iAVec2 = LoadVector(inputA, unrolledIndex)
            let dAVec2 = LoadVector(delta, offsetA + unrolledIndex)
            let rAVec2 = iAVec2- dAVec2
            ToArray(rAVec2,inputA, unrolledIndex)
            let iAVec3 = LoadVector(inputA, unrolledIndex2)
            let dAVec3 = LoadVector(delta, offsetA + unrolledIndex2)
            let rAVec3 = iAVec3 - dAVec3
            ToArray(rAVec3,inputA, unrolledIndex2)
            let iAVec4 = LoadVector(inputA, unrolledIndex3)
            let dAVec4 = LoadVector(delta, offsetA + unrolledIndex3)
            let rAVec4 = iAVec4 - dAVec4
            ToArray(rAVec4,inputA, unrolledIndex3)
            let iBVec = LoadVector(inputB,vectorIndex)
            let dBVec = LoadVector(delta, offsetB + vectorIndex)
            let rBVec = iBVec - dBVec
            ToArray(rBVec,inputB, vectorIndex)
            let iBVec2 = LoadVector(inputB,unrolledIndex)
            let dBVec2 = LoadVector(delta, offsetB + unrolledIndex)
            let rBVec2 = iBVec2 - dBVec2
            ToArray(rBVec2,inputB, unrolledIndex)
            let iBVec3 = LoadVector(inputB,unrolledIndex2)
            let dBVec3 = LoadVector(delta, offsetB + unrolledIndex2)
            let rBVec3 = iBVec3 - dBVec3
            ToArray(rBVec3,inputB, unrolledIndex2)
            let iBVec4 = LoadVector(inputB,unrolledIndex3)
            let dBVec4 = LoadVector(delta, offsetB + unrolledIndex3)
            let rBVec4 = iBVec4 - dBVec4
            ToArray(rBVec4,inputB, unrolledIndex3)
            vectorIndex <- unrolledIndex3 + VSize.Short
    let SubtractAndAddToAll(input:int16 array, delta:int16 array, offsetS:int, offsetA:int) =
        let size = input.Length
        let loopSize = size / VSize.Short / UNROLL
        let mutable vectorIndex = 0
        for i = 0 to loopSize-1 do
            let unrolledIndex = vectorIndex + VSize.Short
            let unrolledIndex2 = unrolledIndex + VSize.Short
            let unrolledIndex3 = unrolledIndex2 + VSize.Short
            let iVec = LoadVector(input, vectorIndex)
            let dAVec = LoadVector(delta, offsetA + vectorIndex)
            let dSVec = LoadVector(delta, offsetS + vectorIndex)
            let rVec = iVec - dSVec + dAVec
            ToArray(rVec,input, vectorIndex)
            let iVec2 = LoadVector(input, unrolledIndex)
            let dAVec2 = LoadVector(delta, offsetA + unrolledIndex)
            let dSVec2 = LoadVector(delta, offsetS + unrolledIndex)
            let rVec2 = iVec2 - dSVec2 + dAVec2
            ToArray(rVec2,input, unrolledIndex)
            let iVec3 = LoadVector(input, unrolledIndex2)
            let dAVec3 = LoadVector(delta, offsetA + unrolledIndex2)
            let dSVec3 = LoadVector(delta, offsetS + unrolledIndex2)
            let rVec3 = iVec3 - dSVec3 + dAVec3
            ToArray(rVec3,input, unrolledIndex2)
            let iVec4 = LoadVector(input, unrolledIndex3)
            let dAVec4 = LoadVector(delta, offsetA + unrolledIndex3)
            let dSVec4 = LoadVector(delta, offsetS + unrolledIndex3)
            let rVec4 = iVec4 - dSVec4 + dAVec4
            ToArray(rVec4,input, unrolledIndex3)
            vectorIndex <- unrolledIndex3 + VSize.Short

