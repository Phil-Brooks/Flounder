namespace FlounderLib
open System
open System.Runtime.CompilerServices

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
