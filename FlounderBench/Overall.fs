namespace FlounderBench
open FlounderLib
open BenchmarkDotNet.Attributes

type Overall() =

    let mutable Default = Board.Default()
    
    [<GlobalSetup>]
    member _.Setup() =
        ()
     
    [<Benchmark>]
    member _.Overall() = 
        FlounderLib.Perft.MoveGeneration(&Default, 4)|>ignore
        NNUEb.ResetAccumulator(Default,0)
        NNUEb.ResetAccumulator(Default,1)
        NNUEb.OutputLayer(Default)|>ignore

