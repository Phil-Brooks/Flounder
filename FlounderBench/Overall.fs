namespace FlounderBench
open FlounderLib
open BenchmarkDotNet.Attributes

type Overall() =

    [<GlobalSetup>]
    member _.Setup() =
        ()
     
    [<Benchmark>]
    member _.Overall() = 
        EngBoard.Default()
        Perft.MoveGeneration(4)|>ignore
        NNUEb.ResetAccumulator(0)
        NNUEb.ResetAccumulator(1)
        NNUEb.OutputLayer()|>ignore

