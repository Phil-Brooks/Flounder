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
        NNUE.ResetAccumulator()
        NNUE.RefreshAccumulator()
        NNUE.Evaluate(Brd.Stm)|>ignore

