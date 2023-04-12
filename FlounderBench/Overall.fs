namespace FlounderBench
open FlounderLib
open BenchmarkDotNet.Attributes
open System
open System.IO

type Overall() =

    let Default = Board.Default()
    
    [<GlobalSetup>]
    member _.Setup() =
        AttackTable1.SetUp()
        BlackMagicBitBoardFactory.SetUp()
     
    [<Benchmark>]
    member _.Overall() = 
        FlounderLib.Perft.MoveGeneration(Default, 4)|>ignore
        NNUEb.ResetAccumulator(Default.Map,0)
        NNUEb.ResetAccumulator(Default.Map,1)
        NNUEb.OutputLayer(Default.Map)|>ignore

