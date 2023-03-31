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
        NNUE.RefreshAccumulator(Default.Map)
        NNUEb.ResetAccumulator(Default.Map,PieceColor.White)
        NNUEb.ResetAccumulator(Default.Map,PieceColor.Black)
        NNUE.Evaluate(Default.ColorToMove)|>ignore

