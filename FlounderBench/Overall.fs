namespace FlounderBench
open FlounderLib
open BenchmarkDotNet.Attributes
open System.IO
open Newtonsoft.Json

type Overall() =

    let Default = Board.Default()
    let path = @"D:\Github\Flounder\FlounderLib"
    let NNUE_FILE = "BasicNNUE"
    let HASH = "0334adf934"
    let inf = Path.Combine(path, NNUE_FILE + "f-" + HASH + ".nnue.json")
    let txt = File.ReadAllText(inf)
    let basicNNUE = JsonConvert.DeserializeObject<BasicNNUE>(txt)
    
    [<GlobalSetup>]
    member _.Setup() =
        AttackTable1.SetUp()
        BlackMagicBitBoardFactory.SetUp()
     
    [<Benchmark>]
    member _.Overall() = 
        FlounderLib.Perft.MoveGeneration(Default, 4)|>ignore
        basicNNUE.RefreshAccumulator(Default.Map)
        basicNNUE.Evaluate(Default.ColorToMove)|>ignore

