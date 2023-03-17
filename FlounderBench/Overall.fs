namespace FlounderBench
open FlounderLib
open BenchmarkDotNet.Attributes
open System
open System.IO

type Overall() =

    let Default = Board.Default()
    let basicNNUE = 
        let path = @"D:\Github\Flounder\FlounderLib"
        let NNUE_FILE = "BasicNNUEf"
        let HASH = "0334adf934"
        let loadtxt str =
            let pref = NNUE_FILE + "-" + HASH + "-"
            let fl= Path.Combine(path,pref + str) //e.g "FeatureBias.txt"
            let txt = File.ReadAllText(fl)
            let nl = Environment.NewLine
            txt.Split(nl.ToString(), StringSplitOptions.RemoveEmptyEntries)
            |>Array.map(fun s -> int16(s))
        let fb = loadtxt "FeatureBias.txt" 
        let fw = loadtxt "FeatureWeight.txt" 
        let ffw = loadtxt "FlippedFeatureWeight.txt" 
        let ob = loadtxt "OutBias.txt" 
        let ow = loadtxt "OutWeight.txt" 
        let nnue =
            let acca = 
                let ans = Array.zeroCreate 128
                ans|>Array.map (fun a -> Array.zeroCreate 256)
            let accb = 
                let ans = Array.zeroCreate 128
                ans|>Array.map (fun a -> Array.zeroCreate 256)
            {
                Output = [| -1072 |]
                CurrentAccumulator = 0
                FeatureWeight = fw
                FlippedFeatureWeight = ffw
                FeatureBias = fb
                OutWeight = ow
                OutBias = ob
                WhitePOV = Array.zeroCreate 768 
                BlackPOV = Array.zeroCreate 768
                AccumulatorA = acca
                AccumulatorB = accb
                Flatten = Array.zeroCreate 512
            }
        nnue
    
    
    
    
    [<GlobalSetup>]
    member _.Setup() =
        AttackTable1.SetUp()
        BlackMagicBitBoardFactory.SetUp()
     
    [<Benchmark>]
    member _.Overall() = 
        FlounderLib.Perft.MoveGeneration(Default, 4)|>ignore
        basicNNUE.RefreshAccumulator(Default.Map)
        basicNNUE.Evaluate(Default.ColorToMove)|>ignore

