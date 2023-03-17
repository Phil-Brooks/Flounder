namespace FlounderLib
open System.Reflection
open System.IO
open System

[<AutoOpen>]
module NNUE_Load =
    let NNUE_FILE = "FlounderLib.BasicNNUEf"
    let HASH = "0334adf934"
    let loadtxt str =
        let pref = NNUE_FILE + "-" + HASH + "-"
        let resource = pref + str //e.g "FeatureBias.txt" 
        use stream = Assembly.GetExecutingAssembly().GetManifestResourceStream(resource)
        use reader = new StreamReader(stream)
        let nl = Environment.NewLine
        reader.ReadToEnd().Split(nl.ToString(), StringSplitOptions.RemoveEmptyEntries)
        |>Array.map(fun s -> int16(s))
    let NNUE = 
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
                Output = Array.zeroCreate 1
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
