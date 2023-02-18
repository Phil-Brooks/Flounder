namespace FlounderLib
open System.Reflection
open Newtonsoft.Json 
open System.IO

[<AutoOpen>]
module NNUE_Load =
    let NNUE = 
        let NNUE_FILE = "FlounderLib.BasicNNUEf"
        let HASH = "0334adf934"
        let resource = NNUE_FILE + "-" + HASH + ".nnue.json"
        use stream = Assembly.GetExecutingAssembly().GetManifestResourceStream(resource)
        use reader = new StreamReader(stream)
        let ans = JsonConvert.DeserializeObject<BasicNNUE>(reader.ReadToEnd())
        ans
