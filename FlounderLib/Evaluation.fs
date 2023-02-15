namespace FlounderLib
open System.Reflection
open Newtonsoft.Json 
open System.IO

module Evaluation =
    let NNUE_FILE = "FlounderLib.BasicNNUEf"
    let HASH = "0334adf934"
    let NNUE = 
        let resource = NNUE_FILE + "-" + HASH + ".nnue.json"
        use stream = Assembly.GetExecutingAssembly().GetManifestResourceStream(resource)
        use reader = new StreamReader(stream)
        JsonConvert.DeserializeObject<BasicNNUE>(reader.ReadToEnd())
