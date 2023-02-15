open FlounderLib
open System.IO
open Newtonsoft.Json

[<EntryPoint>]
let main argv =

    let Board = Board1.Default()
    let path = @"D:\Github\Flounder\FlounderLib"
    let NNUE_FILE = "BasicNNUE"
    let HASH = "0334adf934"
    let inf = Path.Combine(path, NNUE_FILE + "f-" + HASH + ".nnue")
    //let streamf = File.OpenRead(inf)
    //let reader = BinaryFormatter()
    //let basicNNUE:BasicNNUEf = reader.Deserialize(streamf):?>BasicNNUEf
    //let ans = basicNNUE.Evaluate(Board.ColorToMove)
    //Can't Serialize to JSON as nothing works!!! Need to create a more standard class with 
    // tried member val Property1 = property1  but does not work!!!!!!!!!!!!!!!
    //let newbasicNNUE:BasicNNUE = basicNNUE.Convert()
    
    
    //let jsonString = JsonConvert.SerializeObject(newbasicNNUE)
    let outf = inf + ".json"
    //File.WriteAllText(outf,jsonString)

    //let testbasicNNUE:BasicNNUE = JsonConvert.DeserializeObject<BasicNNUE>(jsonString)
    //let ans1 = testbasicNNUE.Evaluate(Board.ColorToMove)
    0