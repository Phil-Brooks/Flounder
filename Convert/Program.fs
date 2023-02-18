open FlounderLib
open System.IO
open Newtonsoft.Json

[<EntryPoint>]
let main argv =

    let Board = Board.Default()
    let path = @"D:\Github\Flounder\FlounderLib"
    let NNUE_FILE = "BasicNNUE"
    let HASH = "0334adf934"
    let inf = Path.Combine(path, NNUE_FILE + "f-" + HASH + ".nnue.json")
    let jsonString = File.ReadAllText(inf)

    let testbasicNNUE:BasicNNUE = JsonConvert.DeserializeObject<BasicNNUE>(jsonString)
    let ans1 = testbasicNNUE.Evaluate(Board.ColorToMove)
    //need to adjust object then write out.

    let jsonOut = JsonConvert.SerializeObject(testbasicNNUE)
    //File.WriteAllText(outf,jsonOut)


    0