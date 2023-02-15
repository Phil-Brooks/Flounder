open FlounderBench
open BenchmarkDotNet.Running

[<EntryPoint>]
let main argv =
    BenchmarkRunner.Run<Overall>() |> ignore
    0 // return an integer exit code