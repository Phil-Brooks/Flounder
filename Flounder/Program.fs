open Flounder
open FlounderLib
open System
open System.IO

[<EntryPoint>]
let main argv =
    Util.RunStaticConstructor()        
        
    AttackTable1.SetUp()
    Zobrist.Setup()
        
    // Run JIT.
    Perft.MoveGeneration(Board1.Default(), 5) |>ignore

    let command = Environment.CommandLine

    if (command.ToLower().Contains("bench")) then
        OpenBenchBenchmark.Bench()
        0
    elif (command.ToLower().Contains("test")) then
        Tester.Test()
        0
    else
        Console.WriteLine("Flounder v" + Version.Current)
        let mutable requiredInterface = ""
        let rec MainInterface() =
            requiredInterface <- Console.ReadLine()
            if requiredInterface="uci" then
                //TODO
                let standardOutput = new StreamWriter(Console.OpenStandardOutput())
                standardOutput.AutoFlush <- true
                Console.SetOut(standardOutput)
                UniversalChessInterface.Setup()
                UniversalChessInterface.LaunchUci()
            else
                Console.WriteLine("Please type uci")
                MainInterface()
        MainInterface()
        0 // return an integer exit code