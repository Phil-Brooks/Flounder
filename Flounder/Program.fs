open Flounder
open FlounderLib
open System
open System.IO

[<EntryPoint>]
let main argv =
    AttackTable1.SetUp()
    Zobrist.Setup()
    NNUEb.ResetRefreshTable()
    let command = Environment.CommandLine
    if (command.ToLower().Contains("test")) then
        Tester.Test(argv)
        0
    else
        Console.WriteLine("Flounder v" + VersionNo)
        let mutable requiredInterface = ""
        let rec MainInterface() =
            requiredInterface <- Console.ReadLine()
            if requiredInterface="uci" then
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