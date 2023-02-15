namespace Flounder
open System
open System.Threading

module UciStdInputThread =

    let commandReceived = new Event<_>()
    let CommandReceived = commandReceived.Publish
    let mutable Running = false

    let StartAcceptingInput() =
        Running <- true

        while (Running) do
            let input = Console.ReadLine()
            if (input.ToLower().Equals("exit_hard")) then
                Running <- false
            else
                commandReceived.Trigger(Thread.CurrentThread, input)
