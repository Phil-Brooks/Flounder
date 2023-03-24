namespace FlounderTest
open NUnit.Framework
open FsUnit
open FlounderLib

module BitBoard =
    let mutable WhiteB = BitBoard.Default
    let mutable BlackB = BitBoard.Default

    [<SetUp>]
    let Setup () =
        for h = 0 to 7 do
            for v = 0 to 7 do
                if (v < 2) then BlackB.[v * 8 + h] <- true
                if (v > 5) then WhiteB.[v * 8 + h] <- true

    [<Test>]
    let TestDefault() =
        let mutable success = true
        for h = 0 to 7 do
            for v = 0 to 7 do
                let lookup = BitBoard.Default.[v * 8 + h]
                if lookup then    
                    success <- false
        success |> should equal true

    [<Test>]
    let MarkA1AsTrue() =
        let mutable useBoard = BitBoard.Default
        useBoard.[Square.A1] <- true
        useBoard.[Square.A1] |> should equal true

    [<Test>]
    let MarkWhiteAsTrue() =
        let mutable useBoard = BitBoard.Default
        for h = 0 to 7 do
            for v = 0 to 7 do
                if (v < 2) then useBoard.[v * 8 + h] <- true

        let mutable success = true
        for h = 0 to 7 do
            for v = 0 to 7 do
                if (v < 2) then
                    if not useBoard.[v * 8 + h] then
                        success <- false
                elif useBoard.[v * 8 + h] then
                    success <- false

        success |> should equal true

    [<Test>]
    let MarkWhiteAsTruef() =
        let mutable useBoard = BitBoard.Default
        for h = 0 to 7 do
            for v = 0 to 7 do
                if (v < 2) then useBoard.[v * 8 + h] <- true

        let mutable success = true
        for h = 0 to 7 do
            for v = 0 to 7 do
                if (v < 2) then
                    if not useBoard.[v * 8 + h] then
                        success <- false
                elif useBoard.[v * 8 + h] then
                    success <- false

        success |> should equal true

    [<Test>]
    let MarkBlackAndWhiteAsTrue() =
        let mutable whiteBoard = BitBoard.Default
        let mutable blackBoard = BitBoard.Default
        for h = 0 to 7 do
            for v = 0 to 7 do
                if (v < 2) then whiteBoard.[v * 8 + h] <- true
                if (v > 5) then blackBoard.[v * 8 + h] <- true

        let mutable final = whiteBoard ||| blackBoard
        let mutable success = true
        for h = 0 to 7 do
            for v = 0 to 7 do
                if (v < 2) then
                    if not whiteBoard.[v * 8 + h] then
                        success <- false
                    if blackBoard.[v * 8 + h] then
                        success <- false
                elif (v > 5) then
                    if not blackBoard.[v * 8 + h] then
                        success <- false
                    if whiteBoard.[v * 8 + h] then
                        success <- false
                elif whiteBoard.[v * 8 + h] then
                    success <- false
                elif blackBoard.[v * 8 + h] then
                    success <- false

        success |> should equal true

    [<Test>]
    let MarkBlackAndWhiteAsTruef() =
        let mutable whiteBoard = BitBoard.Default
        let mutable blackBoard = BitBoard.Default
        for h = 0 to 7 do
            for v = 0 to 7 do
                if (v < 2) then whiteBoard.[v * 8 + h] <- true
                if (v > 5) then blackBoard.[v * 8 + h] <- true

        let mutable final = whiteBoard ||| blackBoard
        let mutable success = true
        for h = 0 to 7 do
            for v = 0 to 7 do
                if (v < 2) then
                    if not whiteBoard.[v * 8 + h] then
                        success <- false
                    if blackBoard.[v * 8 + h] then
                        success <- false
                elif (v > 5) then
                    if not blackBoard.[v * 8 + h] then
                        success <- false
                    if whiteBoard.[v * 8 + h] then
                        success <- false
                elif whiteBoard.[v * 8 + h] then
                    success <- false
                elif blackBoard.[v * 8 + h] then
                    success <- false

        success |> should equal true

    [<Test>]
    let Add() =
        let ans = WhiteB + BlackB
        BlackB.Count|>should equal 16
        WhiteB.Count|>should equal 16
        ans.Count|>should equal 32
        BlackB.Internal|>should equal 65535UL
        WhiteB.Internal|>should equal 18446462598732840960UL
        ans.Internal|>should equal 18446462598732906495UL

    [<Test>]
    let Minus() =
        let ans = BlackB - WhiteB
        ans.Count|>should equal 17
        BlackB.Internal|>should equal 65535UL
        WhiteB.Internal|>should equal 18446462598732840960UL
        ans.Internal|>should equal 281474976776191UL

    [<Test>]
    let Times() =
        let ans = WhiteB * BlackB
        ans.Count|>should equal 1
        BlackB.Internal|>should equal 65535UL
        WhiteB.Internal|>should equal 18446462598732840960UL
        ans.Internal|>should equal 281474976710656UL

    [<Test>]
    let Divide() =
        let ans = BlackB / WhiteB
        ans.Count|>should equal 0
        BlackB.Internal|>should equal 65535UL
        WhiteB.Internal|>should equal 18446462598732840960UL
        ans.Internal|>should equal 0UL

    [<Test>]
    let Remainder() =
        let ans = BlackB % 3UL
        ans.Count|>should equal 0
        BlackB.Internal|>should equal 65535UL
        ans.Internal|>should equal 0UL

    [<Test>]
    let Bor() =
        let ans = WhiteB ||| BlackB
        ans.Count|>should equal 32
        BlackB.Internal|>should equal 65535UL
        WhiteB.Internal|>should equal 18446462598732840960UL
        ans.Internal|>should equal 18446462598732906495UL

    [<Test>]
    let Band() =
        let ans = WhiteB &&& BlackB
        ans.Count|>should equal 0
        BlackB.Internal|>should equal 65535UL
        WhiteB.Internal|>should equal 18446462598732840960UL
        ans.Internal|>should equal 0UL

    [<Test>]
    let Bneg() =
        let ans = ~~~ BlackB
        ans.Count|>should equal 48
        BlackB.Internal|>should equal 65535UL
        ans.Internal|>should equal 18446744073709486080UL

    [<Test>]
    let Bright() =
        let ans = BlackB >>> 3
        ans.Count|>should equal 13
        BlackB.Internal|>should equal 65535UL
        ans.Internal|>should equal 8191UL

    [<Test>]
    let Bleft() =
        let ans = BlackB <<< 3
        ans.Count|>should equal 16
        BlackB.Internal|>should equal 65535UL
        ans.Internal|>should equal 524280UL

    [<Test>]
    let Equals() =
        let ans = WhiteB = BlackB
        ans|>should equal false

    [<Test>]
    let NotEquals() =
        let ans = WhiteB <> BlackB
        ans|>should equal true

    [<Test>]
    let ToBool() =
        let ans = WhiteB.ToBool()
        ans|>should equal true

    [<Test>]
    let ToUint64() =
        let ans = BlackB.ToUint64()
        ans|>should equal 65535UL

    [<Test>]
    let FromSq() =
        let ans = BitBoard.FromSq(Square.A8)
        ans.Internal|>should equal 1UL

    [<Test>]
    let ToSq() =
        let ans:Square = BlackB.ToSq()
        ans|>should equal Square.A8

    [<Test>]
    let ToSqs() =
        let ans:Square array = BlackB.ToSqs()
        ans.[0]|>should equal Square.A8
        ans.Length|>should equal 16

    [<Test>]
    let GetEnum() =
        let ans = BlackB.GetEnumerator()
        ans.Current|>should equal Square.A8

    [<Test>]
    let ToStr() =
        let ans = BlackB.ToString().Substring(0,8)
        ans|>should equal "1 1 1 1 "
        