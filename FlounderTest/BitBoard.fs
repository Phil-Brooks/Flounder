namespace FlounderTest
open NUnit.Framework
open FsUnit
open FlounderLib

module BitBoard =
    let mutable WhiteB = 0UL
    let mutable BlackB = 0UL

    [<SetUp>]
    let Setup () =
        for h = 0 to 7 do
            for v = 0 to 7 do
                if (v < 2) then Bits.SetBit(&BlackB, v * 8 + h)
                if (v > 5) then Bits.SetBit(&WhiteB, v * 8 + h)

    [<Test>]
    let TestDefault() =
        let mutable success = true
        for h = 0 to 7 do
            for v = 0 to 7 do
                let lookup = Bits.IsSet(0UL, v * 8 + h)
                if lookup then    
                    success <- false
        success |> should equal true

    [<Test>]
    let MarkA1AsTrue() =
        let mutable useBoard = 0UL
        Bits.SetBit(&useBoard, A1)
        Bits.IsSet(useBoard,A1) |> should equal true

    [<Test>]
    let MarkWhiteAsTrue() =
        let mutable useBoard = 0UL
        for h = 0 to 7 do
            for v = 0 to 7 do
                if (v < 2) then Bits.SetBit(&useBoard, v * 8 + h)

        let mutable success = true
        for h = 0 to 7 do
            for v = 0 to 7 do
                if (v < 2) then
                    if not (Bits.IsSet(useBoard, v * 8 + h)) then
                        success <- false
                elif Bits.IsSet(useBoard, v * 8 + h) then
                    success <- false

        success |> should equal true

    [<Test>]
    let MarkWhiteAsTruef() =
        let mutable useBoard = 0UL
        for h = 0 to 7 do
            for v = 0 to 7 do
                if (v < 2) then Bits.SetBit(&useBoard, v * 8 + h)

        let mutable success = true
        for h = 0 to 7 do
            for v = 0 to 7 do
                if (v < 2) then
                    if not (Bits.IsSet(useBoard, v * 8 + h)) then
                        success <- false
                elif Bits.IsSet(useBoard, v * 8 + h) then
                    success <- false

        success |> should equal true

    [<Test>]
    let MarkBlackAndWhiteAsTrue() =
        let mutable whiteBoard = 0UL
        let mutable blackBoard = 0UL
        for h = 0 to 7 do
            for v = 0 to 7 do
                if (v < 2) then Bits.SetBit(&whiteBoard, v * 8 + h)
                if (v > 5) then Bits.SetBit(&blackBoard, v * 8 + h)

        let mutable final = whiteBoard ||| blackBoard
        let mutable success = true
        for h = 0 to 7 do
            for v = 0 to 7 do
                if (v < 2) then
                    if not (Bits.IsSet(whiteBoard, v * 8 + h)) then
                        success <- false
                    if Bits.IsSet(blackBoard, v * 8 + h) then
                        success <- false
                elif (v > 5) then
                    if not (Bits.IsSet(blackBoard, v * 8 + h)) then
                        success <- false
                    if Bits.IsSet(whiteBoard, v * 8 + h) then
                        success <- false
                elif Bits.IsSet(whiteBoard, v * 8 + h) then
                    success <- false
                elif Bits.IsSet(blackBoard, v * 8 + h) then
                    success <- false

        success |> should equal true

    [<Test>]
    let MarkBlackAndWhiteAsTruef() =
        let mutable whiteBoard = 0UL
        let mutable blackBoard = 0UL
        for h = 0 to 7 do
            for v = 0 to 7 do
                if (v < 2) then Bits.SetBit(&whiteBoard, v * 8 + h)
                if (v > 5) then Bits.SetBit(&blackBoard, v * 8 + h)

        let mutable final = whiteBoard ||| blackBoard
        let mutable success = true
        for h = 0 to 7 do
            for v = 0 to 7 do
                if (v < 2) then
                    if not (Bits.IsSet(whiteBoard, v * 8 + h)) then
                        success <- false
                    if Bits.IsSet(blackBoard, v * 8 + h) then
                        success <- false
                elif (v > 5) then
                    if not (Bits.IsSet(blackBoard, v * 8 + h)) then
                        success <- false
                    if Bits.IsSet(whiteBoard, v * 8 + h) then
                        success <- false
                elif Bits.IsSet(whiteBoard, v * 8 + h) then
                    success <- false
                elif Bits.IsSet(blackBoard, v * 8 + h) then
                    success <- false

        success |> should equal true

    [<Test>]
    let Add() =
        let ans = WhiteB + BlackB
        Bits.Count(BlackB)|>should equal 16
        Bits.Count(WhiteB)|>should equal 16
        Bits.Count(ans)|>should equal 32
        BlackB|>should equal 65535UL
        WhiteB|>should equal 18446462598732840960UL
        ans|>should equal 18446462598732906495UL

    [<Test>]
    let Minus() =
        let ans = BlackB - WhiteB
        Bits.Count(ans)|>should equal 17
        BlackB|>should equal 65535UL
        WhiteB|>should equal 18446462598732840960UL
        ans|>should equal 281474976776191UL

    [<Test>]
    let Times() =
        let ans = WhiteB * BlackB
        Bits.Count(ans)|>should equal 1
        BlackB|>should equal 65535UL
        WhiteB|>should equal 18446462598732840960UL
        ans|>should equal 281474976710656UL

    [<Test>]
    let Divide() =
        let ans = BlackB / WhiteB
        Bits.Count(ans)|>should equal 0
        BlackB|>should equal 65535UL
        WhiteB|>should equal 18446462598732840960UL
        ans|>should equal 0UL

    [<Test>]
    let Remainder() =
        let ans = BlackB % 3UL
        Bits.Count(ans)|>should equal 0
        BlackB|>should equal 65535UL
        ans|>should equal 0UL

    [<Test>]
    let Bor() =
        let ans = WhiteB ||| BlackB
        Bits.Count(ans)|>should equal 32
        BlackB|>should equal 65535UL
        WhiteB|>should equal 18446462598732840960UL
        ans|>should equal 18446462598732906495UL

    [<Test>]
    let Band() =
        let ans = WhiteB &&& BlackB
        Bits.Count(ans)|>should equal 0
        BlackB|>should equal 65535UL
        WhiteB|>should equal 18446462598732840960UL
        ans|>should equal 0UL

    [<Test>]
    let Bneg() =
        let ans = ~~~ BlackB
        Bits.Count(ans)|>should equal 48
        BlackB|>should equal 65535UL
        ans|>should equal 18446744073709486080UL

    [<Test>]
    let Bright() =
        let ans = BlackB >>> 3
        Bits.Count(ans)|>should equal 13
        BlackB|>should equal 65535UL
        ans|>should equal 8191UL

    [<Test>]
    let Bleft() =
        let ans = BlackB <<< 3
        Bits.Count(ans)|>should equal 16
        BlackB|>should equal 65535UL
        ans|>should equal 524280UL

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
        let ans = WhiteB <> 0UL
        ans|>should equal true

    [<Test>]
    let ToUint64() =
        let ans = BlackB
        ans|>should equal 65535UL

    [<Test>]
    let FromSq() =
        let ans = Bits.FromSq(A8)
        ans|>should equal 1UL

    [<Test>]
    let ToSq() =
        let ans:int = Bits.ToInt(BlackB)
        ans|>should equal A8

    [<Test>]
    let ToSqs() =
        let ans:int array = Bits.ToArray(BlackB)
        ans.[0]|>should equal A8
        ans.Length|>should equal 16

    [<Test>]
    let GetEnum() =
        let ans = Bits.ToSeq(BlackB)
        ans|>Seq.head|>should equal A8

       