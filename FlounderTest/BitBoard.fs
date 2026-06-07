namespace FlounderTest
open NUnit.Framework
open FsUnit
open Flounder

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
    let ``Default board is empty`` () =
        for h = 0 to 7 do
            for v = 0 to 7 do
                Bits.IsSet(0UL, v * 8 + h) |> should equal false

    [<Test>]
    let ``Set single bit at A1`` () =
        let mutable board = 0UL
        Bits.SetBit(&board, A1)
        Bits.IsSet(board, A1) |> should equal true

    [<Test>]
    let ``Set white pieces`` () =
        let mutable board = 0UL
        for h = 0 to 7 do
            for v = 0 to 7 do
                if v < 2 then Bits.SetBit(&board, v * 8 + h)

        Bits.Count(board) |> should equal 16
        for h = 0 to 7 do
            for v = 0 to 7 do
                let isSet = Bits.IsSet(board, v * 8 + h)
                if v < 2 then isSet |> should equal true
                else isSet |> should equal false

    [<Test>]
    let ``Set black and white pieces`` () =
        let mutable white = 0UL
        let mutable black = 0UL
        for h = 0 to 7 do
            for v = 0 to 7 do
                if v < 2 then Bits.SetBit(&white, v * 8 + h)
                if v > 5 then Bits.SetBit(&black, v * 8 + h)

        Bits.Count(white) |> should equal 16
        Bits.Count(black) |> should equal 16

        for h = 0 to 7 do
            for v = 0 to 7 do
                let w = Bits.IsSet(white, v * 8 + h)
                let b = Bits.IsSet(black, v * 8 + h)
                if v < 2 then (w, b) |> should equal (true, false)
                elif v > 5 then (w, b) |> should equal (false, true)
                else (w, b) |> should equal (false, false)

    [<Test>]
    let ``Add bitboards`` () =
        let ans = WhiteB + BlackB
        Bits.Count(BlackB) |> should equal 16
        Bits.Count(WhiteB) |> should equal 16
        Bits.Count(ans) |> should equal 32

    [<Test>]
    let ``Subtract bitboards`` () =
        let ans = BlackB - WhiteB
        Bits.Count(ans) |> should equal 17

    [<Test>]
    let ``Multiply bitboards`` () =
        let ans = WhiteB * BlackB
        Bits.Count(ans) |> should equal 1

    [<Test>]
    let ``Divide bitboards`` () =
        let ans = BlackB / WhiteB
        Bits.Count(ans) |> should equal 0

    [<Test>]
    let ``Modulo bitboard`` () =
        let ans = BlackB % 3UL
        Bits.Count(ans) |> should equal 0

    [<Test>]
    let ``Bitwise OR`` () =
        let ans = WhiteB ||| BlackB
        Bits.Count(ans) |> should equal 32

    [<Test>]
    let ``Bitwise AND`` () =
        let ans = WhiteB &&& BlackB
        Bits.Count(ans) |> should equal 0

    [<Test>]
    let ``Bitwise NOT`` () =
        let ans = ~~~ BlackB
        Bits.Count(ans) |> should equal 48

    [<Test>]
    let ``Right shift`` () =
        let ans = BlackB >>> 3
        Bits.Count(ans) |> should equal 13

    [<Test>]
    let ``Left shift`` () =
        let ans = BlackB <<< 3
        Bits.Count(ans) |> should equal 16

    [<Test>]
    let ``Equality`` () =
        (WhiteB = BlackB) |> should equal false

    [<Test>]
    let ``Inequality`` () =
        (WhiteB <> BlackB) |> should equal true

    [<Test>]
    let ``Non-zero comparison`` () =
        (WhiteB <> 0UL) |> should equal true

    [<Test>]
    let ``FromSq creates single bit`` () =
        Bits.FromSq(A8) |> should equal 1UL

    [<Test>]
    let ``ToInt gets trailing zero count`` () =
        Bits.ToInt(BlackB) |> should equal A8

    [<Test>]
    let ``ToArray converts to square array`` () =
        let ans = Bits.ToArray(BlackB)
        ans.[0] |> should equal A8
        ans.Length |> should equal 16

    [<Test>]
    let ``ToSeq generates sequence of squares`` () =
        Bits.ToSeq(BlackB) |> Seq.head |> should equal A8

       
