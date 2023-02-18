namespace FlounderLib

module UtilityTable =
    let Hs:BitBoard array = 
        [|
            0x101010101010101uL;
            0x202020202020202uL;
            0x404040404040404uL;
            0x808080808080808uL;
            0x1010101010101010uL;
            0x2020202020202020uL;
            0x4040404040404040uL;
            0x8080808080808080uL
        |]
        |>Array.map(fun u -> BitBoard(u))
    let Vs:BitBoard array = 
        [|
            0xFFuL; 
            0xFF00uL; 
            0xFF0000uL; 
            0xFF000000uL;
            0xFF00000000uL;
            0xFF0000000000uL;
            0xFF000000000000uL;
            0xFF00000000000000uL
        |]
        |>Array.map(fun u -> BitBoard(u))
    let Edged = Hs.[0] ||| Hs.[7] ||| Vs.[0] ||| Vs.[7]
    let Between:BitBoard array array = Array.zeroCreate 64
