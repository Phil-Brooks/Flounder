﻿namespace FlounderLib

module AttackTable1 =
    // Attack tables & BM bitboards for fast move-generation.
    // Square BitBoards (size 64)
    let WhitePawnAttacks:BitBoard array = 
        [|
            0x0000000000000200uL; 0x0000000000000500uL; 0x0000000000000a00uL; 0x0000000000001400uL; 
            0x0000000000002800uL; 0x0000000000005000uL; 0x000000000000a000uL; 0x0000000000004000uL; 
            0x0000000000020000uL; 0x0000000000050000uL; 0x00000000000a0000uL; 0x0000000000140000uL; 
            0x0000000000280000uL; 0x0000000000500000uL; 0x0000000000a00000uL; 0x0000000000400000uL; 
            0x0000000002000000uL; 0x0000000005000000uL; 0x000000000a000000uL; 0x0000000014000000uL; 
            0x0000000028000000uL; 0x0000000050000000uL; 0x00000000a0000000uL; 0x0000000040000000uL; 
            0x0000000200000000uL; 0x0000000500000000uL; 0x0000000a00000000uL; 0x0000001400000000uL; 
            0x0000002800000000uL; 0x0000005000000000uL; 0x000000a000000000uL; 0x0000004000000000uL;
            0x0000020000000000uL; 0x0000050000000000uL; 0x00000a0000000000uL; 0x0000140000000000uL; 
            0x0000280000000000uL; 0x0000500000000000uL; 0x0000a00000000000uL; 0x0000400000000000uL; 
            0x0002000000000000uL; 0x0005000000000000uL; 0x000a000000000000uL; 0x0014000000000000uL; 
            0x0028000000000000uL; 0x0050000000000000uL; 0x00a0000000000000uL; 0x0040000000000000uL; 
            0x0200000000000000uL; 0x0500000000000000uL; 0x0a00000000000000uL; 0x1400000000000000uL; 
            0x2800000000000000uL; 0x5000000000000000uL; 0xa000000000000000uL; 0x4000000000000000uL; 
            0x0000000000000000uL; 0x0000000000000000uL; 0x0000000000000000uL; 0x0000000000000000uL; 
            0x0000000000000000uL; 0x0000000000000000uL; 0x0000000000000000uL; 0x0000000000000000uL
        |]
        |> Array.map(fun u -> BitBoard(u))
    let BlackPawnAttacks:BitBoard array = 
        [|
            0x0000000000000000uL; 0x0000000000000000uL; 0x0000000000000000uL; 0x0000000000000000uL; 
            0x0000000000000000uL; 0x0000000000000000uL; 0x0000000000000000uL; 0x0000000000000000uL;
            0x0000000000000002uL; 0x0000000000000005uL; 0x000000000000000auL; 0x0000000000000014uL; 
            0x0000000000000028uL; 0x0000000000000050uL; 0x00000000000000a0uL; 0x0000000000000040uL; 
            0x0000000000000200uL; 0x0000000000000500uL; 0x0000000000000a00uL; 0x0000000000001400uL; 
            0x0000000000002800uL; 0x0000000000005000uL; 0x000000000000a000uL; 0x0000000000004000uL; 
            0x0000000000020000uL; 0x0000000000050000uL; 0x00000000000a0000uL; 0x0000000000140000uL; 
            0x0000000000280000uL; 0x0000000000500000uL; 0x0000000000a00000uL; 0x0000000000400000uL; 
            0x0000000002000000uL; 0x0000000005000000uL; 0x000000000a000000uL; 0x0000000014000000uL; 
            0x0000000028000000uL; 0x0000000050000000uL; 0x00000000a0000000uL; 0x0000000040000000uL; 
            0x0000000200000000uL; 0x0000000500000000uL; 0x0000000a00000000uL; 0x0000001400000000uL; 
            0x0000002800000000uL; 0x0000005000000000uL; 0x000000a000000000uL; 0x0000004000000000uL; 
            0x0000020000000000uL; 0x0000050000000000uL; 0x00000a0000000000uL; 0x0000140000000000uL; 
            0x0000280000000000uL; 0x0000500000000000uL; 0x0000a00000000000uL; 0x0000400000000000uL; 
            0x0002000000000000uL; 0x0005000000000000uL; 0x000a000000000000uL; 0x0014000000000000uL; 
            0x0028000000000000uL; 0x0050000000000000uL; 0x00a0000000000000uL; 0x0040000000000000uL
        |]
        |> Array.map(fun u -> BitBoard(u))
    let KnightMoves:BitBoard array = 
        [|
            0x0000000000020400uL; 0x0000000000050800uL; 0x00000000000A1100uL; 0x0000000000142200uL;
            0x0000000000284400uL; 0x0000000000508800uL; 0x0000000000A01000uL; 0x0000000000402000uL;
            0x0000000002040004uL; 0x0000000005080008uL; 0x000000000A110011uL; 0x0000000014220022uL;
            0x0000000028440044uL; 0x0000000050880088uL; 0x00000000A0100010uL; 0x0000000040200020uL;
            0x0000000204000402uL; 0x0000000508000805uL; 0x0000000A1100110AuL; 0x0000001422002214uL;
            0x0000002844004428uL; 0x0000005088008850uL; 0x000000A0100010A0uL; 0x0000004020002040uL;
            0x0000020400040200uL; 0x0000050800080500uL; 0x00000A1100110A00uL; 0x0000142200221400uL;
            0x0000284400442800uL; 0x0000508800885000uL; 0x0000A0100010A000uL; 0x0000402000204000uL;
            0x0002040004020000uL; 0x0005080008050000uL; 0x000A1100110A0000uL; 0x0014220022140000uL;
            0x0028440044280000uL; 0x0050880088500000uL; 0x00A0100010A00000uL; 0x0040200020400000uL;
            0x0204000402000000uL; 0x0508000805000000uL; 0x0A1100110A000000uL; 0x1422002214000000uL;
            0x2844004428000000uL; 0x5088008850000000uL; 0xA0100010A0000000uL; 0x4020002040000000uL;
            0x0400040200000000uL; 0x0800080500000000uL; 0x1100110A00000000uL; 0x2200221400000000uL;
            0x4400442800000000uL; 0x8800885000000000uL; 0x100010A000000000uL; 0x2000204000000000uL;
            0x0004020000000000uL; 0x0008050000000000uL; 0x00110A0000000000uL; 0x0022140000000000uL;
            0x0044280000000000uL; 0x0088500000000000uL; 0x0010A00000000000uL; 0x0020400000000000uL
        |]
        |> Array.map(fun u -> BitBoard(u))
    let KingMoves:BitBoard array = 
        [|
            0x0000000000000302uL; 0x0000000000000705uL; 0x0000000000000E0AuL; 0x0000000000001C14uL;
            0x0000000000003828uL; 0x0000000000007050uL; 0x000000000000E0A0uL; 0x000000000000C040uL;
            0x0000000000030203uL; 0x0000000000070507uL; 0x00000000000E0A0EuL; 0x00000000001C141CuL;
            0x0000000000382838uL; 0x0000000000705070uL; 0x0000000000E0A0E0uL; 0x0000000000C040C0uL;
            0x0000000003020300uL; 0x0000000007050700uL; 0x000000000E0A0E00uL; 0x000000001C141C00uL;
            0x0000000038283800uL; 0x0000000070507000uL; 0x00000000E0A0E000uL; 0x00000000C040C000uL;
            0x0000000302030000uL; 0x0000000705070000uL; 0x0000000E0A0E0000uL; 0x0000001C141C0000uL;
            0x0000003828380000uL; 0x0000007050700000uL; 0x000000E0A0E00000uL; 0x000000C040C00000uL;
            0x0000030203000000uL; 0x0000070507000000uL; 0x00000E0A0E000000uL; 0x00001C141C000000uL;
            0x0000382838000000uL; 0x0000705070000000uL; 0x0000E0A0E0000000uL; 0x0000C040C0000000uL;
            0x0003020300000000uL; 0x0007050700000000uL; 0x000E0A0E00000000uL; 0x001C141C00000000uL;
            0x0038283800000000uL; 0x0070507000000000uL; 0x00E0A0E000000000uL; 0x00C040C000000000uL;
            0x0302030000000000uL; 0x0705070000000000uL; 0x0E0A0E0000000000uL; 0x1C141C0000000000uL;
            0x3828380000000000uL; 0x7050700000000000uL; 0xE0A0E00000000000uL; 0xC040C00000000000uL;
            0x0203000000000000uL; 0x0507000000000000uL; 0x0A0E000000000000uL; 0x141C000000000000uL;
            0x2838000000000000uL; 0x5070000000000000uL; 0xA0E0000000000000uL; 0x40C0000000000000uL
        |]
        |> Array.map(fun u -> BitBoard(u))

    // Initialization
    let GenerateSlidingMoves(piece:Piece) =
        // Arguments for loop.
        let args1,_ =
            if piece = Piece.Rook then (BlackMagicBitBoardFactory.RookMagic, BlackMagicBitBoardFactory.ROOK)
            elif piece = Piece.Bishop then (BlackMagicBitBoardFactory.BishopMagic, BlackMagicBitBoardFactory.BISHOP)
            else raise (System.IO.InvalidDataException("No magic table found."))
        // Deltas for pieces.
        let deltas = 
            if piece = Piece.Rook then
                [|
                    (1, 0);
                    (0, -1);
                    (-1, 0);
                    (0, 1)
                |]
            elif piece = Piece.Bishop then
                [|
                    (1, 1);
                    (1, -1);
                    (-1, -1);
                    (-1, 1)
                |]
            else raise (System.IO.InvalidDataException("No magic table found."))
        for h = 0 to 7 do
            for v = 0 to 7 do
                // Flip the mask.
                let _,bb2,_ = args1.[v * 8 + h]
                let mask = ~~~(bb2)
                let sq:Square = LanguagePrimitives.EnumOfValue(sbyte(v * 8 + h))
                let mutable occupied = BitBoard.Default
                let mutable keepgoing = true
                while (keepgoing) do
                    let mutable moves = BitBoard.Default
                    // Use deltas for slides.
                    for (dH, dV) in deltas do
                        let mutable hI = h
                        let mutable vI = v
                        // Dumb raycast
                        let mutable keepgoing2 = true
                        while (keepgoing2 && not occupied.[vI * 8 + hI]) do
                            if (hI + dH > 7 || hI + dH < 0 )|| (vI + dV > 7 || vI + dV < 0) then keepgoing2 <- false
                            else
                                hI <- hI + dH
                                vI <- vI + dV
                                let sqI:Square = LanguagePrimitives.EnumOfValue(sbyte(vI * 8 + hI))
                                moves <- moves ||| BitBoard.FromSq(sqI)
                    // Add to list with magic index.
                    AttackTable.SlidingMoves.[BlackMagicBitBoardFactory.GetMagicIndex(piece, occupied, sq)] <- moves
                    // Reset mask.
                    occupied <- (occupied - mask) &&& mask
                    // If there is no occupied, we can break to next iteration.
                    if (occupied.Count = 0) then
                        keepgoing <- false
    let SetUp() =
        // Setup the factory.
        BlackMagicBitBoardFactory.SetUp()
        // Generate sliding moves for sliding pieces.
        GenerateSlidingMoves(Piece.Rook)
        GenerateSlidingMoves(Piece.Bishop)
        // Generate between table.
        UtilityTable1.GenerateBetweenTable()

        
