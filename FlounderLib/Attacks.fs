namespace FlounderLib

module Attacks =
    // Attack tables & BM bitboards for fast move-generation.
    let BlackPawnAttacks:uint64 array = 
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
    let WhitePawnAttacks:uint64 array = 
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
    let KnightMoves:uint64 array = 
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
    let KingMoves:uint64 array = 
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
    let Hs:uint64 array = 
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
    let Vs:uint64 array = 
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
    let Edged = Hs.[0] ||| Hs.[7] ||| Vs.[0] ||| Vs.[7]
    let ROOK = 12
    let BISHOP = 9
    let RookMagicData:(uint64 * int) array = 
        [|
            (0x80280013FF84FFFFuL, 10890); (0x5FFBFEFDFEF67FFFuL, 50579); (0xFFEFFAFFEFFDFFFFuL, 62020);
            (0x003000900300008AuL, 67322); (0x0050028010500023uL, 80251); (0x0020012120A00020uL, 58503);
            (0x0030006000C00030uL, 51175); (0x0058005806B00002uL, 83130);

            (0x7FBFF7FBFBEAFFFCuL, 50430); (0x0000140081050002uL, 21613); (0x0000180043800048uL, 72625);
            (0x7FFFE800021FFFB8uL, 80755); (0xFFFFCFFE7FCFFFAFuL, 69753); (0x00001800C0180060uL, 26973);
            (0x4F8018005FD00018uL, 84972); (0x0000180030620018uL, 31958);

            (0x00300018010C0003uL, 69272); (0x0003000C0085FFFFuL, 48372); (0xFFFDFFF7FBFEFFF7uL, 65477);
            (0x7FC1FFDFFC001FFFuL, 43972); (0xFFFEFFDFFDFFDFFFuL, 57154); (0x7C108007BEFFF81FuL, 53521);
            (0x20408007BFE00810uL, 30534); (0x0400800558604100uL, 16548);

            (0x0040200010080008uL, 46407); (0x0010020008040004uL, 11841); (0xFFFDFEFFF7FBFFF7uL, 21112);
            (0xFEBF7DFFF8FEFFF9uL, 44214); (0xC00000FFE001FFE0uL, 57925); (0x4AF01F00078007C3uL, 29574);
            (0xBFFBFAFFFB683F7FuL, 17309); (0x0807F67FFA102040uL, 40143);

            (0x200008E800300030uL, 64659); (0x0000008780180018uL, 70469); (0x0000010300180018uL, 62917);
            (0x4000008180180018uL, 60997); (0x008080310005FFFAuL, 18554); (0x4000188100060006uL, 14385);
            (0xFFFFFF7FFFBFBFFFuL,     0); (0x0000802000200040uL, 38091);

            (0x20000202EC002800uL, 25122); (0xFFFFF9FF7CFFF3FFuL, 60083); (0x000000404B801800uL, 72209);
            (0x2000002FE03FD000uL, 67875); (0xFFFFFF6FFE7FCFFDuL, 56290); (0xBFF7EFFFBFC00FFFuL, 43807);
            (0x000000100800A804uL, 73365); (0x6054000A58005805uL, 76398);

            (0x0829000101150028uL, 20024); (0x00000085008A0014uL, 9513); (0x8000002B00408028uL, 24324);
            (0x4000002040790028uL, 22996); (0x7800002010288028uL, 23213); (0x0000001800E08018uL, 56002);
            (0xA3A80003F3A40048uL, 22809); (0x2003D80000500028uL, 44545);

            (0xFFFFF37EEFEFDFBEuL, 36072); (0x40000280090013C1uL, 4750); (0xBF7FFEFFBFFAF71FuL, 6014);
            (0xFFFDFFFF777B7D6EuL, 36054); (0x48300007E8080C02uL, 78538); (0xAFE0000FFF780402uL, 28745);
            (0xEE73FFFBFFBB77FEuL,  8555); (0x0002000308482882uL, 1009)
        |]
    let BishopMagicData :(uint64 * int) array = 
        [|
            (0xA7020080601803D8uL, 60984); (0x13802040400801F1uL, 66046); (0x0A0080181001F60CuL, 32910);
            (0x1840802004238008uL, 16369); (0xC03FE00100000000uL, 42115); (0x24C00BFFFF400000uL,   835);
            (0x0808101F40007F04uL, 18910); (0x100808201EC00080uL, 25911);
            
            (0xFFA2FEFFBFEFB7FFuL, 63301); (0x083E3EE040080801uL, 16063); (0xC0800080181001F8uL, 17481);
            (0x0440007FE0031000uL, 59361); (0x2010007FFC000000uL, 18735); (0x1079FFE000FF8000uL, 61249);
            (0x3C0708101F400080uL, 68938); (0x080614080FA00040uL, 61791);
            
            (0x7FFE7FFF817FCFF9uL, 21893); (0x7FFEBFFFA01027FDuL, 62068); (0x53018080C00F4001uL, 19829);
            (0x407E0001000FFB8AuL, 26091); (0x201FE000FFF80010uL, 15815); (0xFFDFEFFFDE39FFEFuL, 16419);
            (0xCC8808000FBF8002uL, 59777); (0x7FF7FBFFF8203FFFuL, 16288);
            
            (0x8800013E8300C030uL, 33235); (0x0420009701806018uL, 15459); (0x7FFEFF7F7F01F7FDuL, 15863);
            (0x8700303010C0C006uL, 75555); (0xC800181810606000uL, 79445); (0x20002038001C8010uL, 15917);
            (0x087FF038000FC001uL,  8512); (0x00080C0C00083007uL, 73069);
            
            (0x00000080FC82C040uL, 16078); (0x000000407E416020uL, 19168); (0x00600203F8008020uL, 11056);
            (0xD003FEFE04404080uL, 62544); (0xA00020C018003088uL, 80477); (0x7FBFFE700BFFE800uL, 75049);
            (0x107FF00FE4000F90uL, 32947); (0x7F8FFFCFF1D007F8uL, 59172);
            
            (0x0000004100F88080uL, 55845); (0x00000020807C4040uL, 61806); (0x00000041018700C0uL, 73601);
            (0x0010000080FC4080uL, 15546); (0x1000003C80180030uL, 45243); (0xC10000DF80280050uL, 20333);
            (0xFFFFFFBFEFF80FDCuL, 33402); (0x000000101003F812uL, 25917);
            
            (0x0800001F40808200uL, 32875); (0x084000101F3FD208uL,  4639); (0x080000000F808081uL, 17077);
            (0x0004000008003F80uL, 62324); (0x08000001001FE040uL, 18159); (0x72DD000040900A00uL, 61436);
            (0xFFFFFEFFBFEFF81DuL, 57073); (0xCD8000200FEBF209uL, 61025);
            
            (0x100000101EC10082uL, 81259); (0x7FBAFFFFEFE0C02FuL, 64083); (0x7F83FFFFFFF07F7FuL, 56114);
            (0xFFF1FFFFFFF7FFC1uL, 57058); (0x0878040000FFE01FuL, 58912); (0x945E388000801012uL, 22194);
            (0x0840800080200FDAuL, 70880); (0x100000C05F582008uL, 11140)
        |]
    let GenerateRookOccupiedMask(sq:int) =
        // Horizontal files inside.
        let hMoves = Hs[sq % 8] &&& ~~~(Vs[0] ||| Vs[7])
        // Vertical ranks inside.
        let vMoves = Vs[sq / 8] &&& ~~~(Hs[0] ||| Hs[7])
        // Occupied inside but the square.
        (hMoves ||| vMoves) &&& (~~~Bits.FromSq(sq))
    let GenerateBishopOccupiedMask(sq:int) =
        let h = sq % 8
        let v = sq / 8
        let mutable rays = 0UL
        // Dumb raycast.
        for hI = 0 to 7 do
            for vI = 0 to 7 do
                let hD = abs(hI - h)
                let vD = abs(vI - v)
                if (hD = vD && vD <> 0)  then rays <- rays ||| (1UL <<< vI * 8 + hI)
        // All rays inside.
        rays &&& ~~~Edged
    let RookMagic =
        let rookMagic:(uint64 * uint64 * int) array = Array.zeroCreate 64
        for h = 0 to 7 do
            for v = 0 to 7 do
                let sq = v * 8 + h
                let magic, offset = RookMagicData[sq]
                // Flip mask for BM bitboards.
                rookMagic[sq] <- (magic, ~~~(GenerateRookOccupiedMask(sq)), offset)
        rookMagic
    let BishopMagic =
        let bishopMagic:(uint64 * uint64 * int) array = Array.zeroCreate 64
        for h = 0 to 7 do
            for v = 0 to 7 do
                let sq = v * 8 + h
                let magic, offset = BishopMagicData[sq]
                // Flip mask for BM bitboards.
                bishopMagic[sq] <- (magic, ~~~(GenerateBishopOccupiedMask(sq)), offset)
        bishopMagic
    let GetMagicIndex(piece:int, occupied:uint64, sq:int) =
        let args1,args2 =
            if piece = Rook then (RookMagic, ROOK)
            else (BishopMagic, BISHOP)
        // Get magic.
        let magic, mask, offset = args1[sq]
        // Get the relevant occupied squares.
        let relevantOccupied = occupied ||| mask
        // Get hash based on relevant occupied and magic.
        let hash = relevantOccupied * magic
        // Return with offset.
        offset + int(hash >>> 64 - args2)
    let SlidingMoves =
        let slidingMoves:uint64 array = Array.zeroCreate 87988
        for piece in [|Rook;Bishop|] do
            let magic = if piece = Rook then RookMagic else BishopMagic
            // Deltas for pieces.
            let deltas = 
                if piece = Rook then
                    [|
                        (1, 0);
                        (0, -1);
                        (-1, 0);
                        (0, 1)
                    |]
                else
                    [|
                        (1, 1);
                        (1, -1);
                        (-1, -1);
                        (-1, 1)
                    |]
            for h = 0 to 7 do
                for v = 0 to 7 do
                    // Flip the mask.
                    let sq = v * 8 + h
                    let _,bb2,_ = magic[sq]
                    let mask = ~~~bb2
                    let mutable occupied = 0UL
                    let mutable keepgoing = true
                    while (keepgoing) do
                        let mutable moves = 0UL
                        // Use deltas for slides.
                        for (dH, dV) in deltas do
                            let mutable hI = h
                            let mutable vI = v
                            // Dumb raycast
                            let mutable keepgoing2 = true
                            while (keepgoing2 && not (Bits.IsSet(occupied, vI * 8 + hI))) do
                                if (hI + dH > 7 || hI + dH < 0 )|| (vI + dV > 7 || vI + dV < 0) then keepgoing2 <- false
                                else
                                    hI <- hI + dH
                                    vI <- vI + dV
                                    let sqI = vI * 8 + hI
                                    moves <- moves ||| Bits.FromSq(sqI)
                        // Add to list with magic index.
                        slidingMoves[GetMagicIndex(piece, occupied, sq)] <- moves
                        // Reset mask.
                        occupied <- (occupied - mask) &&& mask
                        // If there is no occupied, we can break to next iteration.
                        if Bits.Count(occupied) = 0 then
                            keepgoing <- false
        slidingMoves
    let Between =
        let between:uint64 array array = Array.zeroCreate 64
        for fromSq = A8 to H1 do
            let fromH, fromV = fromSq % 8, fromSq / 8
            between[fromSq] <- Array.zeroCreate 64
            for toSq = A8 to H1 do
                between[fromSq][toSq] <- 0UL
                if fromSq <> toSq then 
                    let mutable occ = 0UL
                    let mutable mFrom = 0
                    let mutable mTo = 0
                    let toH, toV = toSq % 8, toSq / 8
                    if fromH = toH || fromV = toV then
                        // We calculate rook (straight) squares here.
                        occ <- Bits.FromSq(fromSq) ||| Bits.FromSq(toSq)
                        mFrom <- GetMagicIndex(Rook, occ, fromSq)
                        mTo <- GetMagicIndex(Rook, occ, toSq)
                        between[fromSq][toSq] <- (SlidingMoves[mFrom] &&& SlidingMoves[mTo])
                    else
                        let absH = abs(fromH - toH)
                        let absV = abs(fromV - toV)
                        if absH = absV then 
                            // We calculate bishop (diagonal) squares between here.
                            occ <- Bits.FromSq(fromSq) ||| Bits.FromSq(toSq)
                            mFrom <- GetMagicIndex(Bishop, occ, fromSq)
                            mTo <- GetMagicIndex(Bishop, occ, toSq)
                            between[fromSq][toSq] <- (SlidingMoves[mFrom] &&& SlidingMoves[mTo])
        between
