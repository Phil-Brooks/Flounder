namespace FlounderLib
open System

module BlackMagicBitBoardFactory =
    let ROOK = 12
    let BISHOP = 9
    // Magic Data
    let RookMagicData:(BitBoard * int) array = 
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
        |>Array.map(fun(u,i) -> BitBoard(u),i)
    let BishopMagicData :(BitBoard * int) array = 
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
        |>Array.map(fun(u,i) -> BitBoard(u),i)
    // Magic Providers
    let RookMagic:(BitBoard * BitBoard * int) array = Array.zeroCreate 64
    let BishopMagic:(BitBoard * BitBoard * int) array = Array.zeroCreate 64
    let GenerateRookOccupiedMask(sq:Square) =
        // Horizontal files inside.
        let hMoves = UtilityTable.Hs.[int(sq) % 8] &&& ~~~(UtilityTable.Vs.[0] ||| UtilityTable.Vs.[7])
        // Vertical ranks inside.
        let vMoves = UtilityTable.Vs.[int(sq) / 8] &&& ~~~(UtilityTable.Hs.[0] ||| UtilityTable.Hs.[7])
        // Occupied inside but the square.
        (hMoves ||| vMoves) &&& (~~~BitBoard.FromSq(sq))
    let GenerateBishopOccupiedMask(sq:Square) =
        let h = int(sq) % 8;
        let v = int(sq) / 8;
        let mutable rays = BitBoard.Default
        // Dumb raycast.
        for hI = 0 to 7 do
            for vI = 0 to 7 do
                let hD = Math.Abs(hI - h)
                let vD = Math.Abs(vI - v)
                if (hD = vD && vD <> 0)  then rays <- rays ||| BitBoard(1UL <<< vI * 8 + hI)
        // All rays inside.
        rays &&& ~~~(UtilityTable.Edged)
    let GenerateRookMagicTable() =
        for h = 0 to 7 do
            for v = 0 to 7 do
                let sq:Square = LanguagePrimitives.EnumOfValue(sbyte((v * 8 + h)))
                let magic, offset = RookMagicData.[int(sq)]
                // Flip mask for BM bitboards.
                RookMagic.[int(sq)] <- (magic, ~~~(GenerateRookOccupiedMask(sq)), offset)
    let GenerateBishopMagicTable() =
        for h = 0 to 7 do
            for v = 0 to 7 do
                let sq:Square = LanguagePrimitives.EnumOfValue(sbyte((v * 8 + h)))
                let magic, offset = BishopMagicData.[int(sq)]
                // Flip mask for BM bitboards.
                BishopMagic.[int(sq)] <- (magic, ~~~(GenerateBishopOccupiedMask(sq)), offset)
    let SetUp() =
        GenerateRookMagicTable()
        GenerateBishopMagicTable()
    let GetMagicIndex(piece:Piece, occupied:BitBoard, sq:Square) =
        let args1,args2 =
            if piece = Piece.Rook then (RookMagic, ROOK)
            elif piece = Piece.Bishop then (BishopMagic, BISHOP)
            else raise (System.IO.InvalidDataException("No magic table found."))
        // Get magic.
        let magic, mask, offset = args1.AA(int(sq))
        // Get the relevant occupied squares.
        let relevantOccupied = occupied ||| mask
        // Get hash based on relevant occupied and magic.
        let hash = relevantOccupied * magic
        // Return with offset.
        offset + int(uint64(hash.ToUint64() >>> 64 - args2))
