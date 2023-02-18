namespace FlounderLib

open System

module AttackTable1 =
    // Initialization
    let GenerateBetweenTable() =
        for obj in Enum.GetValues(typeof<Square>) do
            let fromSq = unbox<Square> obj
            if fromSq<>Square.Na then
                let fromH, fromV = (int(fromSq) % 8, int(fromSq) / 8)
                UtilityTable.Between.[int(fromSq)] <- Array.zeroCreate 64
                for obj in Enum.GetValues(typeof<Square>) do
                    let toSq = unbox<Square> obj
                    if toSq<>Square.Na then
                        UtilityTable.Between.[int(fromSq)].[int(toSq)] <- BitBoard.Default
                        // It's the same square so we can skip.
                        if (fromSq = toSq) then ()
                        else
                            let mutable occ = BitBoard.Default
                            let mutable mFrom = 0
                            let mutable mTo = 0
                            let toH, toV = int(toSq) % 8, int(toSq) / 8
                            if (fromH = toH || fromV = toV) then
                                // We calculate rook (straight) squares here.
                                occ <- BitBoard.FromSq(fromSq) ||| BitBoard.FromSq(toSq)
                                mFrom <- BlackMagicBitBoardFactory.GetMagicIndex(Piece.Rook, occ, fromSq)
                                mTo <- BlackMagicBitBoardFactory.GetMagicIndex(Piece.Rook, occ, toSq)
                                UtilityTable.Between.[int(fromSq)].[int(toSq)] <- (AttackTable.SlidingMoves.[mFrom] &&& AttackTable.SlidingMoves.[mTo])
                            else
                                let absH = Math.Abs(fromH - toH)
                                let absV = Math.Abs(fromV - toV)
                                if (absH <> absV) then ()
                                else
                                    // We calculate bishop (diagonal) squares between here.
                                    occ <- BitBoard.FromSq(fromSq) ||| BitBoard.FromSq(toSq)
                                    mFrom <- BlackMagicBitBoardFactory.GetMagicIndex(Piece.Bishop, occ, fromSq)
                                    mTo <- BlackMagicBitBoardFactory.GetMagicIndex(Piece.Bishop, occ, toSq)
                                    UtilityTable.Between.[int(fromSq)].[int(toSq)] <- (AttackTable.SlidingMoves.[mFrom] &&& AttackTable.SlidingMoves.[mTo])
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
                let sq = Square.FromInt(v * 8 + h)
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
                                let sqI = Square.FromInt(vI * 8 + hI)
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
        GenerateBetweenTable()

        

