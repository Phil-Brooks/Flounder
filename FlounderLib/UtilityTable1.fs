namespace FlounderLib
open System

module UtilityTable1 =
    let  Between:BitBoard array array = Array.zeroCreate 64
    let GenerateBetweenTable() =
        for obj in Enum.GetValues(typeof<Square>) do
            let fromSq = unbox<Square> obj
            if fromSq<>Square.Na then
                let fromH, fromV = (int(fromSq) % 8, int(fromSq) / 8)
                Between.[int(fromSq)] <- Array.zeroCreate 64
                for obj in Enum.GetValues(typeof<Square>) do
                    let toSq = unbox<Square> obj
                    if toSq<>Square.Na then
                        Between.[int(fromSq)].[int(toSq)] <- BitBoard.Default
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
                                Between.[int(fromSq)].[int(toSq)] <- (AttackTable.SlidingMoves.[mFrom] &&& AttackTable.SlidingMoves.[mTo])
                            else
                                let absH = Math.Abs(fromH - toH)
                                let absV = Math.Abs(fromV - toV)
                                if (absH <> absV) then ()
                                else
                                    // We calculate bishop (diagonal) squares between here.
                                    occ <- BitBoard.FromSq(fromSq) ||| BitBoard.FromSq(toSq)
                                    mFrom <- BlackMagicBitBoardFactory.GetMagicIndex(Piece.Bishop, occ, fromSq)
                                    mTo <- BlackMagicBitBoardFactory.GetMagicIndex(Piece.Bishop, occ, toSq)
                                    Between.[int(fromSq)].[int(toSq)] <- (AttackTable.SlidingMoves.[mFrom] &&& AttackTable.SlidingMoves.[mTo])
