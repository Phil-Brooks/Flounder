namespace FlounderLib
open System

module MoveList =
    let BLACK_KING_CASTLE = 96UL
    let WHITE_KING_CASTLE = BLACK_KING_CASTLE <<< 56
    let BLACK_QUEEN_CASTLE = 14UL
    let WHITE_QUEEN_CASTLE = BLACK_QUEEN_CASTLE <<< 56
    let UnderAttack(sq:int, by:int) =
        let pawnAttack = if by = White then Attacks.BlackPawnAttacks[sq] else Attacks.WhitePawnAttacks[sq]
        if (pawnAttack &&& Brd.Pieces[by]) <> 0UL then true
        elif (Attacks.KnightMoves[sq] &&& Brd.Pieces[Knight*2 + by]) <> 0UL then true
        else
            let occupied = Brd.Both
            let queen = Brd.Pieces[Queen*2 + by]
            let mutable mIndex = Attacks.GetMagicIndex(Rook, occupied, sq)
            if (Attacks.SlidingMoves[mIndex] &&& (queen ||| Brd.Pieces[Rook*2 + by])) <> 0UL then true
            else
                mIndex <- Attacks.GetMagicIndex(Bishop, occupied, sq)
                if (Attacks.SlidingMoves[mIndex] &&& (queen ||| Brd.Pieces[Bishop*2 + by])) <> 0UL then true
                else
                     (Attacks.KingMoves[sq] &&& Brd.Pieces[King*2 + by]) <> 0UL
    let CheckBitBoard(sq:int, by:int) =
        let mutable count = 0
        let mutable checks = 0UL
        let pawnAttack = if by = White then Attacks.BlackPawnAttacks[sq] else Attacks.WhitePawnAttacks[sq]
        let pawnCheck = pawnAttack &&& Brd.Pieces[by]
        let knightCheck = Attacks.KnightMoves[sq] &&& Brd.Pieces[Knight*2 + by]
        let occupied = Brd.Both
        let queen = Brd.Pieces[Queen*2 + by]
        let mutable mIndex = Attacks.GetMagicIndex(Rook, occupied, sq)
        let rookQueenCheck = Attacks.SlidingMoves[mIndex] &&& (queen ||| Brd.Pieces[Rook*2 + by])
        mIndex <- Attacks.GetMagicIndex(Bishop, occupied, sq)
        let bishopQueenCheck = Attacks.SlidingMoves[mIndex] &&& (queen ||| Brd.Pieces[Bishop*2 + by])
        if pawnCheck <> 0UL then
            checks <- checks ||| pawnCheck
            count <- count + 1
        if knightCheck <> 0UL then
            checks <- checks ||| knightCheck
            count <- count + 1
        if rookQueenCheck <> 0UL then
            let rqSq = Bits.ToInt(rookQueenCheck)
            checks <- checks ||| Attacks.Between[sq][rqSq] ||| Bits.FromSq(rqSq)
            count <- count + 1
            if Bits.Count(rookQueenCheck) > 1 then count <- count + 1
        if bishopQueenCheck <> 0UL then
            let bqSq = Bits.ToInt(bishopQueenCheck)
            checks <- checks ||| Attacks.Between[sq][bqSq] ||| Bits.FromSq(bqSq)
            count <- count + 1
        if checks = 0UL then checks <- UInt64.MaxValue
        (checks, count > 1)
    let PinBitBoards(sq:int, us:int, by:int) =
        let byBoard = if by = White then Brd.White else Brd.Black
        let usBoard = if us = White then Brd.White else Brd.Black
        let queen = Brd.Pieces[Queen*2 + by]
        let mutable mIndex = Attacks.GetMagicIndex(Rook, byBoard, sq)
        let rookQueenCheck = Attacks.SlidingMoves[mIndex] &&& (queen ||| Brd.Pieces[Rook*2 + by])
        mIndex <- Attacks.GetMagicIndex(Bishop, byBoard, sq)
        let bishopQueenCheck = Attacks.SlidingMoves[mIndex] &&& (queen ||| Brd.Pieces[Bishop*2 + by])
        let mutable hvPin = 0UL
        let mutable dPin = 0UL
        let rqSqarr = Bits.ToArray(rookQueenCheck)
        for rqSq in rqSqarr do
            let possiblePin = Attacks.Between[sq][rqSq] ||| Bits.FromSq(rqSq)
            if Bits.Count(possiblePin &&& usBoard) = 1 then hvPin <- hvPin ||| possiblePin
        let bqSqarr = Bits.ToArray(bishopQueenCheck)
        for bqSq in bqSqarr do
            let possiblePin = Attacks.Between.[sq].[bqSq] ||| Bits.FromSq(bqSq)
            if Bits.Count(possiblePin &&& usBoard) = 1 then dPin <- dPin ||| possiblePin
        (hvPin, dPin)
    let LegalPawnMoveCaptures(from:int, hv:uint64, d:uint64, c:uint64) =
        let color = Brd.Stm
        let mutable moves = 0UL
        if Bits.IsSet(hv, from) then
            moves,false
        else 
            let oppColor = Brd.Xstm
            let opposite = if Brd.IsWtm then Brd.Black else Brd.White
            let mutable epPieceSq = Na
            let promotion = color = White && from < A6 &&  from > H8 || 
                            color = Black && from < A1 && from > H3
            if Brd.EnPassantTarget <> Na then
                epPieceSq <- if Brd.IsWtm then Brd.EnPassantTarget + 8 else Brd.EnPassantTarget - 8
                let epTargetPieceExists =  Bits.IsSet(Brd.Pieces[oppColor], epPieceSq)
                let reverseCorner = if Brd.IsWtm then Attacks.BlackPawnAttacks[Brd.EnPassantTarget] else Attacks.WhitePawnAttacks[Brd.EnPassantTarget]
                if (epTargetPieceExists && Bits.IsSet(reverseCorner, from)) then
                    moves <- moves ||| Bits.FromSq(Brd.EnPassantTarget)
            let attack = if Brd.IsWtm then Attacks.WhitePawnAttacks[from] else Attacks.BlackPawnAttacks[from]
            moves <- moves ||| (attack &&& opposite &&& c)
            if Bits.IsSet(d, from) then
                moves <- moves &&& d
                moves,promotion
            else
                if epPieceSq <> Na then
                    Board.Empty(from)
                    Board.Empty(epPieceSq)
                    Board.InsertPiece(color, Brd.EnPassantTarget)
                    let kingSq = if Brd.IsWtm then Brd.WhiteKingLoc else Brd.BlackKingLoc
                    if (UnderAttack(kingSq, oppColor)) then moves <- moves &&& ~~~(1UL <<< Brd.EnPassantTarget)
                    Board.InsertPiece(color, from)
                    Board.InsertPiece(oppColor, epPieceSq)
                    Board.Empty(Brd.EnPassantTarget)
                    if Bits.IsSet(moves, Brd.EnPassantTarget) && not (Bits.IsSet(c, epPieceSq)) then moves <- moves &&& ~~~(1UL <<< Brd.EnPassantTarget)
                    moves,promotion
                else
                    moves,promotion                
    let LegalPawnMoves(from:int, hv:uint64, d:uint64, c:uint64) =
        let color = Brd.Stm
        let mutable moves = 0UL
        let oppColor = Brd.Xstm
        let colBoard = if Brd.IsWtm then Brd.White else Brd.Black
        let opposite = if Brd.IsWtm then Brd.Black else Brd.White
        let mutable epPieceSq = Na
        let promotion = color = White && from < A6 &&  from > H8 || 
                        color = Black && from < A1 && from > H3
        if Brd.EnPassantTarget <> Na then
            epPieceSq <- if Brd.IsWtm then Brd.EnPassantTarget + 8 else Brd.EnPassantTarget - 8
            let epTargetPieceExists = Bits.IsSet(Brd.Pieces[oppColor], epPieceSq)
            let reverseCorner = if Brd.IsWtm then Attacks.BlackPawnAttacks[Brd.EnPassantTarget] else Attacks.WhitePawnAttacks[Brd.EnPassantTarget]
            if (epTargetPieceExists && Bits.IsSet(reverseCorner, from)) then
                moves <- moves ||| Bits.FromSq(Brd.EnPassantTarget)
        let attack = if color = White then Attacks.WhitePawnAttacks[from] else Attacks.BlackPawnAttacks[from]
        moves <- moves ||| (attack &&& opposite &&& c)
        if Bits.IsSet(d, from) then
            moves <- moves &&& d
            moves,promotion
        else
            let mutable pushes = 0UL
            pushes <- pushes ||| (if Brd.IsWtm then Bits.FromSq(from) >>> 8 else Bits.FromSq(from) <<< 8) &&& ~~~Brd.Both
            if ((from < A1 && from > H3 || from < A6 && from > H8) && pushes <> 0UL) then
                pushes <- pushes ||| if Brd.IsWtm then Bits.FromSq(from) >>> 16 else Bits.FromSq(from) <<< 16
            pushes <- pushes &&& ~~~(opposite) &&& ~~~(colBoard)
            moves <- moves ||| (pushes &&& c)
            if Bits.IsSet(hv, from) then
                moves <- moves &&& hv
                moves,promotion
            else
                if epPieceSq <> Na then
                    Board.Empty(from)
                    Board.Empty(epPieceSq)
                    Board.InsertPiece(color, Brd.EnPassantTarget)
                    let kingSq = if Brd.IsWtm then Brd.WhiteKingLoc else Brd.BlackKingLoc
                    if (UnderAttack(kingSq, oppColor)) then moves <- moves &&& ~~~(1UL <<< Brd.EnPassantTarget)
                    Board.InsertPiece(color, from)
                    Board.InsertPiece(oppColor, epPieceSq)
                    Board.Empty(Brd.EnPassantTarget)
                    if Bits.IsSet(moves, Brd.EnPassantTarget) && not (Bits.IsSet(c, epPieceSq)) then moves <- moves &&& ~~~(1UL <<< Brd.EnPassantTarget)
                    moves,promotion
                else
                    moves,promotion
    let LegalRookMoves(from:int, hv:uint64, d:uint64, c:uint64) =
        let mutable moves = 0UL
        let colBoard = if Brd.IsWtm then Brd.White else Brd.Black
        if Bits.IsSet(d, from) then moves
        else
            let mIndex = Attacks.GetMagicIndex(Rook, Brd.Both, from)
            moves <- moves ||| Attacks.SlidingMoves[mIndex] &&& ~~~(colBoard) &&& c
            if Bits.IsSet(hv, from) then moves <- moves &&& hv
            moves
    let LegalKnightMoves(from:int, hv:uint64, d:uint64, c:uint64) =
        let mutable moves = 0UL
        let colBoard = if Brd.IsWtm then Brd.White else Brd.Black
        if Bits.IsSet(hv, from) || Bits.IsSet(d, from) then moves
        else
            moves <- moves ||| Attacks.KnightMoves[from] &&& ~~~(colBoard) &&& c
            moves
    let LegalBishopMoves(from:int, hv:uint64, d:uint64, c:uint64) =
        let color = Brd.Stm
        let mutable moves = 0UL
        let colBoard = if Brd.IsWtm then Brd.White else Brd.Black
        if Bits.IsSet(hv, from) then moves
        else
            let mIndex = Attacks.GetMagicIndex(Bishop, Brd.Both, from)
            moves <- moves ||| Attacks.SlidingMoves[mIndex] &&& ~~~(colBoard) &&& c
            if Bits.IsSet(d, from) then moves <- moves &&& d
            moves
    let LegalQueenMoves(from:int, hv:uint64, d:uint64, c:uint64) =
        LegalRookMoves(from,hv,d,c) 
        ||| LegalBishopMoves(from,hv,d,c)
    let LegalKingMoves(from:int, hv:uint64, d:uint64, c:uint64) =
        let mutable moves = 0UL
        let colBoard = if Brd.IsWtm then Brd.White else Brd.Black
        let mutable kingMoves = Attacks.KingMoves[from]
        kingMoves <- kingMoves &&& ~~~(colBoard)
        if kingMoves = 0UL then 
            moves
        else
            let oppColor = Brd.Xstm
            Board.Empty(from)
            let movearr = Bits.ToArray(kingMoves)
            for move in movearr do
                if UnderAttack(move, oppColor) then Bits.PopBit(&kingMoves, move)
            Board.InsertPiece((if Brd.IsWtm then WhiteKing else BlackKing), from)
            moves <- moves ||| kingMoves
            if (UnderAttack(from, oppColor)) then moves
            else
                let q = if Brd.IsWtm then Brd.WhiteQCastle else Brd.BlackQCastle
                let k = if Brd.IsWtm then Brd.WhiteKCastle else Brd.BlackKCastle
                if (q <> 0x0 && Bits.IsSet(kingMoves, from - 1) && not (UnderAttack(from - 2, oppColor))) then
                    let path = if Brd.IsWtm then WHITE_QUEEN_CASTLE else BLACK_QUEEN_CASTLE
                    let all = Brd.Both
                    if path &&& all = 0UL then
                        moves <- moves ||| Bits.FromSq(from - 2)
                if (k <> 0x0 && Bits.IsSet(kingMoves, from + 1) && not (UnderAttack(from + 2, oppColor))) then
                    let path = if Brd.IsWtm then WHITE_KING_CASTLE else BLACK_KING_CASTLE
                    let all = Brd.Both
                    if path &&& all = 0UL then
                        moves <- moves ||| Bits.FromSq(from + 2)
                moves
    let FromFields(from:int, horizontalVertical:uint64, diagonal:uint64, checks:uint64, moves:uint64, promotion:bool) =
        {
            From = from
            Hv = horizontalVertical
            D = diagonal
            C = checks
            Moves = moves
            Count = Bits.Count(moves)
            Promotion = promotion
        }
    let ForPawns(from:int, horizontalVertical:uint64, diagonal:uint64, checks:uint64) =
        let moves,promotion = LegalPawnMoveCaptures(from,horizontalVertical,diagonal,checks)
        FromFields(from, horizontalVertical, diagonal, checks, moves, promotion)
    let NotDouble(from:int, piece:int, horizontalVertical:uint64, diagonal:uint64, checks:uint64) =
        let mutable promotion = false
        let moves =
            if piece=Pawn then 
                let m,p = LegalPawnMoves(from,horizontalVertical,diagonal,checks)
                promotion <- p
                m
            elif piece=Rook then 
                LegalRookMoves(from,horizontalVertical,diagonal,checks)
            elif piece=Knight then 
                LegalKnightMoves(from,horizontalVertical,diagonal,checks)
            elif piece=Bishop then 
                LegalBishopMoves(from,horizontalVertical,diagonal,checks)
            elif piece=Queen then 
                LegalQueenMoves(from,horizontalVertical,diagonal,checks)
            elif piece=King then 
                LegalKingMoves(from,horizontalVertical,diagonal,checks)
            else 
                failwith "Cannot generate move for empty piece"
        FromFields(from, horizontalVertical, diagonal, checks, moves, promotion)
    let Double(from:int, horizontalVertical:uint64, diagonal:uint64, checks:uint64, doubleChecked:bool) =
        let colpc = Brd.Squares[from]
        let piece = colpc/2
        let mutable promotion = false
        if doubleChecked && piece <> King then
            FromFields(from, horizontalVertical, diagonal, checks, 0UL, promotion)
        else
            let moves =
                if piece=Pawn then 
                    let m,p = LegalPawnMoves(from,horizontalVertical,diagonal,checks)
                    promotion <- p
                    m
                elif piece=Rook then 
                    LegalRookMoves(from,horizontalVertical,diagonal,checks)
                elif piece=Knight then 
                    LegalKnightMoves(from,horizontalVertical,diagonal,checks)
                elif piece=Bishop then 
                    LegalBishopMoves(from,horizontalVertical,diagonal,checks)
                elif piece=Queen then 
                    LegalQueenMoves(from,horizontalVertical,diagonal,checks)
                elif piece=King then 
                    LegalKingMoves(from,horizontalVertical,diagonal,checks)
                else 
                    failwith "Cannot generate move for empty piece" 
            FromFields(from, horizontalVertical, diagonal, checks, moves, promotion)
    let ForSq(from:int) =
        let piece, color = ColPiece.ToPcCol(Brd.Squares[from])
        let oppositeColor = color ^^^ 1
        let kingSq = if color = White then Brd.WhiteKingLoc else Brd.BlackKingLoc
        let (horizontalVertical, diagonal) = PinBitBoards(kingSq, color, oppositeColor)
        let (checks, doubleChecked) = CheckBitBoard(kingSq, oppositeColor)
        Double(from, horizontalVertical, diagonal, checks, doubleChecked)
