namespace FlounderLib
open System

module MoveList =
    let BLACK_KING_CASTLE = 96UL
    let WHITE_KING_CASTLE = BLACK_KING_CASTLE <<< 56
    let BLACK_QUEEN_CASTLE = 14UL
    let WHITE_QUEEN_CASTLE = BLACK_QUEEN_CASTLE <<< 56
    let UnderAttack(board:BoardRec, sq:int, by:int) =
        let pawnAttack = if by = White then Attacks.BlackPawnAttacks[sq] else Attacks.WhitePawnAttacks[sq]
        if (pawnAttack &&& board.Pieces[by]) <> 0UL then true
        elif (Attacks.KnightMoves[sq] &&& board.Pieces[Knight*2 + by]) <> 0UL then true
        else
            let occupied = board.Both
            let queen = board.Pieces[Queen*2 + by]
            let mutable mIndex = Attacks.GetMagicIndex(Rook, occupied, sq)
            if (Attacks.SlidingMoves[mIndex] &&& (queen ||| board.Pieces[Rook*2 + by])) <> 0UL then true
            else
                mIndex <- Attacks.GetMagicIndex(Bishop, occupied, sq)
                if (Attacks.SlidingMoves[mIndex] &&& (queen ||| board.Pieces[Bishop*2 + by])) <> 0UL then true
                else
                     (Attacks.KingMoves[sq] &&& board.Pieces[King*2 + by]) <> 0UL
    let CheckBitBoard(board:BoardRec, sq:int, by:int) =
        let mutable count = 0
        let mutable checks = 0UL
        let pawnAttack = if by = White then Attacks.BlackPawnAttacks[sq] else Attacks.WhitePawnAttacks[sq]
        let pawnCheck = pawnAttack &&& board.Pieces[by]
        let knightCheck = Attacks.KnightMoves[sq] &&& board.Pieces[Knight*2 + by]
        let occupied = board.Both
        let queen = board.Pieces[Queen*2 + by]
        let mutable mIndex = Attacks.GetMagicIndex(Rook, occupied, sq)
        let rookQueenCheck = Attacks.SlidingMoves[mIndex] &&& (queen ||| board.Pieces[Rook*2 + by])
        mIndex <- Attacks.GetMagicIndex(Bishop, occupied, sq)
        let bishopQueenCheck = Attacks.SlidingMoves[mIndex] &&& (queen ||| board.Pieces[Bishop*2 + by])
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
    let PinBitBoards(board:BoardRec, sq:int, us:int, by:int) =
        let byBoard = if by = White then board.White else board.Black
        let usBoard = if us = White then board.White else board.Black
        let queen = board.Pieces[Queen*2 + by]
        let mutable mIndex = Attacks.GetMagicIndex(Rook, byBoard, sq)
        let rookQueenCheck = Attacks.SlidingMoves[mIndex] &&& (queen ||| board.Pieces[Rook*2 + by])
        mIndex <- Attacks.GetMagicIndex(Bishop, byBoard, sq)
        let bishopQueenCheck = Attacks.SlidingMoves[mIndex] &&& (queen ||| board.Pieces[Bishop*2 + by])
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
    let LegalPawnMoveCaptures(board:byref<BoardRec>, from:int, hv:uint64, d:uint64, c:uint64) =
        let color = board.Stm
        let mutable moves = 0UL
        if Bits.IsSet(hv, from) then
            moves,false
        else 
            let oppColor = board.Xstm
            let opposite = if board.IsWtm then board.Black else board.White
            let mutable epPieceSq = Na
            let promotion = color = White && from < A6 &&  from > H8 || 
                            color = Black && from < A1 && from > H3
            if board.EnPassantTarget <> Na then
                epPieceSq <- if board.IsWtm then board.EnPassantTarget + 8 else board.EnPassantTarget - 8
                let epTargetPieceExists =  Bits.IsSet(board.Pieces[oppColor], epPieceSq)
                let reverseCorner = if board.IsWtm then Attacks.BlackPawnAttacks[board.EnPassantTarget] else Attacks.WhitePawnAttacks[board.EnPassantTarget]
                if (epTargetPieceExists && Bits.IsSet(reverseCorner, from)) then
                    moves <- moves ||| Bits.FromSq(board.EnPassantTarget)
            let attack = if board.IsWtm then Attacks.WhitePawnAttacks[from] else Attacks.BlackPawnAttacks[from]
            moves <- moves ||| (attack &&& opposite &&& c)
            if Bits.IsSet(d, from) then
                moves <- moves &&& d
                moves,promotion
            else
                if epPieceSq <> Na then
                    Board.Empty(&board, from)
                    Board.Empty(&board, epPieceSq)
                    Board.InsertPiece(&board, color, board.EnPassantTarget)
                    let kingSq = if board.IsWtm then board.WhiteKingLoc else board.BlackKingLoc
                    if (UnderAttack(board, kingSq, oppColor)) then moves <- moves &&& ~~~(1UL <<< board.EnPassantTarget)
                    Board.InsertPiece(&board, color, from)
                    Board.InsertPiece(&board, oppColor, epPieceSq)
                    Board.Empty(&board, board.EnPassantTarget)
                    if Bits.IsSet(moves, board.EnPassantTarget) && not (Bits.IsSet(c, epPieceSq)) then moves <- moves &&& ~~~(1UL <<< board.EnPassantTarget)
                    moves,promotion
                else
                    moves,promotion                
    let LegalPawnMoves(board:byref<BoardRec>, from:int, hv:uint64, d:uint64, c:uint64) =
        let color = board.Stm
        let mutable moves = 0UL
        let oppColor = board.Xstm
        let colBoard = if board.IsWtm then board.White else board.Black
        let opposite = if board.IsWtm then board.Black else board.White
        let mutable epPieceSq = Na
        let promotion = color = White && from < A6 &&  from > H8 || 
                        color = Black && from < A1 && from > H3
        if board.EnPassantTarget <> Na then
            epPieceSq <- if board.IsWtm then board.EnPassantTarget + 8 else board.EnPassantTarget - 8
            let epTargetPieceExists = Bits.IsSet(board.Pieces[oppColor], epPieceSq)
            let reverseCorner = if board.IsWtm then Attacks.BlackPawnAttacks[board.EnPassantTarget] else Attacks.WhitePawnAttacks[board.EnPassantTarget]
            if (epTargetPieceExists && Bits.IsSet(reverseCorner, from)) then
                moves <- moves ||| Bits.FromSq(board.EnPassantTarget)
        let attack = if color = White then Attacks.WhitePawnAttacks[from] else Attacks.BlackPawnAttacks[from]
        moves <- moves ||| (attack &&& opposite &&& c)
        if Bits.IsSet(d, from) then
            moves <- moves &&& d
            moves,promotion
        else
            let mutable pushes = 0UL
            pushes <- pushes ||| (if board.IsWtm then Bits.FromSq(from) >>> 8 else Bits.FromSq(from) <<< 8) &&& ~~~board.Both
            if ((from < A1 && from > H3 || from < A6 && from > H8) && pushes <> 0UL) then
                pushes <- pushes ||| if board.IsWtm then Bits.FromSq(from) >>> 16 else Bits.FromSq(from) <<< 16
            pushes <- pushes &&& ~~~(opposite) &&& ~~~(colBoard)
            moves <- moves ||| (pushes &&& c)
            if Bits.IsSet(hv, from) then
                moves <- moves &&& hv
                moves,promotion
            else
                if epPieceSq <> Na then
                    Board.Empty(&board, from)
                    Board.Empty(&board, epPieceSq)
                    Board.InsertPiece(&board, color, board.EnPassantTarget)
                    let kingSq = if board.IsWtm then board.WhiteKingLoc else board.BlackKingLoc
                    if (UnderAttack(board, kingSq, oppColor)) then moves <- moves &&& ~~~(1UL <<< board.EnPassantTarget)
                    Board.InsertPiece(&board, color, from)
                    Board.InsertPiece(&board, oppColor, epPieceSq)
                    Board.Empty(&board, board.EnPassantTarget)
                    if Bits.IsSet(moves, board.EnPassantTarget) && not (Bits.IsSet(c, epPieceSq)) then moves <- moves &&& ~~~(1UL <<< board.EnPassantTarget)
                    moves,promotion
                else
                    moves,promotion
    let LegalRookMoves(board:BoardRec, from:int, hv:uint64, d:uint64, c:uint64) =
        let mutable moves = 0UL
        let colBoard = if board.IsWtm then board.White else board.Black
        if Bits.IsSet(d, from) then moves
        else
            let mIndex = Attacks.GetMagicIndex(Rook, board.Both, from)
            moves <- moves ||| Attacks.SlidingMoves[mIndex] &&& ~~~(colBoard) &&& c
            if Bits.IsSet(hv, from) then moves <- moves &&& hv
            moves
    let LegalKnightMoves(board:BoardRec, from:int, hv:uint64, d:uint64, c:uint64) =
        let mutable moves = 0UL
        let colBoard = if board.IsWtm then board.White else board.Black
        if Bits.IsSet(hv, from) || Bits.IsSet(d, from) then moves
        else
            moves <- moves ||| Attacks.KnightMoves[from] &&& ~~~(colBoard) &&& c
            moves
    let LegalBishopMoves(board:BoardRec, from:int, hv:uint64, d:uint64, c:uint64) =
        let color = board.Stm
        let mutable moves = 0UL
        let colBoard = if board.IsWtm then board.White else board.Black
        if Bits.IsSet(hv, from) then moves
        else
            let mIndex = Attacks.GetMagicIndex(Bishop, board.Both, from)
            moves <- moves ||| Attacks.SlidingMoves[mIndex] &&& ~~~(colBoard) &&& c
            if Bits.IsSet(d, from) then moves <- moves &&& d
            moves
    let LegalQueenMoves( board:BoardRec, from:int, hv:uint64, d:uint64, c:uint64) =
        LegalRookMoves(board,from,hv,d,c) 
        ||| LegalBishopMoves(board,from,hv,d,c)
    let LegalKingMoves(board:byref<BoardRec>, from:int, hv:uint64, d:uint64, c:uint64) =
        let mutable moves = 0UL
        let colBoard = if board.IsWtm then board.White else board.Black
        let mutable kingMoves = Attacks.KingMoves[from]
        kingMoves <- kingMoves &&& ~~~(colBoard)
        if kingMoves = 0UL then 
            moves
        else
            let oppColor = board.Xstm
            Board.Empty(&board, from)
            let movearr = Bits.ToArray(kingMoves)
            for move in movearr do
                if UnderAttack(board, move, oppColor) then Bits.PopBit(&kingMoves, move)
            Board.InsertPiece(&board, (if board.IsWtm then WhiteKing else BlackKing), from)
            moves <- moves ||| kingMoves
            if (UnderAttack(board, from, oppColor)) then moves
            else
                let q = if board.IsWtm then board.WhiteQCastle else board.BlackQCastle
                let k = if board.IsWtm then board.WhiteKCastle else board.BlackKCastle
                if (q <> 0x0 && Bits.IsSet(kingMoves, from - 1) && not (UnderAttack(board, from - 2, oppColor))) then
                    let path = if board.IsWtm then WHITE_QUEEN_CASTLE else BLACK_QUEEN_CASTLE
                    let all = board.Both
                    if path &&& all = 0UL then
                        moves <- moves ||| Bits.FromSq(from - 2)
                if (k <> 0x0 && Bits.IsSet(kingMoves, from + 1) && not (UnderAttack(board, from + 2, oppColor))) then
                    let path = if board.IsWtm then WHITE_KING_CASTLE else BLACK_KING_CASTLE
                    let all = board.Both
                    if path &&& all = 0UL then
                        moves <- moves ||| Bits.FromSq(from + 2)
                moves
    let FromFields(board:BoardRec, from:int, horizontalVertical:uint64, diagonal:uint64, checks:uint64, moves:uint64, promotion:bool) =
        {
            Board = board
            From = from
            Hv = horizontalVertical
            D = diagonal
            C = checks
            Moves = moves
            Count = Bits.Count(moves)
            Promotion = promotion
        }
    let ForPawns(board:byref<BoardRec>, from:int, horizontalVertical:uint64, diagonal:uint64, checks:uint64) =
        let moves,promotion = LegalPawnMoveCaptures(&board,from,horizontalVertical,diagonal,checks)
        FromFields(board, from, horizontalVertical, diagonal, checks, moves, promotion)
    let NotDouble(board:byref<BoardRec>, from:int, piece:int, horizontalVertical:uint64, diagonal:uint64, checks:uint64) =
        let mutable promotion = false
        let moves =
            if piece=Pawn then 
                let m,p = LegalPawnMoves(&board,from,horizontalVertical,diagonal,checks)
                promotion <- p
                m
            elif piece=Rook then 
                LegalRookMoves(board,from,horizontalVertical,diagonal,checks)
            elif piece=Knight then 
                LegalKnightMoves(board,from,horizontalVertical,diagonal,checks)
            elif piece=Bishop then 
                LegalBishopMoves(board,from,horizontalVertical,diagonal,checks)
            elif piece=Queen then 
                LegalQueenMoves(board,from,horizontalVertical,diagonal,checks)
            elif piece=King then 
                LegalKingMoves(&board,from,horizontalVertical,diagonal,checks)
            else 
                failwith "Cannot generate move for empty piece"
        FromFields(board, from, horizontalVertical, diagonal, checks, moves, promotion)
    let Double(board:byref<BoardRec>, from:int, horizontalVertical:uint64, diagonal:uint64, checks:uint64, doubleChecked:bool) =
        let colpc = board.Squares[from]
        let piece = colpc/2
        let mutable promotion = false
        if doubleChecked && piece <> King then
            FromFields(board, from, horizontalVertical, diagonal, checks, 0UL, promotion)
        else
            let moves =
                if piece=Pawn then 
                    let m,p = LegalPawnMoves(&board,from,horizontalVertical,diagonal,checks)
                    promotion <- p
                    m
                elif piece=Rook then 
                    LegalRookMoves(board,from,horizontalVertical,diagonal,checks)
                elif piece=Knight then 
                    LegalKnightMoves(board,from,horizontalVertical,diagonal,checks)
                elif piece=Bishop then 
                    LegalBishopMoves(board,from,horizontalVertical,diagonal,checks)
                elif piece=Queen then 
                    LegalQueenMoves(board,from,horizontalVertical,diagonal,checks)
                elif piece=King then 
                    LegalKingMoves(&board,from,horizontalVertical,diagonal,checks)
                else 
                    failwith "Cannot generate move for empty piece" 
            FromFields(board, from, horizontalVertical, diagonal, checks, moves, promotion)
    let ForSq(board:byref<BoardRec>, from:int) =
        let piece, color = ColPiece.ToPcCol(board.Squares[from])
        let oppositeColor = color ^^^ 1
        let kingSq = if color = White then board.WhiteKingLoc else board.BlackKingLoc
        let (horizontalVertical, diagonal) = PinBitBoards(board, kingSq, color, oppositeColor)
        let (checks, doubleChecked) = CheckBitBoard(board, kingSq, oppositeColor)
        Double(&board, from, horizontalVertical, diagonal, checks, doubleChecked)
