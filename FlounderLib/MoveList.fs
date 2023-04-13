namespace FlounderLib
open System.Runtime.CompilerServices
open System

module MoveList =
    let BLACK_KING_CASTLE = 0x60uL
    let WHITE_KING_CASTLE = BLACK_KING_CASTLE <<< 56
    let BLACK_QUEEN_CASTLE = 0xEuL
    let WHITE_QUEEN_CASTLE = BLACK_QUEEN_CASTLE <<< 56
    let UnderAttack(board:Board, sq:int, by:int) =
        // First, we check if the square is being attacked by pawns.
        // To do this, we generate a reverse attack mask, letting our square act as a pawn and seeing if opposing
        // pawns exist on the squares in the mask. If so, our square can be attacked by pawns.
        let pawnAttack = if by = 0 then AttackTable.BlackPawnAttacks.[sq] else AttackTable.WhitePawnAttacks.[sq]
        if (pawnAttack &&& board.Map.Pieces[by]) <> 0UL then true
        // Then, we check if the square is being attacked by knights.
        // To do this, we generate a reverse attack mask, letting our square act as a knight and seeing if opposing
        // knights exist on the squares in the mask. If so, our square can be attacked by knights.
        elif (AttackTable.KnightMoves.[sq] &&& board.Map.Pieces[Knight*2 + by]) <> 0UL then true
        else
            // Next, we check if the square is being attacked by sliding pieces.
            // To do this, first we need to find all occupied squares (by our and opposing pieces).
            let occupied = board.Map.Both
            // We should check queen along with rook/bishop as queen moves are (rook moves | bishop moves).
            let queen = board.Map.Pieces[Queen*2 + by]
            // Generate a reverse attack mask for rook, letting our square act as a rook and seeing if opposing rook or
            // queen exist on the squares in the mask. If so, our square can be attacked by either rook or queen.
            let mutable mIndex = BlackMagicBitBoardFactory.GetMagicIndex(Rook, occupied, sq)
            if (AttackTable.SlidingMoves.[mIndex] &&& (queen ||| board.Map.Pieces[Rook*2 + by])) <> 0UL then true
            else
                // Generate a reverse attack mask for bishop, letting our square act as a rook and seeing if opposing
                // bishop or queen exist on the squares in the mask. If so, our square can be attacked by either bishop
                // or queen.
                mIndex <- BlackMagicBitBoardFactory.GetMagicIndex(Bishop, occupied, sq)
                if (AttackTable.SlidingMoves.[mIndex] &&& (queen ||| board.Map.Pieces[Bishop*2 + by])) <> 0UL then true
                else
                    // Lastly, we check if our square is being attacked by a king.
                    // We generate a reverse attack mask, letting our square act as king and then check if there if opposing
                    // king exists on the squares in the mask. If so, our square can be attacked by king.
                    // Otherwise, this square is completely safe from all pieces.
                     (AttackTable.KingMoves.[sq] &&& board.Map.Pieces[King*2 + by]) <> 0UL
    let CheckBitBoard(board:Board, sq:int, by:int) =
        let mutable count = 0
        let mutable checks = 0UL
        // First we generate a pawn check.
        let pawnAttack = if by = 0 then AttackTable.BlackPawnAttacks.[sq] else AttackTable.WhitePawnAttacks.[sq]
        let pawnCheck = pawnAttack &&& board.Map.Pieces[by]
        // Next, we generate a knight check.
        let knightCheck = AttackTable.KnightMoves.[sq] &&& board.Map.Pieces[Knight*2 + by]
        // For sliding pieces, we use a BitBoard of all pieces.
        let occupied = board.Map.Both
        // We will reference the queen along with rooks and bishops for the checks.
        let queen = board.Map.Pieces[Queen*2 + by]
        // Now, we generate a rook or queen (straight only) check.
        let mutable mIndex = BlackMagicBitBoardFactory.GetMagicIndex(Rook, occupied, sq)
        let rookQueenCheck = AttackTable.SlidingMoves.[mIndex] &&& (queen ||| board.Map.Pieces[Rook*2 + by])
        // Next, we generate a bishop or queen (diagonal only) check.
        mIndex <- BlackMagicBitBoardFactory.GetMagicIndex(Bishop, occupied, sq)
        let bishopQueenCheck = AttackTable.SlidingMoves.[mIndex] &&& (queen ||| board.Map.Pieces[Bishop*2 + by])
        if pawnCheck <> 0UL then
            // If there is a pawn check, we must add it to the checks and raise the check count.
            checks <- checks ||| pawnCheck
            count<-count+1
        elif knightCheck <> 0UL then
            // Otherwise, if there is a knight check, we must add it to the checks and raise the check count.
            checks <- checks ||| knightCheck
            count<-count+1
        if rookQueenCheck <> 0UL then
            // If there is a rook-queen check hit, we must add in the checks as well as the path from square to
            // rook or queen.
            let rqSq = Bits.ToInt(rookQueenCheck)
            checks <- checks ||| UtilityTable.Between.[sq].[int(rqSq)] ||| Bits.FromSq(rqSq)
            count<-count+1
            // In the case where pawn promotes to queen or rook, we have a secondary check as well.
            if Bits.Count(rookQueenCheck) > 1 then count<-count+1
        if bishopQueenCheck <> 0UL then
            // If there is a bishop-queen check hit, we must add in the checks as well as the path from square to
            // bishop or queen.
            let bqSq:int = Bits.ToInt(bishopQueenCheck)
            checks <- checks ||| UtilityTable.Between.[sq].[int(bqSq)] ||| Bits.FromSq(bqSq)
            count<-count+1
        
        if checks = 0UL then checks <- UInt64.MaxValue

        (checks, count > 1)
    let PinBitBoards(board:Board, sq:int, us:int, by:int) =
        // Unlike for all other boards and checks, we don't use a fully occupied board. We want our paths to go through
        // our pieces so we only consider board occupied by opposing.
        let byBoard = if by = White then board.Map.White else board.Map.Black
        let usBoard = if us = White then board.Map.White else board.Map.Black
        // We will reference the queen along with rooks and bishops for the checks.
        let queen = board.Map.Pieces[Queen*2 + by]
        // First, we generate all rook / queen (straight only) attacks.
        let mutable mIndex = BlackMagicBitBoardFactory.GetMagicIndex(Rook, byBoard, sq)
        let rookQueenCheck = AttackTable.SlidingMoves.[mIndex] &&& (queen ||| board.Map.Pieces[Rook*2 + by])
        // Next, we generate all bishop / queen (diagonal only) attacks.
        mIndex <- BlackMagicBitBoardFactory.GetMagicIndex(Bishop, byBoard, sq)
        let bishopQueenCheck = AttackTable.SlidingMoves.[mIndex] &&& (queen ||| board.Map.Pieces[Bishop*2 + by])
        let mutable horizontalVerticalPin = 0UL
        let mutable diagonalPin = 0UL
        // Iterate over the rooks and queens (pinning straight).
        let rqSqarr = Bits.ToArray(rookQueenCheck)
        let dorqSq rqSq =
            let possiblePin = UtilityTable.Between.[sq].[rqSq] ||| Bits.FromSq(rqSq)
            if Bits.Count(possiblePin &&& usBoard) = 1 then horizontalVerticalPin <- horizontalVerticalPin ||| possiblePin
        Array.iter dorqSq rqSqarr
        // Iterate over the bishops and queens (pinning diagonally).
        let bqSqarr = Bits.ToArray(bishopQueenCheck)
        let dobqSq bqSq =
            let possiblePin = UtilityTable.Between.[sq].[bqSq] ||| Bits.FromSq(bqSq)
            if Bits.Count(possiblePin &&& usBoard) = 1 then diagonalPin <- diagonalPin ||| possiblePin
        Array.iter dobqSq bqSqarr
        (horizontalVerticalPin, diagonalPin)
    let LegalPawnMoveCaptures(iswtm:bool, board:Board, from:int, hv:uint64, d:uint64, c:uint64) =
        let color = if iswtm then 0 else 1
        let mutable moves = 0UL
        if Bits.IsSet(hv, from) then
            // If pawn is horizontally pinned, then we have no moves.
            moves,false
        else 
            let oppositeColor = color^^^1
            let opposite = if iswtm then board.Map.Black else board.Map.White
            
            let mutable epPieceSq = Na
            // Promotion Flag
            // If we're at rank 7 for white or rank 1 for black, we should set the promotion flag to true.
            // It is important to set it earlier rather than later, because if there is a diagonal pin capture
            // leading to a promotion, we must make sure to record that as 4 moves.
            // FEN: 2q5/1Pp5/K2p4/7r/6Pk/8/8/1R6 w - -
            let promotion = color = 0 && from < A6 &&  from > H8 || 
                            color = 1 && from < A1 && from > H3
            // Attack moves
            // En Passant.
            if board.Map.EnPassantTarget <> Na then
                // If EP exists, then we need to check if a piece exists on square that's under attack from Ep, not
                // where we move to.
                epPieceSq <- if color = 0 then board.Map.EnPassantTarget + 8 else board.Map.EnPassantTarget - 8
                let epTargetPieceExists =  Bits.IsSet(board.Map.Pieces[oppositeColor], epPieceSq)
                // We need to check if a piece of ours exists to actually execute the EP.
                // We do this by running a reverse pawn mask, to determine whether a piece of ours is on the corner.
                let reverseCorner = if color = 0 then AttackTable.BlackPawnAttacks.[board.Map.EnPassantTarget] else AttackTable.WhitePawnAttacks[board.Map.EnPassantTarget]
                if (epTargetPieceExists && Bits.IsSet(reverseCorner, from)) then
                    // If both the enemy EP piece and our piece that can theoretically EP exist...
                    moves <- moves ||| Bits.FromSq(board.Map.EnPassantTarget)
            // Attack Moves.
            let attack = if color = 0 then AttackTable.WhitePawnAttacks.[from] else AttackTable.BlackPawnAttacks.[from]
            // Make sure attacks are only on opposite pieces (and not on empty squares or squares occupied by
            // our pieces).
            moves <- moves ||| (attack &&& opposite &&& c)
            if Bits.IsSet(d, from) then
                // If pawn is pinned diagonally, we can only do attacks and EP on the pin.
                moves <- moves &&& d
                moves,promotion
            else
                //Special EP case
                if (epPieceSq <> Na) then
                    // If the pawn isn't pinned diagonally or horizontally/vertically, we must do one final check for EP:
                    // In the rare EP-pin position: 8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - -
                    // If we do EP here, our king can be attacked by rook.
                    // This is known as being pinned through a piece and only happens for EP, thus we must actually EP and see
                    // if our king is under attacked.
                    BitBoardMap.Empty(&board.Map, from)
                    BitBoardMap.Empty(&board.Map, epPieceSq)
                    BitBoardMap.InsertPiece(&board.Map, color, board.Map.EnPassantTarget)
                    let kingSq = if color = White then board.Map.WhiteKingLoc else board.Map.BlackKingLoc
                    // If our king is under attack, it means the pawn was pinned through a piece and the removal of that piece
                    // caused a discovered pin. Thus, we must remove it from our legal moves.
                    if (UnderAttack(board, kingSq, oppositeColor)) then moves <- moves &&& ~~~(1UL <<< board.Map.EnPassantTarget)
                    BitBoardMap.InsertPiece(&board.Map, color, from)
                    BitBoardMap.InsertPiece(&board.Map, oppositeColor, epPieceSq)
                    BitBoardMap.Empty(&board.Map, board.Map.EnPassantTarget)
                    // In the case that the EP piece isn't in our checks during a check, we shouldn't EP.
                    if Bits.IsSet(moves, board.Map.EnPassantTarget) && not (Bits.IsSet(c, epPieceSq)) then moves <- moves &&& ~~~(1UL <<< board.Map.EnPassantTarget)
                    moves,promotion
                else
                    moves,promotion                
    let LegalPawnMoves(color:int, board:Board, from:int, hv:uint64, d:uint64, c:uint64) =
        let mutable moves = 0UL
        let oppositeColor = color^^^1
        let colBoard = if color = White then board.Map.White else board.Map.Black
        let opposite = if color = White then board.Map.Black else board.Map.White
        let mutable epPieceSq = Na
        //Promotion Flag
        // If we're at rank 7 for white or rank 1 for black, we should set the promotion flag to true.
        // It is important to set it earlier rather than later, because if there is a diagonal pin capture
        // leading to a promotion, we must make sure to record that as 4 moves.
        // FEN: 2q5/1Pp5/K2p4/7r/6Pk/8/8/1R6 w - -
        let promotion = color = 0 && from < A6 &&  from > H8 || 
                        color = 1 && from < A1 && from > H3
        // Attack moves
        // En Passant.
        if board.Map.EnPassantTarget <> Na then
            // If EP exists, then we need to check if a piece exists on square that's under attack from Ep, not
            // where we move to.
            epPieceSq <- if color = 0 then board.Map.EnPassantTarget + 8 else board.Map.EnPassantTarget - 8
            let epTargetPieceExists = Bits.IsSet(board.Map.Pieces[oppositeColor], epPieceSq)
            // We need to check if a piece of ours exists to actually execute the EP.
            // We do this by running a reverse pawn mask, to determine whether a piece of ours is on the corner.
            let reverseCorner = if color = 0 then AttackTable.BlackPawnAttacks.[board.Map.EnPassantTarget] else AttackTable.WhitePawnAttacks[board.Map.EnPassantTarget]
            if (epTargetPieceExists && Bits.IsSet(reverseCorner, from)) then
                // If both the enemy EP piece and our piece that can theoretically EP exist...
                moves <- moves ||| Bits.FromSq(board.Map.EnPassantTarget)
        // Attack Moves.
        let attack = if color = 0 then AttackTable.WhitePawnAttacks.[from] else AttackTable.BlackPawnAttacks.[from]
       // Make sure attacks are only on opposite pieces (and not on empty squares or squares occupied by
        // our pieces).
        moves <- moves ||| (attack &&& opposite &&& c)
        if Bits.IsSet(d, from) then
            // If pawn is pinned diagonally, we can only do attacks and EP on the pin.
            moves <- moves &&& d
            moves,promotion
        else
            //Normal moves
            let mutable pushes = 0UL
            // Push pawn once.
            pushes <- pushes ||| (if color = 0 then Bits.FromSq(from) >>> 8 else Bits.FromSq(from) <<< 8) &&& ~~~board.Map.Both
            if ((from < A1 && from > H3 || from < A6 && from > H8) && pushes <> 0UL) then
                // If we are on the starting pawn position & the first pawn push was successful.
                // Push once more.
                pushes <- pushes ||| if color = 0 then Bits.FromSq(from) >>> 16 else Bits.FromSq(from) <<< 16
            // Make sure our pushes are not stepping on to enemy pieces.
            // These are normal moves, not attack moves so we can't capture.
            pushes <- pushes &&& ~~~(opposite) &&& ~~~(colBoard)
            moves <- moves ||| (pushes &&& c)
            if Bits.IsSet(hv, from) then
                // If pawn is horizontally pinned, then we have no moves.
                // However, if pawn is vertically pinned, then we can at least do pushes.
                moves <- moves &&& hv
                moves,promotion
            else
                //Special EP case
                if epPieceSq <> Na then
                    // If the pawn isn't pinned diagonally or horizontally/vertically, we must do one final check for EP:
                    // In the rare EP-pin position: 8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - -
                    // If we do EP here, our king can be attacked by rook.
                    // This is known as being pinned through a piece and only happens for EP, thus we must actually EP and see
                    // if our king is under attacked.
                    BitBoardMap.Empty(&board.Map, from)
                    BitBoardMap.Empty(&board.Map, epPieceSq)
                    BitBoardMap.InsertPiece(&board.Map, color, board.Map.EnPassantTarget)
                    let kingSq = if color = White then board.Map.WhiteKingLoc else board.Map.BlackKingLoc
                    // If our king is under attack, it means the pawn was pinned through a piece and the removal of that piece
                    // caused a discovered pin. Thus, we must remove it from our legal moves.
                    if (UnderAttack(board, kingSq, oppositeColor)) then moves <- moves &&& ~~~(1UL <<< board.Map.EnPassantTarget)
                    BitBoardMap.InsertPiece(&board.Map, color, from)
                    BitBoardMap.InsertPiece(&board.Map, oppositeColor, epPieceSq)
                    BitBoardMap.Empty(&board.Map, board.Map.EnPassantTarget)
                    // In the case that the EP piece isn't in our checks during a check, we shouldn't EP.
                    if Bits.IsSet(moves, board.Map.EnPassantTarget) && not (Bits.IsSet(c, epPieceSq)) then moves <- moves &&& ~~~(1UL <<< board.Map.EnPassantTarget)
                    moves,promotion
                else
                    moves,promotion
    let LegalRookMoves(color:int, board:Board, from:int, hv:uint64, d:uint64, c:uint64) =
        let mutable moves = 0UL
        let colBoard = if color = White then board.Map.White else board.Map.Black
        // If rook is diagonally pinned, it has no moves.
        if Bits.IsSet(d, from) then moves
        else
            // Calculate pseudo-legal moves within check board.
            let mIndex = BlackMagicBitBoardFactory.GetMagicIndex(Rook, board.Map.Both, from)
            moves <- moves ||| AttackTable.SlidingMoves.[mIndex] &&& ~~~(colBoard) &&& c
            // If rook is horizontally or vertically pinned, it can only move within the pin.
            if Bits.IsSet(hv, from) then moves <- moves &&& hv
            moves
    let LegalKnightMoves(color:int, board:Board, from:int, hv:uint64, d:uint64, c:uint64) =
        let mutable moves = 0UL
        let colBoard = if color = White then board.Map.White else board.Map.Black
        if Bits.IsSet(hv, from) || Bits.IsSet(d, from) then moves
        else
            moves <- moves ||| AttackTable.KnightMoves.[from] &&& ~~~(colBoard) &&& c
            moves
    let LegalBishopMoves(color:int, board:Board, from:int, hv:uint64, d:uint64, c:uint64) =
        let mutable moves = 0UL
        let colBoard = if color = White then board.Map.White else board.Map.Black
        // If bishop is horizontally or vertically pinned, it has no moves.
        if Bits.IsSet(hv, from) then moves
        else
            // Calculate pseudo-legal moves within check board.
            let mIndex = BlackMagicBitBoardFactory.GetMagicIndex(Bishop, board.Map.Both, from)
            moves <- moves ||| AttackTable.SlidingMoves.[mIndex] &&& ~~~(colBoard) &&& c
            // If bishop is diagonally pinned, it can only move within the pin.
            if Bits.IsSet(d, from) then moves <- moves &&& d
            moves
    let LegalQueenMoves(color:int, board:Board, from:int, hv:uint64, d:uint64, c:uint64) =
        LegalRookMoves(color,board,from,hv,d,c) 
        ||| LegalBishopMoves(color,board,from,hv,d,c)
    let LegalKingMoves(color:int, board:Board, from:int, hv:uint64, d:uint64, c:uint64) =
        let mutable moves = 0UL
        let colBoard = if color = White then board.Map.White else board.Map.Black
        // Normal
        let mutable kingMoves = AttackTable.KingMoves.[from]
        kingMoves <- kingMoves &&& ~~~(colBoard)
        // If we have no king moves, we can return earlier and avoiding having to check if the moves are legal
        // or not by removing the king.
        if kingMoves = 0UL then 
            moves
        else
            let ioppositeColor = color^^^1
            BitBoardMap.Empty(&board.Map, from)
            let movearr = Bits.ToArray(kingMoves)
            Array.iter (fun move -> if (UnderAttack(board, move, ioppositeColor)) then Bits.PopBit(&kingMoves, move)) movearr
            BitBoardMap.InsertPiece(&board.Map, (if color=0 then WhiteKing else BlackKing), from)
            moves <- moves ||| kingMoves
            // Castling
            // If enemy is attacking our king, we cannot castle.
            if (UnderAttack(board, from, ioppositeColor)) then moves
            else
                // Get castling rights.
                let q = if color = White then board.Map.WhiteQCastle else board.Map.BlackQCastle
                let k = if color = White then board.Map.WhiteKCastle else board.Map.BlackKCastle
                // Make sure castling close-path isn't under attack.
                if (q <> 0x0 && Bits.IsSet(kingMoves, from - 1) && not (UnderAttack(board, from - 2, ioppositeColor))) then
                    // Generate path of castle queen-side.
                    let path = if color = 0 then WHITE_QUEEN_CASTLE else BLACK_QUEEN_CASTLE
                    // If path is empty, we can castle.
                    let all = board.Map.Both
                    if (path &&& all) = 0UL then
                        moves <- moves ||| Bits.FromSq(from - 2)
                // Make sure castling close-path isn't under attack.
                if (k <> 0x0 && Bits.IsSet(kingMoves, from + 1) && not (UnderAttack(board, from + 2, ioppositeColor))) then
                    // Generate path of castle king-side.
                    let path = if color = 0 then WHITE_KING_CASTLE else BLACK_KING_CASTLE
                    // If path is empty, we can castle.
                    let all = board.Map.Both
                    if (path &&& all) = 0UL then
                        moves <- moves ||| Bits.FromSq(from + 2)
                moves

[<IsByRefLike>]
type MoveList =
    struct
        val Board:Board
        val From:int
        val Hv:uint64
        val D:uint64
        val C:uint64
        val Moves:uint64
        val Count:int
        val Promotion:bool
        new(board:Board, from:int, horizontalVertical:uint64, diagonal:uint64, checks:uint64, moves:uint64, promotion:bool) =
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
        new(board:Board, from:int) =
            let piece, color = ColPiece.ToPcCol(board.Map.Squares[from])
            let oppositeColor = color ^^^ 1
            let kingSq = if color = White then board.Map.WhiteKingLoc else board.Map.BlackKingLoc
            let (horizontalVertical, diagonal) = MoveList.PinBitBoards(board, kingSq, color, oppositeColor)
            let (checks, doubleChecked) = MoveList.CheckBitBoard(board, kingSq, oppositeColor)
            new MoveList(board, from, piece, color, horizontalVertical, diagonal, checks, doubleChecked)
        new(board:Board, from:int, piece:int, color:int, horizontalVertical:uint64, diagonal:uint64, checks:uint64, doubleChecked:bool) =
            let mutable promotion = false
            // If we're double-checked (discovered check + normal check), only the king can move. Thus, we can return
            // early here.
            if doubleChecked && piece <> King then
                new MoveList(board, from, horizontalVertical, diagonal, checks, 0UL, promotion)
            else
                let moves =
                    if piece=Pawn then 
                        let m,p = MoveList.LegalPawnMoves(color,board,from,horizontalVertical,diagonal,checks)
                        promotion <- p
                        m
                    elif piece=Rook then 
                        MoveList.LegalRookMoves(color,board,from,horizontalVertical,diagonal,checks)
                    elif piece=Knight then 
                        MoveList.LegalKnightMoves(color,board,from,horizontalVertical,diagonal,checks)
                    elif piece=Bishop then 
                        MoveList.LegalBishopMoves(color,board,from,horizontalVertical,diagonal,checks)
                    elif piece=Queen then 
                        MoveList.LegalQueenMoves(color,board,from,horizontalVertical,diagonal,checks)
                    elif piece=King then 
                        MoveList.LegalKingMoves(color,board,from,horizontalVertical,diagonal,checks)
                    else 
                        raise (InvalidMoveLookupException.FromBoard(board,"Cannot generate move for empty piece: " + from.ToString()))
                new MoveList(board, from, horizontalVertical, diagonal, checks, moves, promotion)
        new(board:Board, from:int, piece:int, color:int, horizontalVertical:uint64, diagonal:uint64, checks:uint64) =
            let mutable promotion = false
            let moves =
                if piece=Pawn then 
                    let m,p = MoveList.LegalPawnMoves(color,board,from,horizontalVertical,diagonal,checks)
                    promotion <- p
                    m
                elif piece=Rook then 
                    MoveList.LegalRookMoves(color,board,from,horizontalVertical,diagonal,checks)
                elif piece=Knight then 
                    MoveList.LegalKnightMoves(color,board,from,horizontalVertical,diagonal,checks)
                elif piece=Bishop then 
                    MoveList.LegalBishopMoves(color,board,from,horizontalVertical,diagonal,checks)
                elif piece=Queen then 
                    MoveList.LegalQueenMoves(color,board,from,horizontalVertical,diagonal,checks)
                elif piece=King then 
                    MoveList.LegalKingMoves(color,board,from,horizontalVertical,diagonal,checks)
                else 
                    raise (InvalidMoveLookupException.FromBoard(board,"Cannot generate move for empty piece: " + from.ToString()))
            new MoveList(board, from, horizontalVertical, diagonal, checks, moves, promotion)
        new(board:Board, from:int, horizontalVertical:uint64, diagonal:uint64, checks:uint64) =
            //this can only be used when processing pawn captiures
            let moves,promotion = MoveList.LegalPawnMoveCaptures(board.Map.IsWtm,board,from,horizontalVertical,diagonal,checks)
            new MoveList(board, from, horizontalVertical, diagonal, checks, moves, promotion)
  
    end
    