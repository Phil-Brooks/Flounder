namespace FlounderLib
open System.Runtime.CompilerServices

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
        if (pawnAttack &&& board.All(Pawn, by)).ToBool() then true
        // Then, we check if the square is being attacked by knights.
        // To do this, we generate a reverse attack mask, letting our square act as a knight and seeing if opposing
        // knights exist on the squares in the mask. If so, our square can be attacked by knights.
        elif (AttackTable.KnightMoves.[sq] &&& board.All(Knight, by)).ToBool() then true
        else
            // Next, we check if the square is being attacked by sliding pieces.
            // To do this, first we need to find all occupied squares (by our and opposing pieces).
            let occupied = ~~~(board.All(2))
            // We should check queen along with rook/bishop as queen moves are (rook moves | bishop moves).
            let queen = board.All(Queen, by)
            // Generate a reverse attack mask for rook, letting our square act as a rook and seeing if opposing rook or
            // queen exist on the squares in the mask. If so, our square can be attacked by either rook or queen.
            let mutable mIndex = BlackMagicBitBoardFactory.GetMagicIndex(Rook, occupied, sq)
            if (AttackTable.SlidingMoves.[mIndex] &&& (queen ||| board.All(Rook, by))).ToBool() then true
            else
                // Generate a reverse attack mask for bishop, letting our square act as a rook and seeing if opposing
                // bishop or queen exist on the squares in the mask. If so, our square can be attacked by either bishop
                // or queen.
                mIndex <- BlackMagicBitBoardFactory.GetMagicIndex(Bishop, occupied, sq)
                if (AttackTable.SlidingMoves.[mIndex] &&& (queen ||| board.All(Bishop, by))).ToBool() then true
                else
                    // Lastly, we check if our square is being attacked by a king.
                    // We generate a reverse attack mask, letting our square act as king and then check if there if opposing
                    // king exists on the squares in the mask. If so, our square can be attacked by king.
                    // Otherwise, this square is completely safe from all pieces.
                     (AttackTable.KingMoves.[sq] &&& board.All(King, by)).ToBool()
    let CheckBitBoard(board:Board, sq:int, by:int) =
        let mutable count = 0
        let mutable checks = BitBoard.Default
        // First we generate a pawn check.
        let pawnAttack = if by = 0 then AttackTable.BlackPawnAttacks.[sq] else AttackTable.WhitePawnAttacks.[sq]
        let pawnCheck = pawnAttack &&& board.All(Pawn, by)
        // Next, we generate a knight check.
        let knightCheck = AttackTable.KnightMoves.[sq] &&& board.All(Knight, by)
        // For sliding pieces, we use a BitBoard of all pieces.
        let occupied = ~~~(board.All(2))
        // We will reference the queen along with rooks and bishops for the checks.
        let queen = board.All(Queen, by)
        // Now, we generate a rook or queen (straight only) check.
        let mutable mIndex = BlackMagicBitBoardFactory.GetMagicIndex(Rook, occupied, sq)
        let rookQueenCheck = AttackTable.SlidingMoves.[mIndex] &&& (queen ||| board.All(Rook, by))
        // Next, we generate a bishop or queen (diagonal only) check.
        mIndex <- BlackMagicBitBoardFactory.GetMagicIndex(Bishop, occupied, sq)
        let bishopQueenCheck = AttackTable.SlidingMoves.[mIndex] &&& (queen ||| board.All(Bishop, by))
        if pawnCheck.ToBool() then
            // If there is a pawn check, we must add it to the checks and raise the check count.
            checks <- checks ||| pawnCheck
            count<-count+1
        elif knightCheck.ToBool() then
            // Otherwise, if there is a knight check, we must add it to the checks and raise the check count.
            checks <- checks ||| knightCheck
            count<-count+1
        if rookQueenCheck.ToBool() then
            // If there is a rook-queen check hit, we must add in the checks as well as the path from square to
            // rook or queen.
            let rqSq:int = rookQueenCheck.ToSq()
            checks <- checks ||| UtilityTable.Between.[sq].[int(rqSq)] ||| BitBoard.FromSq(rqSq)
            count<-count+1
            // In the case where pawn promotes to queen or rook, we have a secondary check as well.
            if (rookQueenCheck.Count > 1) then count<-count+1
        if bishopQueenCheck.ToBool() then
            // If there is a bishop-queen check hit, we must add in the checks as well as the path from square to
            // bishop or queen.
            let bqSq:int = bishopQueenCheck.ToSq()
            checks <- checks ||| UtilityTable.Between.[sq].[int(bqSq)] ||| BitBoard.FromSq(bqSq)
            count<-count+1
        
        if (checks = BitBoard.Default) then checks <- BitBoard.Filled

        (checks, count > 1)
    let PinBitBoards(board:Board, sq:int, us:int, by:int) =
        // Unlike for all other boards and checks, we don't use a fully occupied board. We want our paths to go through
        // our pieces so we only consider board occupied by opposing.
        let byBoard = board.All(by)
        // We will reference the queen along with rooks and bishops for the checks.
        let queen = board.All(Queen, by)
        // First, we generate all rook / queen (straight only) attacks.
        let mutable mIndex = BlackMagicBitBoardFactory.GetMagicIndex(Rook, byBoard, sq)
        let rookQueenCheck = AttackTable.SlidingMoves.[mIndex] &&& (queen ||| board.All(Rook, by))
        // Next, we generate all bishop / queen (diagonal only) attacks.
        mIndex <- BlackMagicBitBoardFactory.GetMagicIndex(Bishop, byBoard, sq)
        let bishopQueenCheck = AttackTable.SlidingMoves.[mIndex] &&& (queen ||| board.All(Bishop, by))
        let mutable horizontalVerticalPin = BitBoard.Default
        let mutable diagonalPin = BitBoard.Default
        // Iterate over the rooks and queens (pinning straight).
        let mutable rookQueenIterator = rookQueenCheck.GetEnumerator()
        let mutable rqSq = rookQueenIterator.Current
        while (rookQueenIterator.MoveNext()) do
            let rqS = int(rqSq)
            let possiblePin = UtilityTable.Between.[sq].[rqS] ||| BitBoard.FromSq(rqSq)
            if ((possiblePin &&& board.All(us)).Count = 1) then horizontalVerticalPin <- horizontalVerticalPin ||| possiblePin
            // Next square iteration.
            rqSq <- rookQueenIterator.Current
        // Iterate over the bishops and queens (pinning diagonally).
        let mutable bishopQueenIterator = bishopQueenCheck.GetEnumerator()
        let mutable bqSq = bishopQueenIterator.Current
        while (bishopQueenIterator.MoveNext()) do
            let possiblePin = UtilityTable.Between.[sq].[bqSq] ||| BitBoard.FromSq(bqSq)
            if ((possiblePin &&& board.All(us)).Count = 1) then diagonalPin <- diagonalPin ||| possiblePin
            // Next square iteration.
            bqSq <- bishopQueenIterator.Current
        (horizontalVerticalPin, diagonalPin)
    let LegalPawnMoveCaptures(color:int, board:Board, from:int, hv:BitBoard, d:BitBoard, c:BitBoard) =
        let mutable moves = BitBoard.Default
        if hv.[from] then
            // If pawn is horizontally pinned, then we have no moves.
            moves,false
        else 
            let oppositeColor = color^^^1
            let opposite = board.All(oppositeColor)
            
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
            if board.EnPassantTarget <> Na then
                // If EP exists, then we need to check if a piece exists on square that's under attack from Ep, not
                // where we move to.
                epPieceSq <- if color = 0 then board.EnPassantTarget + 8 else board.EnPassantTarget - 8
                let epTargetPieceExists = board.All(Pawn, oppositeColor).[epPieceSq]
                // We need to check if a piece of ours exists to actually execute the EP.
                // We do this by running a reverse pawn mask, to determine whether a piece of ours is on the corner.
                let reverseCorner = if color = 0 then AttackTable.BlackPawnAttacks.[int(board.EnPassantTarget)] else AttackTable.WhitePawnAttacks[int(board.EnPassantTarget)]
                if (epTargetPieceExists && reverseCorner.[from]) then
                    // If both the enemy EP piece and our piece that can theoretically EP exist...
                    moves <- moves ||| BitBoard.FromSq(board.EnPassantTarget)
            // Attack Moves.
            let attack = if color = 0 then AttackTable.WhitePawnAttacks.[int(from)] else AttackTable.BlackPawnAttacks.[int(from)]
            // Make sure attacks are only on opposite pieces (and not on empty squares or squares occupied by
            // our pieces).
            moves <- moves ||| (attack &&& opposite &&& c)
            if (d.[from]) then
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
                    board.RemovePiece(color, from)
                    board.RemovePiece(oppositeColor, epPieceSq)
                    board.InsertPiece(color, board.EnPassantTarget)
                    let kingSq = board.KingLoc(color).ToSq()
                    // If our king is under attack, it means the pawn was pinned through a piece and the removal of that piece
                    // caused a discovered pin. Thus, we must remove it from our legal moves.
                    if (UnderAttack(board, kingSq, oppositeColor)) then moves <- moves &&& ~~~(BitBoard(1UL) <<< int(board.EnPassantTarget))
                    board.InsertPiece(color, from)
                    board.InsertPiece(oppositeColor, epPieceSq)
                    board.RemovePiece(color, board.EnPassantTarget)
                    // In the case that the EP piece isn't in our checks during a check, we shouldn't EP.
                    if (moves.[board.EnPassantTarget] && not c.[epPieceSq]) then moves <- moves &&& ~~~(BitBoard(1UL) <<< int(board.EnPassantTarget))
                    moves,promotion
                else
                    moves,promotion                
    let LegalPawnMoves(color:int, board:Board, from:int, hv:BitBoard, d:BitBoard, c:BitBoard) =
        let mutable moves = BitBoard.Default
        let oppositeColor = color^^^1
        let opposite = board.All(oppositeColor)
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
        if board.EnPassantTarget <> Na then
            // If EP exists, then we need to check if a piece exists on square that's under attack from Ep, not
            // where we move to.
            epPieceSq <- if color = 0 then board.EnPassantTarget + 8 else board.EnPassantTarget - 8
            let epTargetPieceExists = board.All(Pawn, oppositeColor).[epPieceSq]
            // We need to check if a piece of ours exists to actually execute the EP.
            // We do this by running a reverse pawn mask, to determine whether a piece of ours is on the corner.
            let reverseCorner = if color = 0 then AttackTable.BlackPawnAttacks.[board.EnPassantTarget] else AttackTable.WhitePawnAttacks[board.EnPassantTarget]
            if (epTargetPieceExists && reverseCorner.[from]) then
                // If both the enemy EP piece and our piece that can theoretically EP exist...
                moves <- moves ||| BitBoard.FromSq(board.EnPassantTarget)
        // Attack Moves.
        let attack = if color = 0 then AttackTable.WhitePawnAttacks.[from] else AttackTable.BlackPawnAttacks.[from]
       // Make sure attacks are only on opposite pieces (and not on empty squares or squares occupied by
        // our pieces).
        moves <- moves ||| (attack &&& opposite &&& c)
        if (d.[from]) then
            // If pawn is pinned diagonally, we can only do attacks and EP on the pin.
            moves <- moves &&& d
            moves,promotion
        else
            //Normal moves
            let mutable pushes = BitBoard.Default
            // Push pawn once.
            pushes <- pushes ||| (if color = 0 then BitBoard.FromSq(from) >>> 8 else BitBoard.FromSq(from) <<< 8) &&& board.All(2)
            if ((from < A1 && from > H3 || from < A6 && from > H8) && pushes.ToBool()) then
                // If we are on the starting pawn position & the first pawn push was successful.
                // Push once more.
                pushes <- pushes ||| if color = 0 then BitBoard.FromSq(from) >>> 16 else BitBoard.FromSq(from) <<< 16
            // Make sure our pushes are not stepping on to enemy pieces.
            // These are normal moves, not attack moves so we can't capture.
            pushes <- pushes &&& ~~~(opposite) &&& ~~~(board.All(color))
            moves <- moves ||| (pushes &&& c)
            if hv.[from] then
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
                    board.RemovePiece(color, from)
                    board.RemovePiece(oppositeColor, epPieceSq)
                    board.InsertPiece(color, board.EnPassantTarget)
                    let kingSq = board.KingLoc(color).ToSq()
                    // If our king is under attack, it means the pawn was pinned through a piece and the removal of that piece
                    // caused a discovered pin. Thus, we must remove it from our legal moves.
                    if (UnderAttack(board, kingSq, oppositeColor)) then moves <- moves &&& ~~~(BitBoard(1UL) <<< int(board.EnPassantTarget))
                    board.InsertPiece(color, from)
                    board.InsertPiece(oppositeColor, epPieceSq)
                    board.RemovePiece(color, board.EnPassantTarget)
                    // In the case that the EP piece isn't in our checks during a check, we shouldn't EP.
                    if (moves.[board.EnPassantTarget] && not c.[epPieceSq]) then moves <- moves &&& ~~~(BitBoard(1UL) <<< int(board.EnPassantTarget))
                    moves,promotion
                else
                    moves,promotion
    let LegalRookMoves(color:int, board:Board, from:int, hv:BitBoard, d:BitBoard, c:BitBoard) =
        let mutable moves = BitBoard.Default
        // If rook is diagonally pinned, it has no moves.
        if d.[from] then moves
        else
            // Calculate pseudo-legal moves within check board.
            let mIndex = BlackMagicBitBoardFactory.GetMagicIndex(Rook, ~~~(board.All(2)), from)
            moves <- moves ||| AttackTable.SlidingMoves.[mIndex] &&& ~~~(board.All(color)) &&& c
            // If rook is horizontally or vertically pinned, it can only move within the pin.
            if hv.[from] then moves <- moves &&& hv
            moves
    let LegalKnightMoves(color:int, board:Board, from:int, hv:BitBoard, d:BitBoard, c:BitBoard) =
        let mutable moves = BitBoard.Default
        if hv.[from] || d.[from] then moves
        else
            moves <- moves ||| AttackTable.KnightMoves.[from] &&& ~~~(board.All(color)) &&& c
            moves
    let LegalBishopMoves(color:int, board:Board, from:int, hv:BitBoard, d:BitBoard, c:BitBoard) =
        let mutable moves = BitBoard.Default
        // If bishop is horizontally or vertically pinned, it has no moves.
        if (hv.[from]) then moves
        else
            // Calculate pseudo-legal moves within check board.
            let mIndex = BlackMagicBitBoardFactory.GetMagicIndex(Bishop, ~~~(board.All(2)), from)
            moves <- moves ||| AttackTable.SlidingMoves.[mIndex] &&& ~~~(board.All(color)) &&& c
            // If bishop is diagonally pinned, it can only move within the pin.
            if (d.[from]) then moves <- moves &&& d
            moves
    let LegalQueenMoves(color:int, board:Board, from:int, hv:BitBoard, d:BitBoard, c:BitBoard) =
        LegalRookMoves(color,board,from,hv,d,c) 
        ||| LegalBishopMoves(color,board,from,hv,d,c)
    let LegalKingMoves(color:int, board:Board, from:int, hv:BitBoard, d:BitBoard, c:BitBoard) =
        let mutable moves = BitBoard.Default
        // Normal
        let mutable kingMoves = AttackTable.KingMoves.[from]
        kingMoves <- kingMoves &&& ~~~(board.All(color))
        // If we have no king moves, we can return earlier and avoiding having to check if the moves are legal
        // or not by removing the king.
        if not (kingMoves.ToBool()) then 
            moves
        else
            let ioppositeColor = color^^^1
            let mutable kingMovesIterator = kingMoves.GetEnumerator()
            let mutable move = kingMovesIterator.Current
            board.RemovePiece((if color=0 then WhiteKing else BlackKing), from)
            while (kingMovesIterator.MoveNext()) do
                if (UnderAttack(board, move, ioppositeColor)) then kingMoves.[move] <- false
                // Next square iteration.
                move <- kingMovesIterator.Current
            board.InsertPiece((if color=0 then WhiteKing else BlackKing), from)
            moves <- moves ||| kingMoves
            // Castling
            // If enemy is attacking our king, we cannot castle.
            if (UnderAttack(board, from, ioppositeColor)) then moves
            else
                // Get castling rights.
                let q, k = board.CastlingRight(color)
                // Make sure castling close-path isn't under attack.
                if (q <> 0x0 && kingMoves.[int(from) - 1] && not (UnderAttack(board, from - 2, ioppositeColor))) then
                    // Generate path of castle queen-side.
                    let path:BitBoard = if color = 0 then BitBoard(WHITE_QUEEN_CASTLE) else BitBoard(BLACK_QUEEN_CASTLE)
                    // If path is empty, we can castle.
                    let all = ~~~(board.All(2))
                    if ((path &&& all) = BitBoard.Default) then
                        moves <- moves ||| BitBoard.FromSq(from - 2)
                // Make sure castling close-path isn't under attack.
                if (k <> 0x0 && kingMoves.[int(from) + 1] && not (UnderAttack(board, from + 2, ioppositeColor))) then
                    // Generate path of castle king-side.
                    let path:BitBoard = if color = 0 then BitBoard(WHITE_KING_CASTLE) else BitBoard(BLACK_KING_CASTLE)
                    // If path is empty, we can castle.
                    let all = ~~~(board.All(2))
                    if ((path &&& all) = BitBoard.Default) then
                        moves <- moves ||| BitBoard.FromSq(from + 2)
                moves

[<IsByRefLike>]
type MoveList =
    struct
        val Board:Board
        val From:int
        val Hv:BitBoard
        val D:BitBoard
        val C:BitBoard
        val Moves:BitBoard
        val Count:int
        val Promotion:bool
        new(board:Board, from:int, horizontalVertical:BitBoard, diagonal:BitBoard, checks:BitBoard, moves:BitBoard, promotion:bool) =
            {
                Board = board
                From = from
                Hv = horizontalVertical
                D = diagonal
                C = checks
                Moves = moves
                Count = moves.Count
                Promotion = promotion
            }
        new(board:Board, from:int) =
            let piece, color = ColPiece.ToPcCol(board.At(from))
            let icolor = int(color)
            let ioppositeColor = icolor^^^1
            let kingSq = board.KingLoc(icolor).ToSq()
            let (horizontalVertical, diagonal) = MoveList.PinBitBoards(board, kingSq, icolor, ioppositeColor)
            let (checks, doubleChecked) = MoveList.CheckBitBoard(board, kingSq, ioppositeColor)
            new MoveList(board, from, piece, icolor, horizontalVertical, diagonal, checks, doubleChecked)
        new(board:Board, from:int, piece:int, color:int, horizontalVertical:BitBoard, diagonal:BitBoard, checks:BitBoard, doubleChecked:bool) =
            let mutable promotion = false
            // If we're double-checked (discovered check + normal check), only the king can move. Thus, we can return
            // early here.
            if doubleChecked && piece <> King then
                new MoveList(board, from, horizontalVertical, diagonal, checks, BitBoard.Default, promotion)
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
        new(board:Board, from:int, piece:int, color:int, horizontalVertical:BitBoard, diagonal:BitBoard, checks:BitBoard) =
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
        new(board:Board, from:int, horizontalVertical:BitBoard, diagonal:BitBoard, checks:BitBoard) =
            //this can only be used when processing pawn captiures
            let moves,promotion = MoveList.LegalPawnMoveCaptures(board.Map.stm,board,from,horizontalVertical,diagonal,checks)
            new MoveList(board, from, horizontalVertical, diagonal, checks, moves, promotion)
  
    end
    