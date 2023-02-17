namespace FlounderLib
open System
open System.Runtime.CompilerServices

[<IsByRefLike; Struct>]
type MoveList(board:Board1, from:Square, horizontalVertical:BitBoard, diagonal:BitBoard, checks:BitBoard, moves:BitBoard, promotion:bool) =
    static member WHITE_KING_CASTLE = 0x60uL
    static member BLACK_KING_CASTLE = MoveList.WHITE_KING_CASTLE <<< 56
    static member WHITE_QUEEN_CASTLE = 0xEuL
    static member BLACK_QUEEN_CASTLE = MoveList.WHITE_QUEEN_CASTLE <<< 56

    member _.Board = board
    member _.From = from
    member _.Hv = horizontalVertical
    member _.D = diagonal
    member _.C = checks
    member this.Count:int = this.Moves.Count
    member _.Moves:BitBoard = moves
    member _.Promotion = promotion

    static member UnderAttack(board:Board1, sq:Square, by:PieceColor) =
        let s = int(sq)
        // First, we check if the square is being attacked by pawns.
        // To do this, we generate a reverse attack mask, letting our square act as a pawn and seeing if opposing
        // pawns exist on the squares in the mask. If so, our square can be attacked by pawns.
        let pawnAttack = if by = PieceColor.White then AttackTable1.BlackPawnAttacks.[s] else AttackTable1.WhitePawnAttacks.[s]
        if (pawnAttack &&& board.All(Piece.Pawn, by)).ToBool() then true
        // Then, we check if the square is being attacked by knights.
        // To do this, we generate a reverse attack mask, letting our square act as a knight and seeing if opposing
        // knights exist on the squares in the mask. If so, our square can be attacked by knights.
        elif (AttackTable1.KnightMoves.[s] &&& board.All(Piece.Knight, by)).ToBool() then true
        else
            // Next, we check if the square is being attacked by sliding pieces.
            // To do this, first we need to find all occupied squares (by our and opposing pieces).
            let occupied = ~~~(board.All(PieceColor.None))
            // We should check queen along with rook/bishop as queen moves are (rook moves | bishop moves).
            let queen = board.All(Piece.Queen, by)
            // Generate a reverse attack mask for rook, letting our square act as a rook and seeing if opposing rook or
            // queen exist on the squares in the mask. If so, our square can be attacked by either rook or queen.
            let mutable mIndex = BlackMagicBitBoardFactory.GetMagicIndex(Piece.Rook, occupied, sq)
            if (AttackTable.SlidingMoves.[mIndex] &&& (queen ||| board.All(Piece.Rook, by))).ToBool() then true
            else
                // Generate a reverse attack mask for bishop, letting our square act as a rook and seeing if opposing
                // bishop or queen exist on the squares in the mask. If so, our square can be attacked by either bishop
                // or queen.
                mIndex <- BlackMagicBitBoardFactory.GetMagicIndex(Piece.Bishop, occupied, sq)
                if (AttackTable.SlidingMoves.[mIndex] &&& (queen ||| board.All(Piece.Bishop, by))).ToBool() then true
                else
                    // Lastly, we check if our square is being attacked by a king.
                    // We generate a reverse attack mask, letting our square act as king and then check if there if opposing
                    // king exists on the squares in the mask. If so, our square can be attacked by king.
                    // Otherwise, this square is completely safe from all pieces.
                     (AttackTable1.KingMoves.[s] &&& board.All(Piece.King, by)).ToBool()
    static member CheckBitBoard(board:Board1, sq:Square, by:PieceColor) =
        let mutable count = 0
        let mutable checks = BitBoard.Default
        let s = int(sq)
        // First we generate a pawn check.
        let pawnAttack = if by = PieceColor.White then AttackTable1.BlackPawnAttacks.[s] else AttackTable1.WhitePawnAttacks.[s]
        let pawnCheck = pawnAttack &&& board.All(Piece.Pawn, by)
        // Next, we generate a knight check.
        let knightCheck = AttackTable1.KnightMoves.[s] &&& board.All(Piece.Knight, by)
        // For sliding pieces, we use a BitBoard of all pieces.
        let occupied = ~~~(board.All(PieceColor.None))
        // We will reference the queen along with rooks and bishops for the checks.
        let queen = board.All(Piece.Queen, by)
        // Now, we generate a rook or queen (straight only) check.
        let mutable mIndex = BlackMagicBitBoardFactory.GetMagicIndex(Piece.Rook, occupied, sq)
        let rookQueenCheck = AttackTable.SlidingMoves.[mIndex] &&& (queen ||| board.All(Piece.Rook, by))
        // Next, we generate a bishop or queen (diagonal only) check.
        mIndex <- BlackMagicBitBoardFactory.GetMagicIndex(Piece.Bishop, occupied, sq)
        let bishopQueenCheck = AttackTable.SlidingMoves.[mIndex] &&& (queen ||| board.All(Piece.Bishop, by))
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
            let rqSq:Square = rookQueenCheck.ToSq()
            checks <- checks ||| UtilityTable1.Between.[s].[int(rqSq)] ||| BitBoard.FromSq(rqSq)
            count<-count+1
            // In the case where pawn promotes to queen or rook, we have a secondary check as well.
            if (rookQueenCheck.Count > 1) then count<-count+1
        if bishopQueenCheck.ToBool() then
            // If there is a bishop-queen check hit, we must add in the checks as well as the path from square to
            // bishop or queen.
            let bqSq:Square = bishopQueenCheck.ToSq()
            checks <- checks ||| UtilityTable1.Between.[s].[int(bqSq)] ||| BitBoard.FromSq(bqSq)
            count<-count+1
        
        if (checks = BitBoard.Default) then checks <- BitBoard.Filled

        (checks, count > 1)
    static member PinBitBoards(board:Board1, sq:Square, us:PieceColor, by:PieceColor) =
        let s = int(sq)
        // Unlike for all other boards and checks, we don't use a fully occupied board. We want our paths to go through
        // our pieces so we only consider board occupied by opposing.
        let byBoard = board.All(by)
        // We will reference the queen along with rooks and bishops for the checks.
        let queen = board.All(Piece.Queen, by)
        // First, we generate all rook / queen (straight only) attacks.
        let mutable mIndex = BlackMagicBitBoardFactory.GetMagicIndex(Piece.Rook, byBoard, sq)
        let rookQueenCheck = AttackTable.SlidingMoves.[mIndex] &&& (queen ||| board.All(Piece.Rook, by))
        // Next, we generate all bishop / queen (diagonal only) attacks.
        mIndex <- BlackMagicBitBoardFactory.GetMagicIndex(Piece.Bishop, byBoard, sq)
        let bishopQueenCheck = AttackTable.SlidingMoves.[mIndex] &&& (queen ||| board.All(Piece.Bishop, by))
        let mutable horizontalVerticalPin = BitBoard.Default
        let mutable diagonalPin = BitBoard.Default
        // Iterate over the rooks and queens (pinning straight).
        let mutable rookQueenIterator = rookQueenCheck.GetEnumerator()
        let mutable rqSq = rookQueenIterator.Current
        while (rookQueenIterator.MoveNext()) do
            let rqS = int(rqSq)
            let possiblePin = UtilityTable1.Between.[s].[rqS] ||| BitBoard.FromSq(rqSq)
            if ((possiblePin &&& board.All(us)).Count = 1) then horizontalVerticalPin <- horizontalVerticalPin ||| possiblePin
            // Next square iteration.
            rqSq <- rookQueenIterator.Current
        // Iterate over the bishops and queens (pinning diagonally).
        let mutable bishopQueenIterator = bishopQueenCheck.GetEnumerator()
        let mutable bqSq = bishopQueenIterator.Current
        while (bishopQueenIterator.MoveNext()) do
            let bqS = int(bqSq)
            let possiblePin = UtilityTable1.Between.[s].[bqS] ||| BitBoard.FromSq(bqSq)
            if ((possiblePin &&& board.All(us)).Count = 1) then diagonalPin <- diagonalPin ||| possiblePin
            // Next square iteration.
            bqSq <- bishopQueenIterator.Current
        (horizontalVerticalPin, diagonalPin)
    static member WithoutProvidedPins(board:Board1, from:Square) =
        let piece, color = board.At(from)
        let oppositeColor = PieceColor.OppositeColor(color)
        let kingSq = board.KingLoc(color).ToSq()
        let (horizontalVertical, diagonal) = MoveList.PinBitBoards(board, kingSq, color, oppositeColor)
        let (checks, doubleChecked) = MoveList.CheckBitBoard(board, kingSq, oppositeColor)
        new MoveList(board, from, piece, color, horizontalVertical, diagonal, checks, doubleChecked)
    static member LegalPawnMoveCaptures(color:PieceColor, board:Board1, from:Square, hv:BitBoard, d:BitBoard, c:BitBoard) =
        let mutable moves = BitBoard.Default
        if (hv.[from]) then
            // If pawn is horizontally pinned, then we have no moves.
            moves,false
        else 
            let oppositeColor = PieceColor.OppositeColor(color)
            let opposite = board.All(oppositeColor)
            let mutable epPieceSq = Square.Na
            // Promotion Flag
            // If we're at rank 7 for white or rank 1 for black, we should set the promotion flag to true.
            // It is important to set it earlier rather than later, because if there is a diagonal pin capture
            // leading to a promotion, we must make sure to record that as 4 moves.
            // FEN: 2q5/1Pp5/K2p4/7r/6Pk/8/8/1R6 w - -
            let promotion = color = PieceColor.White && from > Square.H6 &&  from < Square.A8 || 
                            color = PieceColor.Black && from > Square.H1 && from < Square.A3
            // Attack moves
            // En Passant.
            if (board.EnPassantTarget <> Square.Na) then
                // If EP exists, then we need to check if a piece exists on square that's under attack from Ep, not
                // where we move to.
                epPieceSq <- if color = PieceColor.White then Square.FromInt(int(board.EnPassantTarget) - 8) else Square.FromInt(int(board.EnPassantTarget) + 8)
                let epTargetPieceExists = board.All(Piece.Pawn, oppositeColor).[epPieceSq]
                // We need to check if a piece of ours exists to actually execute the EP.
                // We do this by running a reverse pawn mask, to determine whether a piece of ours is on the corner.
                let reverseCorner = if color = PieceColor.White then AttackTable1.BlackPawnAttacks.[int(board.EnPassantTarget)] else AttackTable1.WhitePawnAttacks[int(board.EnPassantTarget)]
                if (epTargetPieceExists && reverseCorner.[from]) then
                    // If both the enemy EP piece and our piece that can theoretically EP exist...
                    moves <- moves ||| BitBoard.FromSq(board.EnPassantTarget)
            // Attack Moves.
            let attack = if color = PieceColor.White then AttackTable1.WhitePawnAttacks.[int(from)] else AttackTable1.BlackPawnAttacks.[int(from)]
            // Make sure attacks are only on opposite pieces (and not on empty squares or squares occupied by
            // our pieces).
            moves <- moves ||| (attack &&& opposite &&& c)
            if (d.[from]) then
                // If pawn is pinned diagonally, we can only do attacks and EP on the pin.
                moves <- moves &&& d
                moves,promotion
            else
                //Special EP case
                if (epPieceSq <> Square.Na) then
                    // If the pawn isn't pinned diagonally or horizontally/vertically, we must do one final check for EP:
                    // In the rare EP-pin position: 8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - -
                    // If we do EP here, our king can be attacked by rook.
                    // This is known as being pinned through a piece and only happens for EP, thus we must actually EP and see
                    // if our king is under attacked.
                    board.RemovePiece(Piece.Pawn, color, from)
                    board.RemovePiece(Piece.Pawn, oppositeColor, epPieceSq)
                    board.InsertPiece(Piece.Pawn, color, board.EnPassantTarget)
                    let kingSq = board.KingLoc(color).ToSq()
                    // If our king is under attack, it means the pawn was pinned through a piece and the removal of that piece
                    // caused a discovered pin. Thus, we must remove it from our legal moves.
                    if (MoveList.UnderAttack(board, kingSq, oppositeColor)) then moves <- moves &&& ~~~(BitBoard(1UL) <<< int(board.EnPassantTarget))
                    board.InsertPiece( Piece.Pawn, color, from)
                    board.InsertPiece(Piece.Pawn, oppositeColor, epPieceSq)
                    board.RemovePiece(Piece.Pawn, color, board.EnPassantTarget)
                    // In the case that the EP piece isn't in our checks during a check, we shouldn't EP.
                    if (moves.[board.EnPassantTarget] && not c.[epPieceSq]) then moves <- moves &&& ~~~(BitBoard(1UL) <<< int(board.EnPassantTarget))
                    moves,promotion
                else
                    moves,promotion                
    static member LegalPawnMoves(color:PieceColor, board:Board1, from:Square, hv:BitBoard, d:BitBoard, c:BitBoard) =
        let mutable moves = BitBoard.Default
        let oppositeColor = PieceColor.OppositeColor(color)
        let opposite = board.All(oppositeColor)
        let mutable epPieceSq = Square.Na
        //Promotion Flag
        // If we're at rank 7 for white or rank 1 for black, we should set the promotion flag to true.
        // It is important to set it earlier rather than later, because if there is a diagonal pin capture
        // leading to a promotion, we must make sure to record that as 4 moves.
        // FEN: 2q5/1Pp5/K2p4/7r/6Pk/8/8/1R6 w - -
        let promotion = color = PieceColor.White && from > Square.H6 &&  from < Square.A8 || 
                        color = PieceColor.Black && from > Square.H1 && from < Square.A3
        // Attack moves
        // En Passant.
        if (board.EnPassantTarget <> Square.Na) then
            // If EP exists, then we need to check if a piece exists on square that's under attack from Ep, not
            // where we move to.
            epPieceSq <- if color = PieceColor.White then Square.FromInt(int(board.EnPassantTarget) - 8) else Square.FromInt(int(board.EnPassantTarget) + 8)
            let epTargetPieceExists = board.All(Piece.Pawn, oppositeColor).[epPieceSq]
            // We need to check if a piece of ours exists to actually execute the EP.
            // We do this by running a reverse pawn mask, to determine whether a piece of ours is on the corner.
            let reverseCorner = if color = PieceColor.White then AttackTable1.BlackPawnAttacks.[int(board.EnPassantTarget)] else AttackTable1.WhitePawnAttacks[int(board.EnPassantTarget)]
            if (epTargetPieceExists && reverseCorner.[from]) then
                // If both the enemy EP piece and our piece that can theoretically EP exist...
                moves <- moves ||| BitBoard.FromSq(board.EnPassantTarget)
        // Attack Moves.
        let attack = if color = PieceColor.White then AttackTable1.WhitePawnAttacks.[int(from)] else AttackTable1.BlackPawnAttacks.[int(from)]
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
            pushes <- pushes ||| (if color = PieceColor.White then BitBoard.FromSq(from) <<< 8 else BitBoard.FromSq(from) >>> 8) &&& board.All(PieceColor.None)
            if ((from > Square.H1 && from < Square.A3 || from > Square.H6 && from < Square.A8) && pushes.ToBool()) then
                // If we are on the starting pawn position & the first pawn push was successful.
                // Push once more.
                pushes <- pushes ||| if color = PieceColor.White then BitBoard.FromSq(from) <<< 16 else BitBoard.FromSq(from) >>> 16
            // Make sure our pushes are not stepping on to enemy pieces.
            // These are normal moves, not attack moves so we can't capture.
            pushes <- pushes &&& ~~~(opposite) &&& ~~~(board.All(color))
            moves <- moves ||| (pushes &&& c)
            if (hv.[from]) then
                // If pawn is horizontally pinned, then we have no moves.
                // However, if pawn is vertically pinned, then we can at least do pushes.
                moves <- moves &&& hv
                moves,promotion
            else
                //Special EP case
                if (epPieceSq <> Square.Na) then
                    // If the pawn isn't pinned diagonally or horizontally/vertically, we must do one final check for EP:
                    // In the rare EP-pin position: 8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - -
                    // If we do EP here, our king can be attacked by rook.
                    // This is known as being pinned through a piece and only happens for EP, thus we must actually EP and see
                    // if our king is under attacked.
                    board.RemovePiece(Piece.Pawn, color, from)
                    board.RemovePiece(Piece.Pawn, oppositeColor, epPieceSq)
                    board.InsertPiece(Piece.Pawn, color, board.EnPassantTarget)
                    let kingSq = board.KingLoc(color).ToSq()
                    // If our king is under attack, it means the pawn was pinned through a piece and the removal of that piece
                    // caused a discovered pin. Thus, we must remove it from our legal moves.
                    if (MoveList.UnderAttack(board, kingSq, oppositeColor)) then moves <- moves &&& ~~~(BitBoard(1UL) <<< int(board.EnPassantTarget))
                    board.InsertPiece(Piece.Pawn, color, from)
                    board.InsertPiece(Piece.Pawn, oppositeColor, epPieceSq)
                    board.RemovePiece(Piece.Pawn, color, board.EnPassantTarget)
                    // In the case that the EP piece isn't in our checks during a check, we shouldn't EP.
                    if (moves.[board.EnPassantTarget] && not c.[epPieceSq]) then moves <- moves &&& ~~~(BitBoard(1UL) <<< int(board.EnPassantTarget))
                    moves,promotion
                else
                    moves,promotion
    static member LegalRookMoves(color:PieceColor, board:Board1, from:Square, hv:BitBoard, d:BitBoard, c:BitBoard) =
        let mutable moves = BitBoard.Default
        // If rook is diagonally pinned, it has no moves.
        if (d.[from]) then moves
        else
            // Calculate pseudo-legal moves within check board.
            let mIndex = BlackMagicBitBoardFactory.GetMagicIndex(Piece.Rook, ~~~(board.All(PieceColor.None)), from)
            moves <- moves ||| AttackTable.SlidingMoves.[mIndex] &&& ~~~(board.All(color)) &&& c
            // If rook is horizontally or vertically pinned, it can only move within the pin.
            if (hv.[from]) then moves <- moves &&& hv
            moves
    static member LegalKnightMoves(color:PieceColor, board:Board1, from:Square, hv:BitBoard, d:BitBoard, c:BitBoard) =
        let mutable moves = BitBoard.Default
        if (hv.[from] || d.[from]) then moves
        else
            moves <- moves ||| AttackTable1.KnightMoves.[int(from)] &&& ~~~(board.All(color)) &&& c
            moves
    static member LegalBishopMoves(color:PieceColor, board:Board1, from:Square, hv:BitBoard, d:BitBoard, c:BitBoard) =
        let mutable moves = BitBoard.Default
        // If bishop is horizontally or vertically pinned, it has no moves.
        if (hv.[from]) then moves
        else
            // Calculate pseudo-legal moves within check board.
            let mIndex = BlackMagicBitBoardFactory.GetMagicIndex(Piece.Bishop, ~~~(board.All(PieceColor.None)), from)
            moves <- moves ||| AttackTable.SlidingMoves.[mIndex] &&& ~~~(board.All(color)) &&& c
            // If bishop is diagonally pinned, it can only move within the pin.
            if (d.[from]) then moves <- moves &&& d
            moves
    static member LegalQueenMoves(color:PieceColor, board:Board1, from:Square, hv:BitBoard, d:BitBoard, c:BitBoard) =
        MoveList.LegalRookMoves(color,board,from,hv,d,c) 
        ||| MoveList.LegalBishopMoves(color,board,from,hv,d,c)
    static member LegalKingMoves(color:PieceColor, board:Board1, from:Square, hv:BitBoard, d:BitBoard, c:BitBoard) =
        let mutable moves = BitBoard.Default
        // Normal
        let mutable kingMoves = AttackTable1.KingMoves.[int(from)]
        kingMoves <- kingMoves &&& ~~~(board.All(color))
        // If we have no king moves, we can return earlier and avoiding having to check if the moves are legal
        // or not by removing the king.
        if not (kingMoves.ToBool()) then 
            moves
        else
            let oppositeColor = PieceColor.OppositeColor(color)
            let mutable kingMovesIterator = kingMoves.GetEnumerator()
            let mutable move = kingMovesIterator.Current
            board.RemovePiece(Piece.King, color, from)
            while (kingMovesIterator.MoveNext()) do
                if (MoveList.UnderAttack(board, move, oppositeColor)) then kingMoves.[move] <- false
                // Next square iteration.
                move <- kingMovesIterator.Current
            board.InsertPiece(Piece.King, color, from)
            moves <- moves ||| kingMoves
            // Castling
            // If enemy is attacking our king, we cannot castle.
            if (MoveList.UnderAttack(board, from, oppositeColor)) then moves
            else
                // Get castling rights.
                let q, k = board.CastlingRight(color)
                // Make sure castling close-path isn't under attack.
                if (q <> 0x0uy && kingMoves.[int(from) - 1] && not (MoveList.UnderAttack(board, Square.FromInt(int(from) - 2), oppositeColor))) then
                    // Generate path of castle queen-side.
                    let path:BitBoard = if color = PieceColor.White then BitBoard(MoveList.WHITE_QUEEN_CASTLE) else BitBoard(MoveList.BLACK_QUEEN_CASTLE)
                    // If path is empty, we can castle.
                    let all = ~~~(board.All(PieceColor.None))
                    if ((path &&& all) = BitBoard.Default) then
                        moves <- moves ||| BitBoard.FromSq(Square.FromInt(int(from) - 2))
                // Make sure castling close-path isn't under attack.
                if (k <> 0x0uy && kingMoves.[int(from) + 1] && not (MoveList.UnderAttack(board, Square.FromInt(int(from) + 2), oppositeColor))) then
                    // Generate path of castle king-side.
                    let path:BitBoard = if color = PieceColor.White then BitBoard(MoveList.WHITE_KING_CASTLE) else BitBoard(MoveList.BLACK_KING_CASTLE)
                    // If path is empty, we can castle.
                    let all = ~~~(board.All(PieceColor.None))
                    if ((path &&& all) = BitBoard.Default) then
                        moves <- moves ||| BitBoard.FromSq(Square.FromInt(int(from) + 2))
                moves
    new(board:Board1, from:Square, piece:Piece, color:PieceColor, horizontalVertical:BitBoard, diagonal:BitBoard, checks:BitBoard, doubleChecked:bool) =
        let mutable promotion = false
        // If we're double-checked (discovered check + normal check), only the king can move. Thus, we can return
        // early here.
        if (doubleChecked && piece <> Piece.King) then
            new MoveList(board, from, horizontalVertical, diagonal, checks, BitBoard.Default, promotion)
        else
            let moves =
                if piece=Piece.Pawn then 
                    let m,p = MoveList.LegalPawnMoves(color,board,from,horizontalVertical,diagonal,checks)
                    promotion <- p
                    m
                elif piece=Piece.Rook then 
                    MoveList.LegalRookMoves(color,board,from,horizontalVertical,diagonal,checks)
                elif piece=Piece.Knight then 
                    MoveList.LegalKnightMoves(color,board,from,horizontalVertical,diagonal,checks)
                elif piece=Piece.Bishop then 
                    MoveList.LegalBishopMoves(color,board,from,horizontalVertical,diagonal,checks)
                elif piece=Piece.Queen then 
                    MoveList.LegalQueenMoves(color,board,from,horizontalVertical,diagonal,checks)
                elif piece=Piece.King then 
                    MoveList.LegalKingMoves(color,board,from,horizontalVertical,diagonal,checks)
                else 
                    raise (InvalidMoveLookupException.FromBoard(board,"Cannot generate move for empty piece: " + from.ToString()))
            new MoveList(board, from, horizontalVertical, diagonal, checks, moves, promotion)
    new(board:Board1, from:Square, piece:Piece, color:PieceColor, horizontalVertical:BitBoard, diagonal:BitBoard, checks:BitBoard) =
        let mutable promotion = false
        let moves =
            if piece=Piece.Pawn then 
                let m,p = MoveList.LegalPawnMoves(color,board,from,horizontalVertical,diagonal,checks)
                promotion <- p
                m
            elif piece=Piece.Rook then 
                MoveList.LegalRookMoves(color,board,from,horizontalVertical,diagonal,checks)
            elif piece=Piece.Knight then 
                MoveList.LegalKnightMoves(color,board,from,horizontalVertical,diagonal,checks)
            elif piece=Piece.Bishop then 
                MoveList.LegalBishopMoves(color,board,from,horizontalVertical,diagonal,checks)
            elif piece=Piece.Queen then 
                MoveList.LegalQueenMoves(color,board,from,horizontalVertical,diagonal,checks)
            elif piece=Piece.King then 
                MoveList.LegalKingMoves(color,board,from,horizontalVertical,diagonal,checks)
            else 
                raise (InvalidMoveLookupException.FromBoard(board,"Cannot generate move for empty piece: " + from.ToString()))
        new MoveList(board, from, horizontalVertical, diagonal, checks, moves, promotion)
    new(board:Board1, from:Square, horizontalVertical:BitBoard, diagonal:BitBoard, checks:BitBoard) =
        //this can only be used when processing pawn captiures
        let moves,promotion = MoveList.LegalPawnMoveCaptures(board.ColorToMove,board,from,horizontalVertical,diagonal,checks)
        new MoveList(board, from, horizontalVertical, diagonal, checks, moves, promotion)
  