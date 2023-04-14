namespace FlounderLib
open System
open System.Runtime.CompilerServices

module OrderedMoveList =
    let SIZE = 128
    let MvvLvaTable =
        [|
            [| 2005; 2004; 2003; 2002; 2001; 2000 |];
            [| 3005; 3004; 3003; 3002; 3001; 3000 |];
            [| 4005; 4004; 4003; 4002; 4001; 4000 |];
            [| 5005; 5004; 5003; 5002; 5001; 5000 |];
            [| 6005; 6004; 6003; 6002; 6001; 6000 |];
            [| 7005; 7004; 7003; 7002; 7001; 7000 |]
        |]
    let MvvLva(attacker:int, victim:int) = MvvLvaTable.[victim].[attacker]

[<IsByRefLike>]
type OrderedMoveList =
    struct
        val mutable PRIORITY:int
        val mutable Internal:Span<OrderedMoveEntry>
        val mutable KillerMoveOne:OrderedMoveEntry
        val mutable KillerMoveTwo:OrderedMoveEntry
        val mutable HistTbl:HistoryTable
        // Technically; there do exist positions where we'd have 218 legal moves.
        // However; they are so unlikely that 128 seems like an okay number.
        new(memory:Span<OrderedMoveEntry>, ply:int, killerMoveTable:KillerMoveTable, historyTable:HistoryTable) =
            {
                PRIORITY = Int32.MaxValue
                Internal = memory
                KillerMoveOne =  killerMoveTable.[0, ply]
                KillerMoveTwo =  killerMoveTable.[1, ply]
                HistTbl =  historyTable
            }
        member this.ScoreMove(pieceToMove:int, board:BoardRec, move:OrderedMoveEntry, tableMove:OrderedMoveEntry) =
            // Compare our move with the one found from transposition table. There's no guarantee the transposition move
            // is even legal, so this acts as a sort of legal verification for it too.
            // Regardless, if our move is equal to that (also proving that it is legal for this position), then give it
            // highest priority, making it the first move we make.
            if (move.From = tableMove.From && move.To = tableMove.To && move.Promotion = tableMove.Promotion) then this.PRIORITY - 1
            // Score promotions based on the type of promotion it is. 
            // Promotion | Score
            // Queen     | PRIORITY - 4 (HIGHEST)
            // Bishop    | PRIORITY - 5
            // Knight    | PRIORITY - 6
            // Rook      | PRIORITY - 7 (LOWEST)
            elif move.Promotion <> PromNone then this.PRIORITY - 8 + move.Promotion
            // Score captures based on the piece capturing and the piece being captured.
            // The idea behind it is to give highest priority to captures that are capturing most valuable pieces
            // with least valuable pieces.
            else
                let pto = board.Squares[move.To]/2
                if (pto <> EmptyPc) then 
                    let pfrom = board.Squares[move.From]/2
                    OrderedMoveList.MvvLva(pfrom, pto) * 10000
                // If the move is a quiet move (not capture / promotion), then we should check if it is a killer move or history
                // move.
                // Killer moves are moves that are very likely to cause a beta cutoff.
                // History moves are moves that have been scored as most likely beta cutoff moves, depending on how many
                // times the move has caused a beta cutoff (gave us a guaranteed best move).
                // Check if move is a rank 1 killer move (extremely local, recently updated).
                elif move.From = this.KillerMoveOne.From && move.To = this.KillerMoveOne.To && move.Promotion = this.KillerMoveOne.Promotion then 900000
                // Check if move is a rank 2 killer move (less local, might've been updated long time ago).
                elif move.From = this.KillerMoveTwo.From && move.To = this.KillerMoveTwo.To && move.Promotion = this.KillerMoveTwo.Promotion then 800000
                // Return the updated history score for the move.
                else this.HistTbl.[pieceToMove, (if board.IsWtm then 0 else 1), move.To]
        member this.NormalMoveGeneration(board:byref<BoardRec>, transpositionMove:OrderedMoveEntry) =
            // Generate pins and check bitboards.
            let stm = if board.IsWtm then 0 else 1 
            let xstm = if board.IsWtm then 1 else 0
            let kingSq = if stm = White then board.WhiteKingLoc else board.BlackKingLoc
            let (hv, d) = MoveList.PinBitBoards(board, kingSq, stm, xstm)
            let checks, doubleChecked = MoveList.CheckBitBoard(board, kingSq, xstm)
            // Define the list.
            let mutable i = 0
            //BitBoardIterator fromIterator;
            //Square from;
            if not doubleChecked then
                // We can only do this if we're not double checked.
                // In case of double-checked (discovered + normal), only the king can move so we should skip this.
                // Generate all pawn moves.
                let fromarr = Bits.ToArray(board.Pieces[stm])
                for from in fromarr do
                    let moveList = MoveList(&board, from, Pawn, stm, hv, d, checks)
                    let movearr = Bits.ToArray(moveList.Moves)
                    for move in movearr do
                        if (moveList.Promotion) then
                            for p = PromKnight to PromQueen do
                                this.Internal.[i] <- new OrderedMoveEntry(from, move, p)
                                this.Internal.[i].Score <- this.ScoreMove(Pawn, board, this.Internal.[i], transpositionMove)
                                i<-i+1
                        else
                            this.Internal.[i] <- new OrderedMoveEntry(from, move, PromNone)
                            this.Internal.[i].Score <- this.ScoreMove(Pawn, board, this.Internal.[i], transpositionMove)
                            i<-i+1
                // Generate moves for rook, knight, bishop, and queen.
                for piece in [Rook;Knight;Bishop;Queen] do
                    let fromarr = Bits.ToArray(board.Pieces[piece*2 + stm])
                    for from in fromarr do
                        let moveList = MoveList(&board, from, piece, stm, hv, d, checks)
                        let movearr = Bits.ToArray(moveList.Moves)
                        for move in movearr do
                            this.Internal.[i] <- new OrderedMoveEntry(from, move, PromNone)
                            this.Internal.[i].Score <- this.ScoreMove(piece, board, this.Internal.[i], transpositionMove)
                            i<-i+1
            // Generate all king moves.
            let fromarr = Bits.ToArray(board.Pieces[King*2 + stm])
            for from in fromarr do
                let moveList = MoveList(&board, from, King, stm, hv, d, checks)
                let movearr = Bits.ToArray(moveList.Moves)
                for move in movearr do
                    this.Internal.[i] <- new OrderedMoveEntry(from, move, PromNone)
                    this.Internal.[i].Score <- this.ScoreMove(King, board, this.Internal.[i], transpositionMove)
                    i<-i+1
            i
        member this.QSearchMoveGeneration(board:byref<BoardRec>, transpositionMove:OrderedMoveEntry) =
            // If we only want capture moves, we should also define our opposite board.
            let stm = if board.IsWtm then 0 else 1 
            let xstm = if board.IsWtm then 1 else 0 
            let opposite = if board.IsWtm then board.Black else board.White
            // Generate pins and check bitboards.
            let kingSq = if stm = White then board.WhiteKingLoc else board.BlackKingLoc
            let (hv, d) = MoveList.PinBitBoards(board, kingSq, stm, xstm)
            let (checks, doubleChecked) = MoveList.CheckBitBoard(board, kingSq, xstm)
            // Define the list.
            let mutable i = 0
            if not doubleChecked then
                // We can only do this if we're not double checked.
                // In case of double-checked (discovered + normal), only the king can move so we should skip this.
                // Generate all pawn moves.
                let fromarr = Bits.ToArray(board.Pieces[stm])
                for from in fromarr do
                    let moveList = MoveList(&board, from, hv, d, checks)
                    let movearr = Bits.ToArray(moveList.Moves)
                    for move in movearr do
                        if moveList.Promotion then
                            for p = PromKnight to PromQueen do
                                this.Internal.[i] <- new OrderedMoveEntry(from, move, p)
                                this.Internal.[i].Score <- this.ScoreMove(Pawn, board, this.Internal.[i], transpositionMove)
                                i<-i+1
                        else 
                            this.Internal.[i] <- new OrderedMoveEntry(from, move, PromNone)
                            this.Internal.[i].Score <- this.ScoreMove(Pawn, board, this.Internal.[i], transpositionMove)
                            i<-i+1
                // Generate moves for rook, knight, bishop, and queen.
                for piece in [Rook;Knight;Bishop;Queen] do
                    let fromarr = Bits.ToArray(board.Pieces[piece*2 + stm])
                    for from in fromarr do
                        let moveList = MoveList(&board, from, piece, stm, hv, d, checks)
                        let movearr = Bits.ToArray((moveList.Moves &&& opposite))
                        for move in movearr do
                            this.Internal.[i] <- new OrderedMoveEntry(from, move, PromNone)
                            this.Internal.[i].Score <- this.ScoreMove(piece, board, this.Internal.[i], transpositionMove)
                            i<-i+1
            // Generate all king moves.
            let fromarr = Bits.ToArray(board.Pieces[King*2 + stm])
            for from in fromarr do
                let moveList = MoveList(&board, from, King, stm, hv, d, checks)
                let movearr = Bits.ToArray((moveList.Moves &&& opposite))
                for move in movearr do
                    this.Internal.[i] <- new OrderedMoveEntry(from, move, PromNone)
                    this.Internal.[i].Score <- this.ScoreMove(King, board, this.Internal.[i], transpositionMove)
                    i<-i+1                    
            i
        member this.Item with get(i:int):byref<OrderedMoveEntry> = 
            &(this.Internal.[i])
        member this.SortNext(sorted:int, maxSelection:int) =
            let mutable index = sorted
            for i = 1 + sorted to maxSelection - 1 do
                if (this.Internal.[i].Score > this.Internal.[index].Score) then index <- i
            this.Swap(index, sorted)
        member this.Swap(firstIndex:int, secondIndex:int) = 
            let nf = this.Internal.[secondIndex]
            let ns = this.Internal.[firstIndex]
            this.Internal.[firstIndex] <- nf
            this.Internal.[secondIndex] <- ns
    end