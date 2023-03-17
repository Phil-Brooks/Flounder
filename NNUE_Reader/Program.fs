open FlounderLib
open System.IO

[<EntryPoint>]
let main argv =

    let board = Board.Default()
    let kingSq = int(board.KingLoc(board.ColorToMove).ToUint64())
    let occupied = ~~~(board.All(PieceColor.None))
    let mutable fromIterator = occupied.GetEnumerator()
    let mutable from = fromIterator.Current
    let mutable i = 0
    while (fromIterator.MoveNext()) do
        let pc,col = board.At(from)
        i <- i + 1
        from <- fromIterator.Current


    //TEST
    

    0
