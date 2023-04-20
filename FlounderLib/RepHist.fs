namespace FlounderLib

module RepHist =
    let mutable Arr = Array.create 1024 0UL
    let mutable Idx = 0
    let Reset() =
        Idx <- 0
    let Append(zobristHash) = 
        Arr[Idx] <- zobristHash
        Idx <- Idx + 1
    let RemoveLast() = Idx <- Idx - 1
    let Count(zobristHash) =
        let mutable count = 0
        for i = Idx - 1 downto 0 do
            if Arr[i] = zobristHash then count <- count + 1
        count
