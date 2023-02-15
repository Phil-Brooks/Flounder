namespace FlounderLib

type MoveTranspositionTableEntryType =
    | Exact = 0uy
    | BetaCutoff = 1uy
    | AlphaUnchanged = 2uy
    | Invalid = 3uy
