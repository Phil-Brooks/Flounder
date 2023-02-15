namespace FlounderLib
open System.Runtime.CompilerServices
open System.Runtime.InteropServices

[<AutoOpen>]
module Array =
    type ``[]``<'a> with
        member this.AA(index:int) = 
#if DEBUG
            &this.[index]
#else
            &(Unsafe.Add(&(MemoryMarshal.GetArrayDataReference(this)), index))
#endif

    let DJAA (firstIndex:int, secondIndex:int) (array:'T array array) =
#if DEBUG
        &array.AA(firstIndex).[secondIndex]
#else
        &(Unsafe.Add(&(MemoryMarshal.GetArrayDataReference(array.AA(firstIndex))), secondIndex))
#endif