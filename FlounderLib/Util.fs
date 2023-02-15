namespace FlounderLib
open System.Reflection
open System.Runtime.CompilerServices

module Util =
    let inline RunStaticConstructor() =
        for typ in Assembly.GetExecutingAssembly().GetTypes() do 
            RuntimeHelpers.RunClassConstructor(typ.TypeHandle)