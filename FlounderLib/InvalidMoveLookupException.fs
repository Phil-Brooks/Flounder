namespace FlounderLib
open System
open System.Runtime.Serialization

type InvalidMoveLookupException =
    inherit InvalidOperationException
    new(message) = 
        {
            inherit InvalidOperationException(message)
        } 
    new(message:string,innerException) = 
        {
            inherit InvalidOperationException(message,innerException)
        }
    new(info:SerializationInfo, context) = 
        {
            inherit InvalidOperationException(info, context)
        }
    static member FromBoard(board:Board, message:string) = 
        InvalidOperationException("\n" + board.ToString() + message)
        