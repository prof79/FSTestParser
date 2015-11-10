// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open System

let readKey () = Console.ReadKey(true) |> ignore

[<EntryPoint>]
let main argv = 
    
    printfn "%A" argv
    
    readKey ()

    0    // return an integer exit code
