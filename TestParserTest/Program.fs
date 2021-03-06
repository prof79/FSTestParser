﻿// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

open System
open TestParser

let readKey () = Console.ReadKey(true) |> ignore

let keywords = ["("; ")"; "+"; "-"; "*"]

let rec loop () =

    let input = Console.ReadLine()

    if input = "quit"
        then ()
    else
        // eval input
        match arithmeticTest keywords input with
        | Some (terminals) -> printfn "%A" <| arithmeticEval terminals
                              printfn "(Terminals: %A)" terminals
        | None -> printfn "Couldn't parse your input."

        printfn ""

        loop ()


[<EntryPoint>]
let main argv = 
    
    printfn "%A" argv
    
    printfn ""
    printfn "Test Interpreter"
    printfn "Enter arithmetic expressions or 'quit' to quit."
    printfn ""

    loop ()

    printfn "Press a key ..."
    readKey ()

    0    // return an integer exit code
