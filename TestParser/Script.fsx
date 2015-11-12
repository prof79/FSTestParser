// Learn more about F# at http://fsharp.org. See the 'F# Tutorial' project
// for more guidance on F# programming.

#load "TestParser.fs"

open TestParser

let ex = explode "Hello World!"

printfn "%A" ex

printfn "%A" <| implode ex

let test_explode_implode = explode >> implode

printfn "Combined test: %A" <| test_explode_implode "Abc blabla foo bar"

"abZ04: \r-."
|> explode
|> List.map (fun c ->
        printfn "Checking: %A" c
        printfn "Is letter: %A" <| isLetter c
        printfn "Is digit:  %A" <| isDigit c
        printfn "Is space:  %A" <| isSpace c
        printfn "Is punct:  %A" <| isPunctuation c)


//System.Console.ReadKey (true)
