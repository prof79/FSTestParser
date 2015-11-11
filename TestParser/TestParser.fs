module TestParser

open System

type MyParser<'a, 'b> = 'a list -> ('b * 'a list) option

let explode (str : string) =
    str.ToCharArray()
    |> List.ofArray

let implode (chars : char list) =
    chars
    |> List.fold (fun state c -> state + c.ToString()) ""

let is_digit = Char.IsDigit

let is_letter = Char.IsLetter

let is_space c =
    c = ' ' || c = '\r' || c = '\n' || c = '\t' || c = '\v'

let is_punct c =
    not <| is_digit c &&
    not <| is_letter c &&
    not <| is_space c

let check pred : MyParser<'a, 'a> = function
    | x :: xs when pred x -> Some (x, xs)
    | _ -> None

//let alpha_parser : MyParser<char, char> = function
//    | c :: cs when is_letter c -> Some (c, cs)
//    | _ -> None

let pletter = check is_letter
let pdigit  = check is_digit
let pspace  = check is_space
let ppunct  = check is_punct

let expect c cs = check ((=) c) cs

let (|||) (parser1 : MyParser<'a, 'b>) (parser2 : MyParser<'a, 'b>) : MyParser<'a, 'b> =
    fun xs ->
        match parser1 xs with
        | Some res -> Some res
        | None -> parser2 xs

let (>>>) (parser : MyParser<'a, 'b>) (f : 'b -> 'c) xs =
    match parser xs with
    | Some (x, xs) -> Some (f x, xs)
    | None -> None

let (<&>) (parser1 : MyParser<'a, 'b>) (parser2 : MyParser<'a, 'c>) : MyParser<'a, 'b * 'c> =
    fun xs ->
        match parser1 xs with
        | Some (res1, xs) -> match parser2 xs with
                             | Some (res2, xs) -> Some ((res1, res2), xs)
                             | None -> None
        | None -> None

let empty xs = Some ((), xs)

let rec repeat parser xs =
    xs |> ((parser <&> (repeat parser) >>> (fun (res1, res2) -> res1 :: res2)) |||
           (empty >>> (fun _ -> [])))

let repeat1 parser xs =
    xs |> (parser <&> (repeat parser) >>> (fun (res1, res2) -> res1 :: res2))

type MyToken =
    | Id of string
    | Keyword of string
    | Operator of string
    | Integer of int

let pspaces cs = repeat pspace cs

let tokops cs = cs |> ((repeat1 ppunct) >>> fun cs -> Operator (implode cs))

let tokinteger cs =
    match cs |> (repeat1 pdigit) with
    | Some (cs, rest) ->
            let intval = cs
                         |> List.map (fun c ->
                                (Char.GetNumericValue(c) |> int) -
                                (Char.GetNumericValue('0') |> int))
                         |> List.fold (fun state i -> state * 10 + i) 0
            Some (Integer intval, rest)

    | None -> None


type TestClass() = 
    member this.Hello = "F#"
