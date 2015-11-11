(* KUDOS to alf42red/Armin Heller https://www.youtube.com/user/alf42red *)

module TestParser

open System

(* Parser *)

type MyParserResult<'a, 'b> = ('b * 'a list) option

type MyParser<'a, 'b> = 'a list -> MyParserResult<'a, 'b>

let explode (str : string) =
    str.ToCharArray()
    |> List.ofArray

let implode (chars : char list) =
    chars
    |> List.fold (fun state c -> state + c.ToString()) ""

let is_digit = Char.IsDigit

let is_letter = Char.IsLetter

let is_space ch =
    ch = ' ' || ch = '\r' || ch = '\n' || ch = '\t' || ch = '\v'

let is_punct ch =
    not <| is_digit ch &&
    not <| is_letter ch &&
    not <| is_space ch

let digitchar2int ch =
    (Char.GetNumericValue(ch) |> int) - (Char.GetNumericValue('0') |> int)

(* Checks whether xs start with ss *)
let rec starts_with xs ss =
    match xs, ss with
    | _, []  -> true
    | [], _  -> false
    | x :: xs, s :: ss -> if x = s then starts_with xs ss else false

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

let expect ch = check ((=) ch)

let (|||) (parser1 : MyParser<'a, 'b>) (parser2 : MyParser<'a, 'b>) : MyParser<'a, 'b> =
    fun xs ->
        match parser1 xs with
        | Some res -> Some res
        | None -> parser2 xs

let (>>>) (parser : MyParser<'a, 'b>) (f : 'b -> 'c) xs : MyParserResult<'a, 'c> =
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

let empty xs : MyParserResult<'a, unit> = Some ((), xs)

let rec repeat parser xs =
    xs |> ((parser <&> (repeat parser) >>> (fun (res1, res2) -> res1 :: res2)) |||
           (empty >>> (fun _ -> [])))

let repeat1 parser xs =
    xs |> (parser <&> (repeat parser) >>> (fun (res1, res2) -> res1 :: res2))

(* Spaces are optional ("repeat" not "repeat1") *)
let pspaces = repeat pspace


(* Tokenizer/Lexer *)

type MyToken =
    | Id of string
    | Keyword of string
    | Operator of string
    | Integer of int

type MyTokenizer = MyParser<char, MyToken>

type MyLexer = string list -> string -> MyToken list

let tokops = (repeat1 ppunct) >>> (implode >> Operator)

let tokinteger =
    (repeat1 pdigit) >>> fun ds -> ds
                                   |> List.map digitchar2int
                                   |> List.fold (fun state i -> state * 10 + i) 0
                                   |> Integer

let tokidentifier (keywords : string list) =
    (pletter <&> repeat (pletter ||| pdigit ||| expect '_'))
        >>> fun (res1, res2) -> let ident = res1 :: res2
                                match keywords
                                      |> List.tryFind (fun kw -> kw = implode ident)
                                      with
                                      | Some (kw) -> Keyword kw
                                      | None -> Id <| implode ident

let lex keywords str =
    explode str
    (* ignore spaces *)
    |> (repeat (pspaces <&> (tokidentifier keywords |||
                             tokops |||
                             tokinteger)
            >>> snd))



(* Dummy *)

type TestClass() = 
    member this.Hello = "F#"
