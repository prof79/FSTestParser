(* KUDOS to alf42red/Armin Heller https://www.youtube.com/user/alf42red *)

module TestParser

open System

(* Helper Functions *)

let explode (str : string) =
    str.ToCharArray()
    |> List.ofArray

let implode (chars : char list) =
    chars
    |> List.fold (fun state ch -> state + ch.ToString()) ""

let isDigit = Char.IsDigit

let isLetter = Char.IsLetter

let isSpace ch =
    ch = ' ' || ch = '\r' || ch = '\n' || ch = '\t' || ch = '\v'

let isPunctuation ch =
    not <| isDigit ch &&
    not <| isLetter ch &&
    not <| isSpace ch

let digitCharToInt ch =
    (Char.GetNumericValue(ch) |> int) - (Char.GetNumericValue('0') |> int)

(* Checks whether xs start with ss *)
let rec startsWith xs ss =
    match xs, ss with
    | _, []  -> true
    | [], _  -> false
    | x :: xs, s :: ss -> if x = s then startsWith xs ss else false


(* Basic/Single Character Parsers *)

type MyParserResult<'a, 'b> = ('b * 'a list) option

type MyParser<'a, 'b> = 'a list -> MyParserResult<'a, 'b>

let check predicate : MyParser<'a, 'a> = function
    | x :: xs when predicate x -> Some (x, xs)
    | _ -> None

//let alpha_parser : MyParser<char, char> = function
//    | c :: cs when is_letter c -> Some (c, cs)
//    | _ -> None

let parseLetter       = check isLetter
let parseDigit        = check isDigit
let parseSpace        = check isSpace
let parsePunctuation  = check isPunctuation

let parseChar ch = check ((=) ch)

(* OR combination of two parsers - either parser1 or parser 2 matches, or none *)
let (|||) (parser1 : MyParser<'a, 'b>) (parser2 : MyParser<'a, 'b>) : MyParser<'a, 'b> =
    fun xs ->
        match parser1 xs with
        | Some res -> Some res
        | None -> parser2 xs

(* Result transformation combinator -
   changes the result (first tuple element) of a parser *)
let (>>>) (parser : MyParser<'a, 'b>) (f : 'b -> 'c) xs : MyParserResult<'a, 'c> =
    match parser xs with
    | Some (x, xs) -> Some (f x, xs)
    | None -> None

(* AND combination of two parsers; both must match.
   The result changes to a (nested) tuple of the results of all AND'ed parsers. *)
let (<&>) (parser1 : MyParser<'a, 'b>) (parser2 : MyParser<'a, 'c>) : MyParser<'a, 'b * 'c> =
    fun xs ->
        match parser1 xs with
        | Some (res1, xs) -> match parser2 xs with
                             | Some (res2, xs) -> Some ((res1, res2), xs)
                             | None -> None
        | None -> None

(* Doesn't alter anything, transforms a list of to-parse symbols
   to a parser result with unit as the result and itself as the remainder. *)
let empty xs : MyParserResult<'a, unit> = Some ((), xs)

(* Repeat a parser zero or more times. ('parser*') *)
let rec repeat parser xs =
    xs |> ((parser <&> (repeat parser) >>> (fun (res1, res2) -> res1 :: res2)) |||
           (empty >>> (fun _ -> [])))

(* Repeat a parser one or more times. ('parser+') *)
let repeat1 parser xs =
    xs |> (parser <&> (repeat parser) >>> (fun (res1, res2) -> res1 :: res2))

(* Parse spaces. In this implementation they are optional
   (thus "repeat" and not "repeat1"). *)
let parseSpaces = repeat parseSpace


(* Tokenizer/Lexer *)

type MyToken =
    | Id of string
    | Keyword of string
    | Operator of string
    | Integer of int

type MyTokenizer = MyParser<char, MyToken>

type MyLexer = string list -> string -> MyToken list

let tokOperators = (repeat1 parsePunctuation) >>> (implode >> Operator)

let tokInteger =
    (repeat1 parseDigit) >>> fun ds -> ds
                                       |> List.map digitCharToInt
                                       |> List.fold (fun state i -> state * 10 + i) 0
                                       |> Integer

let tokIdentifier (keywords : string list) =
    (parseLetter <&> repeat (parseLetter ||| parseDigit ||| parseChar '_'))
        >>> fun (res1, res2) -> let ident = res1 :: res2
                                match keywords
                                      |> List.tryFind (fun kw -> kw = implode ident)
                                      with
                                      | Some (kw) -> Keyword kw
                                      | None -> Id <| implode ident

let lex keywords str =
    (* convert string input to character list *)
    explode str
    (* ignore spaces then match elements of our mini-language *)
    |> (repeat (parseSpaces <&> (tokIdentifier keywords |||
                                 tokOperators |||
                                 tokInteger)
            >>> snd))


(* Parser/Evaluator *)




(* Dummy *)

type TestClass() = 
    member this.Hello = "F#"
