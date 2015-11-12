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

let expect x = check ((=) x)

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

(* Repeat a parser zero or more times. ('parser*')
   Result (fst) is a list, eg. of chars. *)
let rec repeat parser xs =
    xs |> ((parser <&> (repeat parser) >>> (fun (res1, res2) -> res1 :: res2)) |||
           (empty >>> (fun _ -> [])))

(* Repeat a parser one or more times. ('parser+')
   Result (fst) is a list, eg. of chars. *)
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
    | Punctuation of string
    | Integer of int

type MyTokenizer = MyParser<char, MyToken>

type MyLexer = string list -> string -> MyToken list

let tokOperatorsOld = (repeat1 parsePunctuation) >>> (implode >> Operator)

let tokOperators keywords cs =
    match repeat1 parsePunctuation cs with
    | Some (results, cs) ->  let candidates = keywords
                                              |> List.map explode
                                              |> List.filter (startsWith results)
                             if List.isEmpty candidates then
                                Some (Punctuation <| implode results, cs)
                             else
                                let keyword = candidates
                                              |> List.maxBy List.length
                                let remainder = results
                                                |> List.skip (List.length keyword)
                                Some (Keyword <| implode keyword, remainder @ cs)
    | None -> None
    
let tokInteger =
    (repeat1 parseDigit) >>> fun ds -> ds
                                       |> List.map digitCharToInt
                                       |> List.fold (fun state i -> state * 10 + i) 0
                                       |> Integer

let tokIdentifier (keywords : string list) =
    (parseLetter <&> repeat (parseLetter ||| parseDigit ||| expect '_'))
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
                                 tokOperators  keywords |||
                                 tokInteger)
            >>> snd))


(* Parser/Evaluator *)

type Term =
    | Num of int
    | Neg of Term
    | Add of Term * Term
    | Mul of Term * Term

(* Operator to skip an operator, then continue parsing. *)
let (.|) operator parser =
    expect (Operator operator) <&> parser >>> snd

(* Operator to skip an operator behind a parse expression. *)
let (|.) parser operator =
    parser <&> expect (Operator operator) >>> fst

(* Make an expression optional to parse. ('[parser]') *)
let optional parser =
    (parser >>> Some) ||| (empty >>> fun () -> None)
    (* fun () ... is a tricky one
       because empty >>> None would infer a wrong type *)

let rec arithmeticEval = function
    | Num i -> i
    | Neg a -> -(arithmeticEval a)
    | Add (a, b) -> (arithmeticEval a) + (arithmeticEval b)
    | Mul (a, b) -> (arithmeticEval a) * (arithmeticEval b)




(* Dummy *)

type TestClass() = 
    member this.Hello = "F#"
