(* KUDOS to alf42red/Armin Heller https://www.youtube.com/user/alf42red *)

module Basic

open Utility

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
   to a parser result with unit as the result and itself as the remainder.
   It is like an identity function for parser lists. *)
let empty xs : MyParserResult<'a, unit> = Some ((), xs)

(* Repeat a parser zero or more times. ('parser*')
   Result (fst) is a list, eg. of chars. *)
let rec repeat parser xs =
    xs |> ((parser <&> (repeat parser) >>> fun (res1, res2) -> res1 :: res2) |||
           (empty >>> fun _ -> []))

(* Repeat a parser one or more times. ('parser+')
   Result (fst) is a list, eg. of chars. *)
let repeat1 parser xs =
    xs |> (parser <&> (repeat parser) >>> fun (res1, res2) -> res1 :: res2)

(* Parse spaces. In this implementation they are optional
   (thus "repeat" and not "repeat1"). *)
let parseSpaces = repeat parseSpace
