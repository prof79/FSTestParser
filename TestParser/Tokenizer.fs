(* KUDOS to alf42red/Armin Heller https://www.youtube.com/user/alf42red *)

module Tokenizer

open System

open Utility
open Basic

(* Tokenizer/Lexer *)

type MyToken =
    | Id of string
    | Keyword of string
    | Punctuation of string
    | Integer of int

type MyTokenizer = MyParser<char, MyToken>

type MyLexer = string list -> string -> MyToken list

[<Obsolete("Original implementation, superseded by tokPunctuation.", false)>]
let tokPunctuationOld = (repeat1 parsePunctuation) >>> (implode >> Punctuation)

(* Parses an operator or punctuation from a char list to a token. *)
let tokPunctutation keywords cs =
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

(* Parses an integer from a char list to a token. *)    
let tokInteger =
    (repeat1 parseDigit) >>> fun ds -> ds
                                       |> List.map digitCharToInt 
                                       |> List.fold (fun state i -> state * 10 + i) 0
                                       |> Integer

(* Parses a string from a char list to a keyword or identifier. *)
let tokIdentifier (keywords : string list) =
    (parseLetter <&> repeat (parseLetter ||| parseDigit ||| expect '_'))
        >>> fun (res1, res2) -> let ident = res1 :: res2
                                match keywords
                                      |> List.tryFind (fun kw -> kw = implode ident)
                                      with
                                      | Some (kw) -> Keyword kw
                                      | None -> Id <| implode ident

(* Lexer - parses a string to a list of tokens.
   Keywords must be supplied as a string list parameter. *)
let lex keywords str =
    (* convert string input to character list *)
    explode str
    (* ignore spaces then match elements of our mini-language *)
    |> (repeat (parseSpaces <&> (tokIdentifier   keywords |||
                                 tokPunctutation keywords |||
                                 tokInteger)
            >>> snd))
