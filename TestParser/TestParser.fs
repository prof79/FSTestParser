(* KUDOS to alf42red/Armin Heller https://www.youtube.com/user/alf42red *)

module TestParser

open System.Diagnostics

open Basic
open Tokenizer

(* Parser/Evaluator *)

type Term =
    | Num of int
    | Neg of Term
    | Add of Term * Term
    | Sub of Term * Term
    | Mul of Term * Term
    | Div of Term * Term

(* Operator to skip an operator (keyword), then continue parsing. *)
let (.|) operator parser =
    expect (Keyword operator) <&> parser >>> snd

(* Experimental *)
let (..|) (operator1, operator2) parser =
    (expect (Keyword operator1) ||| expect (Keyword operator2))
        <&> parser

(* Operator to skip an operator (keyword) behind a parse expression. *)
let (|.) parser operator =
    parser <&> expect (Keyword operator) >>> fst

(* Make an expression optional to parse. ('[parser]') *)
let optional parser =
    (parser >>> Some) ||| (empty >>> fun () -> None)
    (* fun () ... is a tricky one
       because empty >>> None would infer a wrong type *)

let rec arithmeticEval = function
    | Num i -> i
    | Neg a -> -(arithmeticEval a)
    | Add (a, b) -> (arithmeticEval a) + (arithmeticEval b)
    | Sub (a, b) -> (arithmeticEval a) - (arithmeticEval b)
    | Mul (a, b) -> (arithmeticEval a) * (arithmeticEval b)
    | Div (a, b) -> (arithmeticEval a) / (arithmeticEval b)

(* Parse an Integer token to a Term of our arithmetic language. *)
let number =
    check (function
            | Integer i -> true
            | _ -> false) >>> (fun (Integer i) -> Num i)

(*
    Combines two parsers to an optional binary operation:
    
    A ::= parser1 ["operator" parser2]

    Operator is the string representation of the operator (keyword).
    OpTerm is the terminal symbol used for the binary operation result.
*)
let binParserOp (parser1 : MyParser<MyToken, Term>)
                (operator : string)
                (toOpTerm : ('a * 'b) -> Term)
                (parser2 : MyParser<MyToken, Term>)
                : MyParser<MyToken, Term> =
    (parser1 <&> optional (operator .| parser2))
                >>> function
                    | (t1, Some (t2)) -> Debug.WriteLine (sprintf "binOp: bin %s %A (%A, %A)" operator toOpTerm t1 t2)
                                         toOpTerm (t1, t2)
                    | (t1, None) -> Debug.WriteLine (sprintf "binOp: single %s %A (%A)" operator toOpTerm t1)
                                    t1

(* Experimental *)
let binParserOp' (parser1 : MyParser<MyToken, Term>)
                 (operator : string * string)
                 (toOpTerm : MyToken -> ('a * 'b) -> Term)
                 (parser2 : MyParser<MyToken, Term>)
                 : MyParser<MyToken, Term> =
    let operator1, operator2 = operator
    (parser1 <&> optional ((operator1, operator2) ..| parser2))
                >>> function
                    | (t1, Some (t2Op, t2)) -> Debug.WriteLine (sprintf "binOp: bin %A %A (%A, %A)" operator toOpTerm t1 t2)
                                               toOpTerm t2Op (t1, t2)
                    | (t1, None) -> Debug.WriteLine (sprintf "binOp: single %A %A (%A)" operator toOpTerm t1)
                                    t1

(*
    EBNF:

    term0 ::= number | '(' term3 ')'
    term1 ::= '-' term1 | term0
    term2 ::= term1 ['*' term2 ]
    term3 ::= term2 ['+' term3 ]
    wrong: term4 ::= term3 ['-' term4 ]

    expr ::= term | '(' expr ')'
    term ::= factor ['+'] term
    factor ::= number ['*'] expr
*)

(*
    This functions parse lexed tokens to terminal tokens
    of our arithmetic language.

    Wikipedia:
    "In a strict language like OCaml, we can avoid the infinite recursion
    problem by forcing the use of a closure."
*)
let rec term0 toks =
    Debug.WriteLine "term0"
    toks |> (number ||| ("(" .| term3 |. ")"))
and term1 toks =
    Debug.WriteLine "term1"
    toks |> (("-" .| term1 >>> fun t1 -> Neg t1) ||| term0)
and term2 toks =
    Debug.WriteLine "term2"
    toks |> binParserOp term1 "*" Mul term2
and term3 toks =
    Debug.WriteLine "term3"
    toks |> binParserOp term2 "+" Add term3
    (*toks |> binParserOp' term2 ("+", "-") (fun kw (a, b) ->
        match kw with
        | Keyword "+" -> Add (a, b)
        | Keyword "-" -> Sub (a, b))
            term3*)

(* Test function to lex and parse an arithmetic string input to terminal symbols. *)
let arithmeticTest keywords str =
    match lex keywords str with
    | Some (toks, cs) -> if not <| List.isEmpty cs then
                             Debug.WriteLine(sprintf "Unparsed input left: %A" cs)
                         match term3 toks with
                         | Some (term, toks) -> if not <| List.isEmpty toks then
                                                    Debug.WriteLine(sprintf "Unparsed tokens left: %A" toks)
                                                Some (term)
                         | None -> None
    | None -> None



(* Dummy *)

type TestClass() = 
    member this.Hello = "F#"

