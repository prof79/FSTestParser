(* KUDOS to alf42red/Armin Heller https://www.youtube.com/user/alf42red *)

module Utility

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
