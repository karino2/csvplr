module TestUtils

open System
open FParsec
open Eval

type CsvplrException(message) = inherit Exception(message)

let should left right =
    if left <> right then
        raise (CsvplrException (sprintf "Not equal, left=(%A), right=(%A)" left right))

let shouldNot left right =
    if left = right then
        raise (CsvplrException (sprintf "Wrongly Equal, left=(%A), right=(%A)" left right))

let shouldSome a =
    match a with
    | Some _ -> ()
    | None -> raise (CsvplrException "Wrongly None")

let shouldNone a =
    match a with
    | Some value -> raise (CsvplrException (sprintf "Wrongly Some: %A" value))
    | None -> ()

let runParse p str = 
    match run p str with
    | Success(result, _, _)   ->
        result
    | Failure(errorMsg, _, _) -> raise (CsvplrException (sprintf "Parse fail: %s" errorMsg))

let mutateDf df expr =
    mutateWithExpr expr df

let filterDf df expr =
    filterWithExpr expr df

let summariseDf df expr =
    summarise expr df
