#r "nuget:Deedle"

#load "Deedle.fsx"

open System
open Deedle

let pollenCsv = Frame.ReadCsv "/Users/arinokazuma/Downloads/pollen_14208_20220225_20220307.csv"

pollenCsv

pollenCsv |> Frame.indexRowsDate "date"

DateTime.Now.Date

pollenCsv.Columns.["date"]

pollenCsv.GetColumn<int>("pollen") |> Series.filterValues ((<>) -9999)


pollenCsv.Columns
pollenCsv.Rows

pollenCsv.Rows.[263]
pollenCsv.Rows.[0].GetAs<DateTime>("date")


let pollen012 = pollenCsv.Rows.[[0; 1; 2]]

pollen012?pollen
pollen012?citycode

// NG
pollen012?date

DateTime.Now.ToString("yyyy-MM-dd")


// add field
pollenCsv?dateonly <-  pollenCsv |> Frame.mapRowValues (fun row->row.GetAs<DateTime>("date").Date)


let series = pollenCsv |> Frame.mapRowValues (fun row->row.GetAs<DateTime>("date").Date)

pollenCsv.AddColumn("dateonly", series)
pollenCsv


pollenCsv
pollenCsv.ColumnKeys

// filter
pollenCsv.Rows |> Series.filterValues (fun row -> row?pollen <> -9999 ) |> Frame.ofRows

pollenCsv |> Frame.filterRowValues (fun row -> row?pollen <> -9999)

printfn "%A" pollenCsv

pollenCsv.Print()

Console.WriteLine("hello {0}", pollenCsv)


pollenCsv.ColumnKeys

let tps = pollenCsv.ColumnTypes |> Seq.toArray

tps.[0].ToString()
tps.[1].ToString()
tps.[2].ToString()


// NG
pollenCsv?dateonly <- pollenCsv?date


// Parser

#r "nuget: FParsec"
open FParsec

#load "Common.fs"
open Common

#load "Parser.fs"
open Parser

let test p str =
    match run p str with
    | Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg




test pexpr "123"
test pexpr "123.0"

test pexpr "pollen != -9999"

test pfloat "123"

#load "Eval.fs"
open Eval

let eval p str = 
    match run p str with
    | Success(result, _, _)   ->
        printfn "Success: %A" result
        let res = filterWithExpr result pollenCsv
        res.Print()
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg


eval pexpr "pollen != -9999"

eval pexpr "pollen == 7"


#load "TestUtils.fs"
open TestUtils

let funcall = runParse pexpr "date(date)"

evalRow funcall pollenCsv.Rows[0]

let mutateDf df expr =
    mutateWithExpr expr df

runParse pAssignment "year=year(date)"
|> mutateDf pollenCsv

runParse pAssignment "month=month(date)"
|> mutateDf pollenCsv

runParse pAssignment "day=day(date)"
|> mutateDf pollenCsv

pollenCsv.SaveCsv("test/test_with_ymd.csv", includeRowKeys=false)

pollenCsv.ColumnTypes

pollenCsv.ColumnTypes |> Seq.map (fun tp->tp.ToString())
pollenCsv.GetColumn "pollen"


pollenCsv?year.[0]

Console.WriteLine("{0}", (float 3))

printfn "%O" (float 3)
printfn "%O" (float 3.1)

3.1 = 3.1

(float 3)

let varlist = runParse pvarlist "year,month,day"

varlist |> String.concat "!" |> sprintf "!%s!"



let split_to_list (enclosed:string) =
    enclosed.Substring(1, enclosed.Length-2).Split("!")

toGroupByColumnName varlist

let cells = pollenCsv.Rows.[0].GetItems(["year"; "month"])



pollenCsv |> Frame.mapRowValues (fun row-> rowToGroupByCell varlist row)

groupBy varlist pollenCsv
pollenCsv

split_to_list "!2022!2!25!"


"!2022!2!25!".Substring(1, )

"!2022!2!25!".Split("!").[1..]


pollenCsv.Rows.[0].GetAs<string>("year")


cells.GetAllValues |> String.concat "!"


.Select("year", "month")

 df |> Frame.mapRowValues (fun row-> evalRowAsString assignExpr.rexpr row)

