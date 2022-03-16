#r "nuget:Deedle"

open System
open Deedle

#r "nuget: FParsec"
open FParsec

#load "Common.fs"
open Common

#load "Parser.fs"
open Parser

#load "Eval.fs"
open Eval

#load "TestUtils.fs"
open TestUtils

let pollenCsv = Frame.ReadCsv "./test/test.csv"
pollenCsv.RowCount |> should 264


runParse pexpr "pollen != -9999" |> filterDf pollenCsv
|> countRow |> should 257

runParse pexpr "pollen >= 0 && pollen < 10" |> filterDf pollenCsv |> countRow |> should 229

runParse pAssignList "dtonly=date(date)"
|> mutateDf pollenCsv

pollenCsv.ColumnKeys |> Seq.contains "dtonly" |> should true

runParse pAssignList "year=year(date), month=month(date)"
|> mutateDf pollenCsv

pollenCsv.ColumnKeys |> Seq.contains "year" |> should true
pollenCsv.ColumnKeys |> Seq.contains "month" |> should true

let newcols = pollenCsv.ColumnKeys |> Seq.toArray
newcols.[3] |> should "dtonly"
pollenCsv.Rows[0].Get("dtonly") |> should "2022-02-25"

let filterGt = runParse pexpr "dtonly > \"2022-03-06\"" |> filterDf pollenCsv
filterGt.RowCount |> should 24

let filterGe = runParse pexpr "dtonly >= \"2022-03-06\"" |> filterDf pollenCsv
filterGe.RowCount |> should 48

let gbcsv = Frame.ReadCsv "./test/test_groupby.csv"

let assignExpr2 = runParse pAssignment "perday=sum(pollen)"
summarise assignExpr2 gbcsv

let gbcsv_na = Frame.ReadCsv("./test/test_groupby_na.csv", inferTypes=false)

// should no exception.
runParse pAssignment "perday=sum(pollen)"
|> summariseDf gbcsv_na |> ignore

runParse pexpr "is.na(pollen)"
|> filterDf gbcsv_na |> countRow |> should 24

runParse pexpr "!is.na(pollen)"
|> filterDf gbcsv_na |> countRow |> should 240 

// filter for na is always filter-out.
runParse pexpr "pollen < 9999"
|> filterDf gbcsv_na |> countRow |> should 240

// filter for na is always filter-out.
runParse pexpr "pollen > -9999"
|> filterDf gbcsv_na |> countRow |> should 240