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


let excludeExpr = runParse pexpr "pollen != -9999"
let excluded = filterWithExpr excludeExpr pollenCsv
excluded.RowCount |> should 257


let assignExpr = runParse pAssignment "dtonly=date(date)"
mutateWithExpr assignExpr pollenCsv

let newcols = pollenCsv.ColumnKeys |> Seq.toArray
newcols.[3] |> should "dtonly"
pollenCsv.Rows[0].Get("dtonly") |> should "2022-02-25"


let gbcsv = Frame.ReadCsv "./test/test_groupby.csv"

let assignExpr2 = runParse pAssignment "perday=sum(pollen)"
summarise assignExpr2 gbcsv
