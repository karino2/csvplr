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

