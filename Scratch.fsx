#r "nuget:Deedle"

#load "Deedle.fsx"

#r "nuget: FParsec"
open FParsec

#load "Common.fs"
open Common

#load "Parser.fs"
open Parser

open System
open Deedle

let pollenCsv = Frame.ReadCsv "test/test.csv"

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

let test p str =
    match run p str with
    | ParserResult.Success(result, _, _)   -> printfn "Success: %A" result
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg




test pexpr "123"
test pexpr "123.0"

test pexpr "pollen != -9999"

test pexpr "pollen == \"2022-03-08\""
test pexpr "pollen >= \"2022-03-08\""
test pexpr "pollen > \"2022-03-08\""
test pexpr "pollen <= \"2022-03-08\""
test pexpr "pollen < \"2022-03-08\""


test pfloat "123"

#load "Eval.fs"
open Eval

let evalFilter df p str = 
    match run p str with
    | ParserResult.Success(result, _, _)   ->
        printfn "Success: %A" result
        let res = filterWithExpr result df
        res.Print()
    | Failure(errorMsg, _, _) -> printfn "Failure: %s" errorMsg


evalFilter pollenCsv pexpr "pollen != -9999"

evalFilter pollenCsv pexpr "pollen == 7"



let ymdCsv = Frame.ReadCsv("test/test_with_ymd.csv", inferTypes=false) 
evalFilter ymdCsv pexpr "dtonly == \"2022-03-07\""
evalFilter ymdCsv pexpr "dtonly >= \"2022-03-06\""



#load "TestUtils.fs"
open TestUtils

let funcall = runParse pexpr "date(date)"

evalRow funcall pollenCsv.Rows[0]


runParse pAssignment "dtonly=date(date)"
|> mutateDf pollenCsv

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




toGroupByColumnName varlist

let cells = pollenCsv.Rows.[0].GetItems(["year"; "month"])



pollenCsv |> Frame.mapRowValues (fun row-> rowToGroupByCell varlist row)

groupBy varlist pollenCsv
pollenCsv


let noinfer = Frame.ReadCsv("/Users/arinokazuma/Downloads/pollen_14208_20220225_20220307.csv", inferTypes=false) 
noinfer.GetColumn<DateTime>("date")


splitToList "!2022!2!25!"


let ymdCsv = Frame.ReadCsv "test/test_with_ymd.csv"
groupBy ["year"; "month"; "day"] ymdCsv
ymdCsv
ymdCsv.SaveCsv("test/test_groupby.csv", includeRowKeys=false)

ymdCsv.ColumnKeys |> Seq.tryFind (fun key -> key.StartsWith "!csvplr_group_by_zzz!")





let (Some target2) = findGBCN ymdCsv

let cols = GBCN2CNs target2

let gbcsv = ymdCsv

let bygb2 = doGroupBy target2 gbcsv

let aggrcol = aggregate "pollen" Stats.sum bygb2


recoverDf "pollen" aggrcol cols bygb2


let keycolArr = recoverKeyCol cols bygb2
let colnames2 = Array.append cols [|"pollen"|]
let colvals2 = Array.append keycolArr [|aggrcol|] 

Array.zip colnames2 colvals2
|> Frame.ofColumns



let gb = gbcsv |> Frame.groupRowsByString target2

// let gb = gbcsv.GroupRowsUsing (fun _ row-> row.GetAs<string>(target2))

gb.ColumnKeys
gb.RowKeys

let bygb = gb |> Frame.nest

bygb.Get("!2022!3!4!").GetColumn<float>("pollen") |> Stats.sum
bygb.Get("!2022!3!5!").GetColumn<float>("pollen") |> Stats.sum
bygb.Get("!2022!3!6!").GetColumn<float>("pollen") |> Stats.mean

bygb.Keys


let collist = cols |> Array.toList
collist.Tail

"abc"::collist

Array.append

let pymd = bygb |> Series.map (fun k m->
     let aggr = m.GetColumn<float>("pollen") |> Stats.sum |> sprintf "%O"
     let ymd = splitToList k |> Array.toList
     aggr::ymd
     )

pymd





let colnames = Array.append cols [|"pollen"|]
let colvals = Array.append ymdcolArr [|polcol|] 

Array.zip colnames colvals 
|> Frame.ofColumns


let polcol = bygb |> Series.mapValues (fun m -> m.GetColumn<float>("pollen") |> Stats.sum |> sprintf "%O")
let ymdlistCol = bygb |> Series.map (fun k _-> splitToList k)
let ycol = ymdlistCol |> Series.mapValues (fun m->m.[0])
let mcol = ymdlistCol |> Series.mapValues (fun m->m.[1])
let dcol = ymdlistCol |> Series.mapValues (fun m->m.[2])

ymdlistCol.GetAt(0)

let test = ["a"; "b"]
List.append test ["c"]

Array.append [|"a"; "b"|] [|"c"|]

let ymdcolArr = cols |> Array.mapi (fun i _-> ymdlistCol |> Series.mapValues (fun m->m.[i]))

(Series<string, 'T>)




Array.append ymdcolArr [|polcol|]




let hoge = [ycol; mcol; dcol; polcol]

Frame(["year"; "month"; "day"; "pollen"], [ycol; mcol; dcol; polcol])



let hoge3 = ["year"=> ycol; "month" => mcol]

Frame.ofColumns hoge3


Frame(["year"; "month"; "day"; "pollen"], hoge)

Frame(colnames, colvals)


Frame( (Array.append cols [|"pollen"|]), (Array.append ymdcolArr [|polcol|]))


gb
|> Frame.mapRows (fun rowkey row-> 1)

gb.GetColumn<float>("pollen")
|> Series.mapValues (fun t)


gb.GetColumn<float>("pollen")
|> Series.mapValues (Stats.sum fst)

let target = toGroupByColumnName ["year"; "month"; "day"]

target.StartsWith "!csvplr_group_by_zzz!"


"!2022!2!25!".Substring(1, )

"!2022!2!25!".Split("!").[1..]


pollenCsv.Rows.[0].GetAs<string>("year")


cells.GetAllValues |> String.concat "!"


.Select("year", "month")

 df |> Frame.mapRowValues (fun row-> evalRowAsString assignExpr.rexpr row)

