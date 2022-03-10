open Argu
open Deedle
open System
open Common
open Parser
open FParsec
open Eval

type CliArguments =
    | [<CliPrefix(CliPrefix.None)>] Load of path:string
    | [<CliPrefix(CliPrefix.None)>] Filter of expr:string
    | [<CliPrefix(CliPrefix.None)>] Mutate of expr:string
    | [<CliPrefix(CliPrefix.None)>] Group_By of expr:string
    | [<CliPrefix(CliPrefix.None)>] Dump

    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Load _ -> "Load csv from path and dump information."
            | Filter _ -> "Filter with expression."
            | Mutate _ -> "Mutate with expression."
            | Group_By _ -> "Group-By with column list."
            | Dump -> "Read csv from stdin and dump"


[<EntryPoint>]
let main argv =

    let parser = ArgumentParser.Create<CliArguments>(programName = "csvplr")
    try
        let results = parser.ParseCommandLine(inputs = argv, raiseOnUsage = true)
        if (results.Contains Load) then
            let path = results.GetResult(Load)
            let csv = Frame.ReadCsv path
            csv.Print()
        elif (results.Contains Filter) then
            let exprArg = results.GetResult(Filter)
            match run pexpr exprArg with
            | Success(expr, _, _)  ->
                let csv = Frame.ReadCsv Console.In
                          |> filterWithExpr expr
                csv.SaveCsv(Console.Out, includeRowKeys=false)
            | Failure(errorMsg, _, _) -> failwithf "Failure: %s" errorMsg

        elif (results.Contains Mutate) then
            let exprArg = results.GetResult(Mutate)
            match run pAssignment exprArg with
            | Success(expr, _, _)  ->
                let csv = Frame.ReadCsv Console.In
                mutateWithExpr expr csv
                csv.SaveCsv(Console.Out, includeRowKeys=false)
            | Failure(errorMsg, _, _) -> failwithf "Failure: %s" errorMsg
        elif (results.Contains Group_By) then
            let exprArg = results.GetResult(Group_By)
            match run pvarlist exprArg with
            | Success(varlist, _, _)  ->
                let csv = Frame.ReadCsv Console.In
                groupBy varlist csv
                csv.SaveCsv(Console.Out, includeRowKeys=false)
            | Failure(errorMsg, _, _) -> failwithf "Failure: %s" errorMsg
        elif (results.Contains Dump) then
            let csv = Frame.ReadCsv Console.In
            csv.SaveCsv(Console.Out, includeRowKeys=false)
            // csv.Print()
        0
    with e ->
        printfn "%s" e.Message
        1
