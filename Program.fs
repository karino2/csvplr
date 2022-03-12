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
    | [<CliPrefix(CliPrefix.None); AltCommandLine("group_by")>] Group_By of expr:string
    | [<CliPrefix(CliPrefix.None)>] Select of expr:string
    | [<CliPrefix(CliPrefix.None)>] Summarise of expr:string
    | [<CliPrefix(CliPrefix.None)>] Dump

    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Load _ -> "Load csv from path and dump information."
            | Filter _ -> "Filter with expression."
            | Mutate _ -> "Mutate with expression."
            | Group_By _ -> "Group-By with column list."
            | Select _ -> "Select with column list."
            | Summarise _ -> "Summarise with expression. Do after group_by."
            | Dump -> "Read csv from stdin and dump"


[<EntryPoint>]
let main argv =

    let loadCsv path =
        Frame.ReadCsv(path, inferTypes=false)

    let readCsv () =
        Frame.ReadCsv(Console.In, inferTypes=false)

    let saveCsv (csv:Frame<_, _>) =
        csv.SaveCsv(Console.Out, includeRowKeys=false)

    let parser = ArgumentParser.Create<CliArguments>(programName = "csvplr")
    try
        let results = parser.ParseCommandLine(inputs = argv, raiseOnUsage = true)
        if (results.Contains Load) then
            let path = results.GetResult(Load)
            let csv = loadCsv path
            csv.Print()
        elif (results.Contains Filter) then
            let exprArg = results.GetResult(Filter)
            match run pexpr exprArg with
            | Success(expr, _, _)  ->
                let csv = readCsv () 
                          |> filterWithExpr expr
                csv.SaveCsv(Console.Out, includeRowKeys=false)
            | Failure(errorMsg, _, _) -> failwithf "Failure: %s" errorMsg

        elif (results.Contains Mutate) then
            let exprArg = results.GetResult(Mutate)
            match run pAssignment exprArg with
            | Success(expr, _, _)  ->
                let csv = readCsv ()
                mutateWithExpr expr csv
                csv.SaveCsv(Console.Out, includeRowKeys=false)
            | Failure(errorMsg, _, _) -> failwithf "Failure: %s" errorMsg
        elif (results.Contains Group_By) then
            let exprArg = results.GetResult(Group_By)
            match run pvarlist exprArg with
            | Success(varlist, _, _)  ->
                let csv = readCsv ()
                groupBy varlist csv
                csv.SaveCsv(Console.Out, includeRowKeys=false)
            | Failure(errorMsg, _, _) -> failwithf "Failure: %s" errorMsg
        elif (results.Contains Select) then
            let exprArg = results.GetResult(Select)
            match run pvarlist exprArg with
            | Success(varlist, _, _)  ->
                let csv = readCsv ()
                csv.Columns[varlist] |> saveCsv
            | Failure(errorMsg, _, _) -> failwithf "Failure: %s" errorMsg
        elif (results.Contains Summarise) then
            let exprArg = results.GetResult(Summarise)
            match run pAssignment exprArg with
            | Success(expr, _, _)  ->
                let csv = readCsv ()
                let newCsv = summarise expr csv
                newCsv.SaveCsv(Console.Out, includeRowKeys=false)
            | Failure(errorMsg, _, _) -> failwithf "Failure: %s" errorMsg
        elif (results.Contains Dump) then
            let csv = readCsv ()
            csv.SaveCsv(Console.Out, includeRowKeys=false)
            // csv.Print()
        0
    with e ->
        printfn "%s" e.Message
        1
