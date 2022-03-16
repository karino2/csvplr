module Eval

open Deedle
open Common
open System

let guessType (expr:Rexpr) =
    match expr with
    | Atom x ->
        match x with
        | String _ -> Some CValueType.String
        | Number _ -> Some CValueType.Number
        | Float _ -> Some CValueType.Float
        | Variable _ -> None
    | BinOp _ -> Some CValueType.Number // bool, 
    | UnaryOp (NotOp, _) -> Some CValueType.Bool 
    | Funcall (fname, _) -> 
        match fname with
        | "date" -> Some CValueType.String
        | "is.na" -> Some CValueType.Bool
        | _ -> None

let resolveType ltp rtyp =
    match ltp with
    | None ->
        match rtyp with
        | None -> CValueType.String
        | Some tp -> tp
    | Some tp -> tp

let evalVariable (valtype:CValueType) varname (row:ObjectSeries<'T>) =
    match valtype with
    | CValueType.Number ->
        try
            CValue.Number (row.GetAs<int>(varname))
        with
        | _ -> Missing
    | CValueType.Float ->
        try
            CValue.Float  (row.GetAs<float>(varname))
        with
        | _ -> Missing
    | CValueType.Bool -> CValue.Bool (row.GetAs<bool>(varname))
    | CValueType.String -> CValue.String (row.GetAs<string>(varname))
    | CValueType.DateTime -> CValue.Date (row.GetAs<DateTime>(varname))


let evalAtom (valtype:CValueType) (x:Atom) row =
    match x with
    | Number n -> CValue.Number n
    | Float n -> CValue.Float n
    | String n -> CValue.String n
    | Variable varname ->  evalVariable valtype varname row

let str2float (s:string) =
    match System.Double.TryParse s with
    | (true, v) -> v
    | _ -> nan


let cvalue2bool cval =
    match cval with
    | CValue.Bool b -> b
    | CValue.Number n -> n <> 0
    | CValue.String str -> str <> ""
    | _ -> failwith "cvalue2bool, unsupported type."

let rec evalRow (expr:Rexpr) row =

    let rec evalRowT valtype (expr:Rexpr) : CValue =
        match expr with
        | BinOp (optype, lop, rop) ->
            let ltype = guessType lop
            let rtype = guessType rop
            let argtype = resolveType ltype rtype
            let larg = evalRowT argtype lop
            let rarg = evalRowT argtype rop
            match larg, rarg with
            | Missing, _ -> CValue.Missing
            | _, Missing -> CValue.Missing
            | _, _ -> 
                match optype with
                | EqOp -> CValue.Bool (larg = rarg)
                | NeqOp -> CValue.Bool (larg <> rarg)
                | LtOp -> CValue.Bool (larg < rarg)
                | LeOp -> CValue.Bool (larg <= rarg)
                | GtOp -> CValue.Bool (larg > rarg)
                | GeOp -> CValue.Bool (larg >= rarg)
                | OrOp ->CValue.Bool ((cvalue2bool larg) || (cvalue2bool rarg))
                | AndOp ->CValue.Bool ((cvalue2bool larg) && (cvalue2bool rarg))

        | Atom a ->
            evalAtom valtype a row
        | _ -> failwith "NYI3"

    match expr with
    | BinOp _ ->
        // valtype not used in this case.
        evalRowT CValueType.Bool expr
    | UnaryOp (NotOp, expr) ->
        let evalAsBool arg =
            let farg = evalRow arg row
            match farg with
            | CValue.Number n -> if n = 0 then false else true
            | CValue.Float n -> if n = 0 then false else true 
            | CValue.Bool n -> n
            | CValue.String n -> if n = "" then false else true
            | CValue.Missing -> failwith "NYI of missing value of !"
            | _ -> failwith "evalAsBool with unknown type. Never reached here."


        let argval = evalAsBool expr
        CValue.Bool (not argval)
    | Atom _ ->
        // No type information, if this is variable, treat as string.
        evalRowT CValueType.String expr
    | Funcall (fid, argexprs) ->
        let evalAsDate arg =
            let farg = evalRowT CValueType.DateTime arg
            match farg with
            | Date d -> d
            | _ -> failwith "date related function with non date arg"

        let evalAsString arg =
            let farg = evalRow arg row
            match farg with
            | CValue.Number n -> sprintf "%O" n
            | CValue.Float n -> sprintf "%O" n
            | CValue.Bool n -> sprintf "%O" n
            | CValue.String n -> n
            | _ -> failwith "evalAsString with unknown type. Never reached here."

        let evalAsFloat arg =
            let farg = evalRow arg row
            match farg with
            | CValue.Number n -> float n
            | CValue.Float n -> n
            | CValue.Bool n -> if n then 1 else 0
            | CValue.String n -> str2float n
            | _ -> failwith "evalAsFloat with unknown type. Never reached here."

        match (fid, argexprs) with
        | ("date", onearg::[]) -> 
            let d = evalAsDate onearg
            CValue.String(d.Date.ToString("yyyy-MM-dd"))
        | ("year", onearg::[]) -> 
            let d = evalAsDate onearg
            CValue.String(d.Year.ToString())
        | ("month", onearg::[]) -> 
            let d = evalAsDate onearg
            CValue.String(d.Month.ToString())
        | ("day", onearg::[]) -> 
            let d = evalAsDate onearg
            CValue.String(d.Day.ToString())
        | ("hour", onearg::[]) -> 
            let d = evalAsDate onearg
            CValue.String(d.Hour.ToString())
        | ("minute", onearg::[]) -> 
            let d = evalAsDate onearg
            CValue.String(d.Minute.ToString())
        | ("paste0", _) ->
            argexprs |> List.map evalAsString |> String.concat "" |> CValue.String
        | ("is.na", onearg::[]) -> 
            let d = evalAsFloat onearg
            CValue.Bool (System.Double.IsNaN d)
        | _ -> failwithf "NYI4, %A" fid
 

let evalRowAsBoolForFilter expr row =
    let cval = evalRow expr row
    match cval with
    | CValue.Bool b -> b
    | CValue.Missing -> false
    | _ -> failwith "Non bool type in evalRowAsBool"

let filterWithExpr expr df =
    df |> Frame.filterRowValues (fun row ->
        evalRowAsBoolForFilter expr row
        )

let evalRowAsString expr row =
    let cval = evalRow expr row
    match cval with
    | CValue.Bool x -> x.ToString()
    | CValue.Number x -> x.ToString()
    | CValue.Float x -> x.ToString()
    | CValue.Date x -> x.ToString()
    | CValue.String x -> x
    | CValue.Missing -> ""


let mutateWithExpr (assignList:Assign list) df =
    assignList |> List.iter (fun assignExpr->
        let newcolumn = df |> Frame.mapRowValues (fun row-> evalRowAsString assignExpr.rexpr row)
        df.AddColumn(assignExpr.identifier, newcolumn)
    )


//
// group by
//

let groupby_special_key = "csvplr_group_by_zzz"

let encloseWithSep (strlist: string list) =
    strlist |> String.concat "!" |> sprintf "!%s!"

let toGroupByColumnName (varlist: string list) =
    groupby_special_key::varlist |> encloseWithSep

let rowToGroupByCell varlist (row:ObjectSeries<'T>) =
    varlist |> List.map (fun key -> row.GetAs<string> key) |> encloseWithSep

let groupBy varlist df =
    let newcols = df |> Frame.mapRowValues (fun row-> rowToGroupByCell varlist row)
    let newcolname = toGroupByColumnName varlist
    df.AddColumn(newcolname, newcols)

//
// summarise
//

let splitToList (enclosed:string) =
    enclosed.Substring(1, enclosed.Length-2).Split("!")

// GB = GroupBy, ColumnName = CN
let findGBCN (df:Frame<int, string>) =
    df.ColumnKeys |> Seq.tryFind (fun key -> key.StartsWith "!csvplr_group_by_zzz!")


let GBCN2CNs colname =  
    splitToList colname |> (fun arr->arr[1..])

let doGroupBy gbcn df =
    df |> Frame.groupRowsByString gbcn |> Frame.nest

let getFloatCell (row:ObjectSeries<string>) targetname =
    row.GetAs<string> targetname |> str2float

let number2output (num:float) =
    if System.Double.IsNaN(num) then
        ""
    else
        sprintf "%O" num

let aggregate targetname aggrfun (gbdf:Series<string, Frame<int, string>>) =
    gbdf |> Series.mapValues (fun m ->
            m |> Frame.mapRowValues (fun row ->
                getFloatCell row targetname
            ) |> aggrfun |> number2output           
            )

// retrun keycolArr
let recoverKeyCol cols gbdf =
    let keyarrcols = gbdf |> Series.map (fun k _-> splitToList k)
    cols |> Array.mapi (fun i _-> keyarrcols |> Series.mapValues (fun m->m.[i]))

let recoverDf newname aggrcol gbcols gbdf =
    let keycolArr = recoverKeyCol gbcols gbdf
    let colnames = Array.append gbcols [|newname|]
    let colvals = Array.append keycolArr [|aggrcol|] 

    Array.zip colnames colvals
    |> Frame.ofColumns

let summarise (expr:Assign) df =
    let newname = expr.identifier
    match expr.rexpr with
    | Funcall ("sum", onearg::[]) -> 
        match onearg with
        | (Atom (Variable vname)) ->
            match (findGBCN df) with
            | (Some gbcn) ->
                let gcols = GBCN2CNs gbcn
                let gbdf = doGroupBy gbcn df
                let aggrcol = aggregate vname Stats.sum gbdf
                recoverDf newname aggrcol gcols gbdf
            | _ -> failwith "NYI3, sumarise with no grouping"
        | _ -> failwith "NYI2"
    | _ -> failwith "NYI"