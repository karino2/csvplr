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
    | Funcall (fname, _) -> 
        match fname with
        | "date" -> Some CValueType.String
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
    | CValueType.Number -> CValue.Number (row.GetAs<int>(varname))
    | CValueType.Float ->CValue.Float  (row.GetAs<float>(varname))
    | CValueType.Bool -> CValue.Bool (row.GetAs<bool>(varname))
    | CValueType.String -> CValue.String (row.GetAs<string>(varname))
    | CValueType.DateTime -> CValue.Date (row.GetAs<DateTime>(varname))


let evalAtom (valtype:CValueType) (x:Atom) row =
    match x with
    | Number n -> CValue.Number n
    | Float n -> CValue.Float n
    | String n -> CValue.String n
    | Variable varname ->  evalVariable valtype varname row


let evalRow (expr:Rexpr) row =

    let rec evalRowT valtype (expr:Rexpr) : CValue =
        match expr with
        | BinOp (optype, lop, rop) ->
            let ltype = guessType lop
            let rtype = guessType rop
            let argtype = resolveType ltype rtype
            let larg = evalRowT argtype lop
            let rarg = evalRowT argtype rop
            match optype with
            | EqOp -> CValue.Bool (larg = rarg)
            | NeqOp -> CValue.Bool (larg <> rarg)
        | Atom a ->
            evalAtom valtype a row
        | _ -> failwith "NYI3"

    match expr with
    | BinOp _ ->
        // valtype not used in this case.
        evalRowT CValueType.Bool expr
    | Atom _ ->
        // No type information, if this is variable, treat as string.
        evalRowT CValueType.String expr
    | Funcall (fid, argexprs) ->
        let evalAsDate arg =
            let farg = evalRowT CValueType.DateTime arg
            match farg with
            | Date d -> d
            | _ -> failwith "date related function with non date arg"

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
        | _ -> failwith "NYI4"
 

let evalRowAsBool expr row =
    let cval = evalRow expr row
    match cval with
    | CValue.Bool b -> b
    | _ -> failwith "Non bool type in evalRowAsBool"

let filterWithExpr expr df =
    // pollenCsv.Rows |> Series.filterValues (fun row -> row?pollen <> -9999 )
    df |> Frame.filterRowValues (fun row ->
        evalRowAsBool expr row
        )

let evalRowAsString expr row =
    let cval = evalRow expr row
    match cval with
    | CValue.Bool x -> x.ToString()
    | CValue.Number x -> x.ToString()
    | CValue.Float x -> x.ToString()
    | CValue.Date x -> x.ToString()
    | CValue.String x -> x


let mutateWithExpr (assignExpr:Assign) df =
    let newcolumn = df |> Frame.mapRowValues (fun row-> evalRowAsString assignExpr.rexpr row)
    df.AddColumn(assignExpr.identifier, newcolumn)


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

