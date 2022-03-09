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
    | _ -> failwith "NYI3"


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
