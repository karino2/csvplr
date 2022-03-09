module Eval

open Deedle
open Common



let guessType (expr:Rexpr) =
    match expr with
    | Atom x ->
        match x with
        | String _ -> Some ValueType.String
        | Number _ -> Some ValueType.Number
        | Float _ -> Some ValueType.Float
        | Variable _ -> None
    | BinOp _ -> Some ValueType.Number // bool, 
    | Funcall (fname, _) -> 
        match fname with
        | "date" -> Some ValueType.String
        | _ -> None

let resolveType ltp rtyp =
    match ltp with
    | None ->
        match rtyp with
        | None -> ValueType.String
        | Some tp -> tp
    | Some tp -> tp

(*
let evalVariable (valtype:ValueType) varname (row:ObjectSeries<string>) =
    match valtype with
    | ValueType.Number -> row.GetAs<int>(varname)
    | ValueType.Float -> row.GetAs<float>(varname)
    | ValueType.String -> row.GetAs<string>(varname)
*)

let evalStringAtom (x:Atom) (row:ObjectSeries<string>) =
    match x with
    | String x -> x        
    | Variable varname -> row.GetAs<string>(varname)
    | _ -> failwith "evalStringAtom with number or other nonstring atom. Never reached here"

let evalIntAtom (x:Atom) (row:ObjectSeries<string>) =
    match x with
    | Number n -> n
    | Float n -> (int n)
    | Variable varname -> row.GetAs<int>(varname)
    | _ -> failwith "evalIntAtom with non number. Never reached here"



let evalRowAsBool (expr:Rexpr) row =
    match expr with
    | BinOp (optype, lop, rop) ->
        let ltype = guessType lop
        let rtype = guessType rop
        let argtype = resolveType ltype rtype
        match argtype with
        | ValueType.Number ->
            match (lop, rop) with
            | (Atom la), (Atom ra) ->
                let larg = evalIntAtom la row
                let rarg = evalIntAtom ra row
                match optype with
                | EqOp -> larg = rarg
                | NeqOp -> larg <> rarg
            | _ -> failwith "NYI"
        | _ -> failwith "NYI2"
    | _ -> failwith "NYI3"

let filterWithExpr expr df =
    // pollenCsv.Rows |> Series.filterValues (fun row -> row?pollen <> -9999 )
    df |> Frame.filterRowValues (fun row ->
        evalRowAsBool expr row
        )
