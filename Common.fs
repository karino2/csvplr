module Common

open System

type CValueType =
| Number
| Float
| Bool
| String
| DateTime

type CValue = 
| Number of int
| Float of float
| Bool of bool
| String of string
| Date of DateTime


type Atom =
| String of string
| Number of int
| Float of float
| Variable of string


type BinOpType = 
| EqOp
| NeqOp

type Rexpr =
| Atom of Atom
| BinOp of (BinOpType * Rexpr * Rexpr)
| Funcall of (string * Rexpr list)

type Assign = {identifier: string; rexpr: Rexpr}

