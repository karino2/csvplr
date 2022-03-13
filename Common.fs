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
| GtOp
| GeOp
| LtOp
| LeOp

type UnaryOpType =
| NotOp


type Rexpr =
| Atom of Atom
| UnaryOp of (UnaryOpType * Rexpr)
| BinOp of (BinOpType * Rexpr * Rexpr)
| Funcall of (string * Rexpr list)

type Assign = {identifier: string; rexpr: Rexpr}

