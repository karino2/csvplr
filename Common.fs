module Common

open System

type ValueType =
| Number
| Float
| String
| DateTime

type Value = 
| Number of int
| Float of float
| Bool of bool
| String of string


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

