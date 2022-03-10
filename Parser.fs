module Parser

open FParsec
open Common


let ws = spaces
let str_ws s = pstring s .>> ws

let pidentifier =
    let isIdentifierFirstChar c = isLetter c || c = '_' || c = '$'
    let isIdentifierChar c = isLetter c || isDigit c || c = '_'

    many1Satisfy2L isIdentifierFirstChar isIdentifierChar "identifier"
    .>> ws // skips trailing whitespace

let pstringLiteral =
    let normalChar = satisfy (fun c -> c <> '\\' && c <> '"')
    let unescape c = match c with
                     | 'n' -> '\n'
                     | 'r' -> '\r'
                     | 't' -> '\t'
                     | c   -> c
    let escapedChar = pstring "\\" >>. (anyOf "\\nrt\"" |>> unescape)
    between (pstring "\"") (pstring "\"")
            (manyChars (normalChar <|> escapedChar))

// just yet another string literal.
let pregexLiteral =
    let normalChar = satisfy (fun c -> c <> '\\' && c <> '/')
    let unescape c = match c with
                     | 'n' -> '\n'
                     | 'r' -> '\r'
                     | 't' -> '\t'
                     | c   -> c
    let escapedChar = pstring "\\" >>. (anyOf "\\nrt\"" |>> unescape)
    between (pstring "/") (pstring "/")
            (manyChars (normalChar <|> escapedChar))

// Similar to pnumber below.
// https://www.quanttec.com/fparsec/reference/charparsers.html

let numberFormat =     NumberLiteralOptions.AllowMinusSign
                   ||| NumberLiteralOptions.AllowFraction
                   ||| NumberLiteralOptions.AllowExponent
                   ||| NumberLiteralOptions.AllowHexadecimal
                   ||| NumberLiteralOptions.AllowSuffix

let pnumber : Parser<Rexpr, unit> =
    let parser = numberLiteral numberFormat "number"
    fun stream ->
        let reply = parser stream
        if reply.Status = Ok then
            let nl = reply.Result // the parsed NumberLiteral
            if nl.SuffixLength = 0
               || (   nl.IsInteger
                   && nl.SuffixLength = 1 && nl.SuffixChar1 = 'L')
            then
                try
                    let result = if nl.IsInteger then
                                    Atom (Number (int32 nl.String))
                                 else
                                     if nl.IsHexadecimal then
                                         Atom (Float (floatOfHexString nl.String))
                                     else
                                         Atom (Float (float nl.String))
                    Reply(result)
                with
                | :? System.OverflowException as e ->
                    stream.Skip(-nl.String.Length)
                    Reply(FatalError, messageError e.Message)
            else
                stream.Skip(-nl.SuffixLength)
                Reply(Error, messageError "invalid number suffix")
        else // reconstruct error reply
            Reply(reply.Status, reply.Error)

let pterm =
    (pstringLiteral |>> (fun x -> Atom (String x)))
    <|>(pregexLiteral |>> (fun x -> Atom (String x)))
    <|> pnumber
    <|>(pidentifier |>> (fun x -> Atom (Variable x)))

let pterm_ws = pterm .>> ws


let opp = new OperatorPrecedenceParser<Rexpr,unit,unit>()
let pexpr = opp.ExpressionParser

let pargs = 
     between (pstring "(") (pstring ")")
        (sepBy pexpr (str_ws ","))


opp.TermParser <- (attempt (pipe2 pidentifier pargs (fun funid args -> Funcall (funid, args) ) ))
                 <|> pterm_ws
                 <|> between (str_ws "(") (str_ws ")") pexpr

opp.AddOperator(InfixOperator("==", ws, 1, Associativity.Left, fun x y -> BinOp (EqOp, x, y)))
opp.AddOperator(InfixOperator("!=", ws, 1, Associativity.Left, fun x y -> BinOp (NeqOp, x, y)))


let pAssignment =
    pipe2 (pidentifier .>> (str_ws "=")) pexpr (fun ident expr -> {identifier=ident; rexpr=expr}) 


let pvarlist = 
    (sepBy pidentifier (str_ws ","))
