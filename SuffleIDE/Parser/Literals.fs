module Parser.Literals

open FParsec
open Suffle.Specification.Types
open Suffle.Specification.Syntax
open Parser.Auxiliary

let internal lUnit stream = 
    pstr sUnit >>% VUnit
    <?> "unit literal - ()" 
    <| stream 

let internal lBool stream = 
    (pstr sTrue >>% VBool true) <|> (pstr sFalse >>% VBool false)
    <?> "true or false"
    <| stream

let internal lChar stream = 
    between pquote pquote anyChar |>> VChar
    <?> "char literal"
    <| stream

let internal lInt stream = 
    ws_ pint32 |>> VInt
    <?> "int"
    <| stream

let literal stream = 
    choice [lUnit; lBool; lChar; lInt]
    <| stream