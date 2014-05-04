module Parser.Literals

open FParsec
open Suffle.Specification.Types
open Suffle.Specification.Syntax
open Parser.Auxiliary

let lUnit stream = 
    pstring sUnit >>% VUnit
    <| stream 

let lBool stream = 
    (pstring sTrue >>% VBool true) <|> (pstring sFalse >>% VBool false)
    <| stream

let lChar stream = 
    between pquote pquote anyChar |>> VChar
    <| stream

let lInt stream = 
    pint32 |>> VInt
    <| stream

let literals stream = 
    choice [lUnit; lBool; lChar; lInt]
    <| stream