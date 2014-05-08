module Parser.Literals

open FParsec
open Suffle.Specification.Types
open Suffle.Specification.Syntax
open Parser.Auxiliary

let lUnit stream = 
    pstring sUnit >>% VUnit
    <??> "unit literal - ()" 
    <| stream 

let lBool stream = 
    (pstring sTrue >>% VBool true) <|> (pstring sFalse >>% VBool false)
    <??> "true or false"
    <| stream

let lChar stream = 
    between pquote pquote anyChar |>> VChar
    <??> "char literal"
    <| stream

let lInt stream = 
    pint32 |>> VInt
    <??> "int"
    <| stream

let literals stream = 
    choice [lUnit; lBool; lChar; lInt]
    <??> "literals"
    <| stream