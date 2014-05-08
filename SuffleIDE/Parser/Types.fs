module Parser.Types

open FParsec
open Suffle.Specification.Types
open Parser.Auxiliary

let tUnit stream = 
    pstring "unit" >>% TUnit
    <??> "unit type"
    <| stream

let tBool stream = 
    pstring "bool" >>% TBool
    <??> "bool type"
    <| stream

let tChar stream = 
    pstring "char" >>% TChar
    <??> "char type"  
    <| stream

let tInt  stream = 
    pstring "int"  >>% TInt
    <??> "int type" 
    <| stream

let tVar stream = 
    pvartype |>> TVar 
    <??> "variable type"     
    <| stream

let rec tDatatype stream =
    ws_ ctor |>> (fun c -> TDatatype(c, []))
    <??> "datatype name" 
    <| stream

and tDatatypeGeneric s =
    ws_ ctor .>>. (many1 (ws_ tType)) |>> TDatatype
    <??> "constructor with arguments"
    <| s

and basicType stream = 
    choice [tUnit; tBool; tChar; tInt; tVar; tDatatype]
    <??> "basic type"
    <| stream

and tLambda stream =
    let left = ws_ (inbrackets tType <|> basicType <|> tDatatype)
    let arrow = ws_ (pstring "->")
    let right = ws_ tType
    let tl = left .>> arrow .>>. right |>> TLambda
    //attempt tl <|> inbrackets tLambda
    tl
    <??> "lambda type"
    <| stream

and tType stream =
    choice [attempt tLambda; inbrackets tType; attempt tDatatypeGeneric; basicType]
    <??> "type declaration"
    <| stream