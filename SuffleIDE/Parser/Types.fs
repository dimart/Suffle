module Parser.Types

open FParsec
open Suffle.Specification.Types
open Parser.Auxiliary

let tUnit stream = 
    pstring "unit" >>% TUnit
    <| stream

let tBool stream = 
    pstring "bool" >>% TBool
    <| stream

let tChar stream = 
    pstring "char" >>% TChar  
    <| stream

let tInt  stream = 
    pstring "int"  >>% TInt 
    <| stream

let tVar stream = 
    pvartype |>> TVar      
    <| stream

let rec tDatatype stream =
    let c = ctor |>> (fun c -> TDatatype(c, []))
    let cp = skipws_after ctor .>>. (many1 (skipws_after tType)) |>> TDatatype
    inbrackets cp <|> c
    <| stream

and basicType stream = 
    choice [attempt tDatatype; tUnit; tBool; tChar; tInt; tVar]
    <| stream

and tLambda stream =
    let left = skipws_after (attempt basicType <|> inbrackets tType)
    let arrow = skipws_after <| pstring "->"
    let right = skipws_after tType
    let tl = left .>> arrow .>>. right |>> TLambda
    attempt tl <|> (inbrackets tLambda)
    <| stream

and tType stream =
    attempt tLambda <|> attempt (inbrackets tType) <|> basicType
    <| stream