module Parser.Types

open FParsec
open Suffle.Specification.Types
open Suffle.Specification.Syntax
open Parser.Auxiliary

let internal tUnit stream = 
    pstr "unit" >>% TUnit
    <?> "unit"
    <| stream

let internal tBool stream = 
    pstr "bool" >>% TBool
    <?> "bool"
    <| stream

let internal tChar stream = 
    pstr "char" >>% TChar
    <?> "char"  
    <| stream

let internal tInt stream = 
    pstr "int"  >>% TInt
    <?> "int" 
    <| stream

let internal tVar stream = 
    pvartype |>> TVar 
    <?> "variable"     
    <| stream

let rec internal tDatatype stream =
    ctor |>> (fun c -> TDatatype(c, []))
    <?> "datatype name" 
    <| stream

and internal tDatatypeGeneric s =
    ctor .>>. (many1 tType) |>> TDatatype
    <?> "constructor with arguments"
    <| s

and internal basicType stream = 
    choice [tUnit; tBool; tChar; tInt; tVar; tDatatype]
    <| stream

and internal tLambda stream =
    let left = inbrackets tType <|> basicType <|> tDatatype
    let arrow = pstr sArrow
    let right = tType
    left .>> arrow .>>. right |>> TLambda
    <?> "lambda"
    <| stream

and tType stream =
    choice [attempt tLambda; inbrackets tType; attempt tDatatypeGeneric; basicType]
    <| stream