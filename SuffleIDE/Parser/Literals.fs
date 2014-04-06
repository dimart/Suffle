module Parser.Literals

open ParserCombinators.Core
open Specification.Types
open Specification.Syntax

let lUnit : Parser<Value> = 
    pstr sUnit >>% VUnit

let lBool : Parser<Value> = 
    (pstr sTrue >>% VBool true) <|> (pstr sFalse >>% VBool false)

let lChar : Parser<Value> = 
    between (sym '\'') (symf (fun _ -> true)) (sym '\'') |>> VChar

let lInt  : Parser<Value> = 
    pint |>> VInt

let literals : Parser<Value> = 
    any [lUnit; lBool; lChar; lInt]