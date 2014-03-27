module Parser.Literals

open ParserCombinators.Core
open Types

let lUnit : Parser<Value> = 
    pstr "()" >>% VUnit

let lBool : Parser<Value> = 
    (pstr "true" >>% VBool true) <|> (pstr "false" >>% VBool false)

let lChar : Parser<Value> = 
    between (sym '\'') (symf (fun _ -> true)) (sym '\'') |>> VChar

let lInt  : Parser<Value> = 
    pint |>> VInt

let literals : Parser<Value> = 
    any [lUnit; lBool; lChar; lInt]