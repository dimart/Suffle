module Parser.Types

open ParserCombinators.Core
open Types
open Parser.Auxiliary

let tUnit : Parser<Type> = 
    pstr "unit" >>% TUnit

let tBool : Parser<Type> = 
    pstr "bool" >>% TBool

let tChar : Parser<Type> = 
    pstr "char" >>% TChar

let tInt  : Parser<Type> = 
    pstr "int"  >>% TInt

let tDatatype : Parser<Type> = 
    ctor |>> TDatatype

let tVar : Parser<Type> = 
    sym '\'' >|>> many1 (syms <| ['a'..'z'] @ ['A'..'Z']) |>> (TVar << chars2str)

let basicType : Parser<Type> = 
    any [tUnit; tBool; tChar; tInt; tDatatype; tVar]

let rec tLambda pi = 
    let tl =
        parse {
            let! a = basicType <|> inbrackets (tType)
            let! _ = between pws (pstr "->") pws
            let! b = tType
            return TLambda(a, b)
        }
    tl <|> inbrackets tLambda <| pi

and tType pi =
    let t = basicType <|> tLambda
    t <|> inbrackets tType <| pi