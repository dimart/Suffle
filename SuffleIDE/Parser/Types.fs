module Parser.Types

open ParserCombinators.Core
open Specification.Types
open Parser.Auxiliary

let tUnit : Parser<Type> = 
    pstr "unit" >>% TUnit

let tBool : Parser<Type> = 
    pstr "bool" >>% TBool

let tChar : Parser<Type> = 
    pstr "char" >>% TChar

let tInt  : Parser<Type> = 
    pstr "int"  >>% TInt

let tVar : Parser<Type> = 
    pvartype |>> TVar

let rec tDatatype pi =
    let c = 
        parse {
            let! c' = ctor
            return TDatatype(c', [])
        }
    let cp =
        parse {
            let! c' = ctor
            let! ptypes = many1 (skipws_and_comments1 tType)
            return TDatatype(c', ptypes)
        }
    c <|> inbrackets cp <| pi

and basicType : Parser<Type> = 
    any [tUnit; tBool; tChar; tInt; tDatatype; tVar]

and tLambda pi = 
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