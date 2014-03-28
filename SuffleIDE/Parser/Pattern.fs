module Parser.Pattern

open ParserCombinators.Core
open Specification.Types
open Specification.Syntax
open Parser.Auxiliary
open Parser.Literals

let pIdentifier : Parser<Pattern> = 
    parse {
        let! name = ident
        return PIdent{ Name = name }
    }

let pLiteral : Parser<Pattern> =
    parse {
        let! value = literals
        return PLiteral{ Value = value }
    }

let pWildcard : Parser<Pattern> =
    pstr sWildcard >>% PWildcard

let rec pCtor pi =
    parse {
        let! c = ctor 
        let! p = mws1 pattern <|> inbrackets pattern
        return PCtor(c, p)
    } <| pi

and pattern pi =
    let p = any [pWildcard; pLiteral; pIdentifier; pCtor]
    p <|> inbrackets pattern <| pi