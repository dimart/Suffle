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
    let pp = skipws_and_comments1 pattern <|> skipws_and_comments (inbrackets pattern)
    parse {
        let! c = ctor 
        let! ps = many (pp <|> (inbrackets pp))
        return PCtor(c, ps)
    } <| pi

and pattern pi =
    let p = any [pWildcard; pLiteral; pIdentifier; pCtor]
    skipws_and_comments (p <|> inbrackets pattern) <| pi