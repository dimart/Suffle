module Parser.Pattern

open ParserCombinators.Core
open Types
open Parser.Auxiliary
open Parser.Literals

let pIdentifier : Parser<Pattern> = 
    parse {
        let! name = ident
        return PIdentifier{ Name = name }
    }

let pLiteral : Parser<Pattern> =
    parse {
        let! value = literals
        return PLiteral{ Value = value }
    }

let pWildcard : Parser<Pattern> =
    sym '_' >>% PWildcard

let rec pCtor : Parser<Pattern> =
    parse {
        let! c = ctor 
        let! p = mws1 pattern <|> inbrackets pattern
        return PCtor(c, p)
    }

and pattern pi =
    let p = any [pWildcard; pLiteral; pIdentifier; pCtor]
    p <|> inbrackets pattern <| pi