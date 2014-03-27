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
    sym '_' >>% Wildcard

let rec pConstructor : Parser<Pattern> =
    parse {
        let! c = ctor 
        let! p = pattern
        return PConstructor(c, p)
    }

and pattern : Parser<Pattern> =
    any [pIdentifier; pLiteral; pWildcard; pConstructor]