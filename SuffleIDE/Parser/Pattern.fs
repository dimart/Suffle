module Parser.Pattern

open ParserCombinators.Core
open Suffle.Specification.Types
open Suffle.Specification.Syntax
open Parser.Auxiliary
open Parser.Literals

open Specification.Libs
open Specification.Sugar

let pIdent : Parser<Pattern> = 
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

and basicpp = any [pWildcard; pLiteral; pIdent; pCtor]

and pListCons pi =
    let left = basicpp <|> inbrackets pattern
    parse {
        let! x = left
        let! _ = skipws_and_comments <| pstr Lists.consOp
        let! xs = skipws_and_comments pattern
        return PCtor(Lists.consName, [x; xs])
    } <| pi

and pListEmpty pi = pstr <| Lists.openBracket + Lists.closeBracket >>% PCtor(Lists.emptName, []) <| pi

and pList pi =

    let elem = skipws_and_comments pattern

    parse {
        let! _ = pstr Lists.openBracket
        let! x = elem
        let! xs = many ((skipws_and_comments <| pstr Lists.separator) >>. elem)
        let! _ = skipws_and_comments <| pstr Lists.closeBracket
        let ys = List.foldBack (fun x acc -> PCtor(Lists.consName, [x; acc])) xs (PCtor(Lists.emptName, []))
        return PCtor(Lists.consName, [x; ys])
    } <| pi

and pattern pi =
    let p = any [basicpp; pListCons; pListEmpty; pList]
    skipws_and_comments (p <|> inbrackets pattern) <| pi


