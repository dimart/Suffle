module Parser.Pattern

open FParsec
open Suffle.Specification.Types
open Suffle.Specification.Syntax
open Parser.Auxiliary
open Parser.Literals

open Specification.Libs
open Specification.Sugar

let pIdent stream = 
    ident |>> (fun n -> PIdent{ Name = n })
    <| stream

let pLiteral stream = 
    literals |>> (fun v -> PLiteral{ Value = v })
    <| stream

let pWildcard stream =
    pstring sWildcard >>% PWildcard
    <| stream

let basicpp stream = 
    any [pWildcard; pLiteral; pIdent]
    <| stream

let rec pCtor stream =
    let bp = skipws_before (inbrackets pattern) <??> "pCtor, inbrackets pattern"
    let basic = (skipws_before1 basicpp) <??> "pCtor, basic"
    let ctor_arg = attempt basic <|> bp
    ctor .>>. many ctor_arg |>> PCtor
    <| stream 
                
and pListCons stream =
    let head = ws_after <| basicpp <|> inbrackets pattern
    let cons = ws_after <| pstring Lists.consOp
    let plc = head .>> cons .>>. pattern |>> (fun (h, t) -> PCtor(Lists.consName, h::[t]))
    plc stream               

and pListEmpty stream = 
    let pnil = pstring <| Lists.openBracket + Lists.closeBracket
    pnil >>% PCtor(Lists.emptName, []) 
    <| stream

and pList stream =
    let elem = ws_after pattern
    let right_rec xs = List.foldBack (fun x acc -> PCtor(Lists.consName, [x; acc])) xs (PCtor(Lists.emptName, []))
    let elems = many ((ws_after <| pstring Lists.separator) >>. elem)
    between 
        (ws_after <| pstring Lists.openBracket) 
        (pstring Lists.closeBracket) 
        (elem .>>. elems |>> (fun (x, xs) -> PCtor(Lists.consName, [x; right_rec xs])))
    <| stream

and pattern s =
    let p = any [attempt pListEmpty; attempt pListCons; attempt pList; basicpp; pCtor ]
    inbrackets pattern <|> p
    <| s


