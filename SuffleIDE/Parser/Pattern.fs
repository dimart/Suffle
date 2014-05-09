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
    <??> "identifier"
    <| stream

let pLiteral stream = 
    literals |>> (fun v -> PLiteral{ Value = v })
    <??> "literal"
    <| stream

let pWildcard stream =
    pstring sWildcard >>% PWildcard
    <??> "wildcard pattern"
    <| stream

let basicpp stream = 
    choice [pWildcard; pLiteral; pIdent]
    <| stream

let rec pCtor stream =
    let bp = ws_ (inbrackets pattern)
    let basic = (ws_ basicpp) 
    let ctor_arg = attempt basic <|> bp
    ws_ ctor .>>. many ctor_arg |>> PCtor
    <??> "constructor"
    <| stream 
                
and pListCons stream =
    let head = ws_ <| basicpp <|> inbrackets pattern
    let cons = ws_ <| pstring Lists.consOp
    let plc = head .>> cons .>>. pattern |>> (fun (h, t) -> PCtor(Lists.consName, h::[t]))
    plc
    <??> "list contructor (:)"
    <| stream               

and pListEmpty stream = 
    let pnil = pstring <| Lists.openBracket + Lists.closeBracket
    pnil >>% PCtor(Lists.emptName, [])
    <??> "empty list" 
    <| stream

and pList stream =
    let elem = ws_ pattern
    let right_rec xs = List.foldBack (fun x acc -> PCtor(Lists.consName, [x; acc])) xs (PCtor(Lists.emptName, []))
    let elems = many ((ws_ <| pstring Lists.separator) >>. elem)
    between 
        (ws_ <| pstring Lists.openBracket) 
        (pstring Lists.closeBracket) 
        (elem .>>. elems |>> (fun (x, xs) -> PCtor(Lists.consName, [x; right_rec xs])))
    <??> "list"
    <| stream

and pattern s =
    let p = choice [attempt pListEmpty; attempt pListCons; attempt pList; basicpp; pCtor]
    inbrackets pattern <|> p
    <??> "pattern"
    <| s


