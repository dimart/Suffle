﻿module Parser.Pattern

open FParsec
open Suffle.Specification.Types
open Suffle.Specification.Syntax
open Parser.Auxiliary
open Parser.Literals
open Suffle.Specification.Libs
open Suffle.Specification.Sugar

let internal pIdent stream = 
    ident |>> (fun n -> PIdent{ Name = n })
    <?> "identifier"
    <| stream

let internal pLiteral stream = 
    literal |>> (fun v -> PLiteral{ Value = v })
    <?> "literal"
    <| stream

let internal pWildcard stream =
    pstr sWildcard >>% PWildcard
    <?> "wildcard pattern"
    <| stream

let rec basicpp stream = 
    choice <| List.map attempt [pListEmpty; pList; pLiteral; pIdent; pWildcard]
    <| stream

and pCtor stream =
    let ctor_arg = attempt basicpp <|> inbrackets pattern
    ctor .>>. many ctor_arg |>> PCtor
    <?> "constructor"
    <| stream 
                
and internal pListCons stream =
    let head = basicpp <|> inbrackets pattern
    let cons = pstr Lists.consOp
    head .>> cons .>>. pattern |>> (fun (h, t) -> PCtor(Lists.consName, h::[t]))
    <?> "list contructor (:)"
    <| stream               

and internal pListEmpty stream = 
    let pnil = pstr <| Lists.openBracket + Lists.closeBracket
    pnil >>% PCtor(Lists.emptName, [])
    <?> "empty list" 
    <| stream

and internal pList stream =
    let right_rec xs = List.foldBack (fun x acc -> PCtor(Lists.consName, [x; acc])) xs (PCtor(Lists.emptName, []))
    let patterns = many (pstr Lists.separator >>. pattern)
    between 
        (pstr Lists.openBracket) 
        (pstr Lists.closeBracket) 
        (pattern .>>. patterns |>> (fun (x, xs) -> PCtor(Lists.consName, [x; right_rec xs])))
    <?> "list"
    <| stream

and pattern s =
    let p = choice [attempt pListCons; basicpp; pCtor]
    attempt p <|> inbrackets pattern
    <| s


