module Parser.Structures

open FParsec
open Suffle.Specification.Types
open Suffle.Specification.Syntax
open Suffle.Specification.Keywords
open Parser.Auxiliary
open Parser.Literals
open Parser.Unary
open Parser.Binary
open Parser.Pattern
open Parser.Types
open Suffle.Specification.Libs
open Suffle.Specification.Sugar

let internal _eident stream =
    ident |>> (fun name -> { EIdent.Name = name })
    <| stream

let pLineEnding = optional (pstr sLineEnding)
let pEndKeyword = pstr sEndKeyword

let eIdent stream = 
    let pkw = choice <| List.map (pstring >> attempt) keywords
    let pid = _eident |>> EIdent
    (attempt pkw >>. fail "Keyword cannot be an identifier") <|> pid
    <?> "identifier"
    <| stream

let eLiteral stream =
    literal |>> (fun v -> ELiteral{ Value = v })
    <?> "literal"
    <| stream

let primary stream = 
    choice [attempt eLiteral; eIdent]
    <| stream

let rec arg stream =
    choice [primary; attempt eListEmpty; eList; inbrackets expression]
    <| stream

and eUnary stream = 
    ws_ unaries .>>. arg  |>> (fun (u, e) -> EUnary{ Op = u; Arg = e })
    <??> "unary operation"
    <| stream

and eBinary stream = 

    let mkbin b arg1 arg2 =
        EBinary{ Op = b; Arg1 = arg1; Arg2 = arg2 }

    let leftAssos p op =
        let op_arg = ws_ op .>>.? ws_ p
        ws_ p .>>. (many op_arg) |>> (fun (x, xs) -> Seq.fold (fun acc (binOp, y) -> mkbin binOp acc y) x xs)

    let rec mkbinp bin_ops =
        match bin_ops with
        | [] -> attempt eFunApp <|> arg
        | b :: bs -> leftAssos (mkbinp bs) b
            
    mkbinp binPrioritised
    <??> "binary operation"
    <| stream

and eLambda stream =
    let ld x b = ELambda{ Arg = x; Body = b }
    let mId x = { EIdent.Name = x }
    let kw_lambda = pstr sLambda
    let arrow = pstr sArrow
    let arg0 = ident
    let args = many ident

    kw_lambda 
    >>. arg0 .>>. args
    |>> (fun (arg, args) -> List.rev <| arg::args)
    .>> arrow
    .>>. expression
    |>> (fun (args, body) -> 
            List.fold (fun body lname -> ld (mId lname) body)
                        (ld (mId args.Head) body)
                        args.Tail
        )
    <??> "lambda expression"
    <| stream
        
and eFunApp stream =
    let fa f a = EFunApp{ Func = f; Arg = a }
    let ct c = EIdent{ Name = c }
    let pf = eIdent <|> (ctor |>> ct) <|> inbrackets expression
    let arg0 = attempt arg
    let args = many arg0
    tuple3 pf arg0 args
                |>> (fun (f, x, xs) ->
                        List.fold (fun f' a' -> fa f' a') 
                                    (fa f <| x) 
                                    xs
                    )
    <??> "function application"
    <| stream

and eIfElse stream =
    let p_if = pstr "if"                              
    let p_then = pstr "then"
    let p_else = pstr "else"            

    tuple3 (p_if >>. expression)
           (p_then >>. expression)
           (between p_else pEndKeyword expression)
    |>> (fun (cond, onTrue, onFalse) -> EIfElse{ Cond = cond; OnTrue = onTrue; OnFalse = onFalse })
    <??> "if-else expression"
    <| stream
    
and eLetIn stream = 
    let mli b e = ELetIn{ Binding = b; Body = e}
    let p_let = pstr "let"
    let p_in = pstr "in"
    let p_binds = 
        p_let
        >>. many1 declaration
        |>> List.rev
    let p_body = between p_in pEndKeyword expression

    p_binds .>>. p_body
    |>> (fun (bi, bo) ->
            List.fold (fun expr b -> mli b expr)
                      (mli (bi.Head) bo)
                      bi.Tail
        )
    <??> "let-in expression"
    <| stream

and eCaseOf stream =
    let p_pipe = pstr sPipe 
    let arrow = pstr sArrow 
    let patternLine =
        p_pipe
        >>. pattern
        .>> arrow
        .>>. expression

    let p_case = pstr "case"
    let p_of = pstr "of"
    let plist = many1 patternLine

    p_case
    >>. expression
    .>> p_of
    .>>. plist
    .>> (pEndKeyword)
    |>> (fun (sample, plist) ->
            ECaseOf{ Matching = sample; Patterns = plist }
        )
    <??> "case-of expression"
    <| stream

and eListCons stream =
    let fa f a = EFunApp{ Func = f; Arg = a }
    let mkIde name = EIdent{ Name = name }
    let left = attempt eFunApp <|> arg 
    let cons = pstr Lists.consOp   
    
    left
    .>> cons
    .>>. expression
    |>> (fun (x, xs) ->
            fa (fa (mkIde Lists.consName) x) xs
        )
    <??> "list constructor (:)"
    <| stream

and eListEmpty stream =
    let empt = Lists.openBracket + Lists.closeBracket 
    pstr empt >>% EIdent{ Name = Lists.emptName } 
    <??> "empty list literal"
    <| stream

and eList stream =
    let fa f a = EFunApp{ Func = f; Arg = a }
    let mkIde name = EIdent{ Name = name }

    let popen = pstr Lists.openBracket
    let pclose = pstring Lists.closeBracket 
    let psep = pstr Lists.separator

    between popen pclose (expression .>>. (many (psep >>. expression)))
    |>> (fun (x, xs) ->
            let ys = List.foldBack (fun x acc -> fa (fa (mkIde Lists.consName) x) acc) xs (mkIde Lists.emptName)
            fa (fa (mkIde Lists.consName) x) ys
        )
    <??> "list expression"
    <| stream

and expression stream = 
    let e = choice [                 
                    attempt eListCons 
                    attempt eBinary
                    attempt eIfElse 
                    attempt eCaseOf  
                    attempt eLetIn   
                    attempt eFunApp
                    attempt eListEmpty
                    attempt eLiteral
                    eList
                    eIdent
                    eLambda
                ]
    attempt e <|> inbrackets expression
    <| stream
                                                             

    /////////////////////////////////////////////////////////////////////////////////////////////////////////////

and dValue stream =          
    let p_val = pstr sValKeyword
    let p_typeOp = pstr sTypeDefSep
    let p_name = _eident
    let p_binding = pstr sBinding
    let p_typedef = p_val >>. p_typeOp >>. tType .>> pLineEnding 
    let p_value = p_binding >>. expression .>> pLineEnding

    tuple3 p_typedef p_name p_value
    |>> (fun (t, name, value) ->
            DValue{ Type = t; Name = name; Value = value }
        )
    <??> "value declaration"
    <| stream 

and dFunction stream =
    let ld x b = ELambda{ Arg = x; Body = b }
    let mId x = { EIdent.Name = x }

    let p_fun = pstr sFunKeyword
    let p_typeOp = pstr sTypeDefSep
    let p_name = _eident
    let p_binding = pstr sBinding
    let p_typedef = p_fun >>. p_typeOp >>. tType .>> pLineEnding
    let p_arg = _eident
    let p_body = p_binding >>. expression .>> pLineEnding

    tuple5 p_typedef
           p_name
           p_arg
           (many p_arg)
           p_body
    |>> (fun (t, name, arg0, args, body) ->
             let body' = List.foldBack (fun x acc -> ld x acc) (arg0::args) body 
             DFunction{ Type = t; Name = name; Body = body' }
        )       
    <??> "function declaration"
    <| stream  

and dDatatype stream =    
    let pdt = pstr sDatatype
    let p_binding = pstr sBinding
    let constr =
        let ts = many (inbrackets tType <|> basicType)
        let p_pipe = pstr sPipe
        p_pipe >>. ctor .>>. ts

    let constrs = between p_binding pEndKeyword (many constr)
    
    tuple3 (pdt >>. ctor)
           (many pvartype)
           constrs
    |>> (fun (name, ptypes, clist) ->
             DDatatype{ Name = {EIdent.Name = name}; Params = ptypes; Ctors = clist }
        )      
    <??> "datatype declaration"
    <| stream

and declaration stream =
    choice [attempt dValue; attempt dFunction; dDatatype]
    <| stream

let declarations = 
    _ws_ (many declaration)