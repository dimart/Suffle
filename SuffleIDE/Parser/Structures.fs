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

open Specification.Libs
open Specification.Sugar

let internal _eident stream =
    ident |>> (fun name -> { EIdent.Name = name })
    <| stream
            
let pSepString s = _ws1 <| pstring s
let pWsString s = _ws <| pstring s
let pWsAroundString s = _ws_ <| pstring s 
let pLineEnding = (pstring >> ws_ >> optional) sLineEnding
let pEndKeyword = (pstring >> ws_) sEndKeyword

let eIdent stream = 
    let pkw = choice <| List.map (pstring >> attempt) keywords
    let pid = _eident |>> EIdent
    (attempt pkw >>. fail "Keyword cannot be an identifier") <|> pid
    <| stream

let eLiteral stream =
    literals |>> (fun v -> ELiteral{ Value = v })
    <| stream

let primary stream = 
    choice [attempt eLiteral; eIdent]
    <| stream

let rec arg stream =
    choice [primary; attempt eListEmpty; eList <!> "arg as list"; inbrackets expression <!> "arg as expr"]
    <!> "arg" 
    <| stream

and eUnary stream = 
    ws_ unaries .>>. arg  |>> (fun (u, e) -> EUnary{ Op = u; Arg = e })
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
    <!> "binary operation"
    <| stream

and eLambda stream =
    let ld x b = ELambda{ Arg = x; Body = b }
    let mId x = { EIdent.Name = x }
    let kw_lambda = ws_ <| pstring sLambda
    let arrow = _ws_ <| pstring sArrow
    let arg0 = ident
    let args = many (ws1 >>? ident)

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
    <!> "lambda"
    <| stream
        
and eFunApp stream =
    let fa f a = EFunApp{ Func = f; Arg = a }
    let pf = ws_ <| attempt eIdent <|> inbrackets expression <!> "function ident"
    let arg0 = attempt (ws_ arg) <!> "arg0"
    let args = many arg0 <!> "args"
    let pfa = tuple3 pf arg0 args
                |>> (fun (f, x, xs) ->
                        List.fold (fun f' a' -> fa f' a') 
                                    (fa f <| x) 
                                    xs
                    )
    pfa <!> "eFunApp"
    <| stream

and eIfElse stream =
    let p_if = ws_ (pstring "if")                              
    let p_then = ws_ (pstring "then")
    let p_else = ws_ (pstring "else")
    let pIfElse =
        tuple3 (p_if >>. ws_ expression)
               (p_then >>. ws_ expression)
               (between p_else pEndKeyword expression)

    pIfElse
    |>> (fun (cond, onTrue, onFalse) -> EIfElse{ Cond = cond; OnTrue = onTrue; OnFalse = onFalse })
    <!> "if-else"
    <| stream
    
and eLetIn stream = 
    let mli b e = ELetIn{ Binding = b; Body = e}
    let p_let = ws_ <| pstring "let"
    let p_in = ws_ <| pstring "in"
    let p_binds = 
        p_let
        >>. many1 (ws_ declaration)
        |>> List.rev
        <!> "bingings in let"
    let p_body =
        between p_in pEndKeyword expression
        <!> "body in let"

    p_binds .>>. p_body
    |>> (fun (bi, bo) ->
            List.fold (fun expr b -> mli b expr)
                      (mli (bi.Head) bo)
                      bi.Tail
        )
    <!> "let-in"
    <| stream

and eCaseOf stream =
    let p_pipe = ws_ (pstring sPipe) <!> "pipe"
    let arrow = ws_ (pstring sArrow)  <!> "arrow"
    let patternLine =
        p_pipe
        >>. ws_ pattern
        .>> arrow
        .>>. ws_ expression <!> "pattern line"

    let p_case = ws_ <| pstring "case"
    let p_of = ws_ <| pstring "of"
    let plist = many1 patternLine <!> "many pattern lines"

    p_case
    >>. (ws_ expression)
    .>> p_of
    .>>. plist
    .>> (pEndKeyword)
    |>> (fun (sample, plist) ->
            ECaseOf{ Matching = sample; Patterns = plist }
        )
    <!> "case-of"
    <| stream

and eListCons stream =
    let fa f a = EFunApp{ Func = f; Arg = a }
    let mkIde name = EIdent{ Name = name }
    let left = attempt eFunApp <|> (ws_ arg) <!> "cons left"
    let cons = _ws_ <| pstring Lists.consOp    <!> "cons op"
    
    left
    .>> cons
    .>>. ws_ (expression <!> "cons right")
    |>> (fun (x, xs) ->
            fa (fa (mkIde Lists.consName) x) xs
        )
    <!> "list constructor (:)"
    <| stream

and eListEmpty stream =
    let empt = pstring <| Lists.openBracket + Lists.closeBracket 
    empt >>% EIdent{ Name = Lists.emptName } 
    <!> "empty list"
    <| stream

and eList stream =
    let fa f a = EFunApp{ Func = f; Arg = a }
    let mkIde name = EIdent{ Name = name }

    let popen = ws_ <| pstring Lists.openBracket
    let pclose = pstring Lists.closeBracket 
    let psep = ws_ <| pstring Lists.separator

    between popen pclose (ws_ expression .>>. (many (psep >>. ws_ expression)))
    |>> (fun (x, xs) ->
            let ys = List.foldBack (fun x acc -> fa (fa (mkIde Lists.consName) x) acc) xs (mkIde Lists.emptName)
            fa (fa (mkIde Lists.consName) x) ys
        )
    <!> "list"
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
    let p = attempt e <|> inbrackets expression 
    p <!> "expression"
    <| stream
                                                             

    /////////////////////////////////////////////////////////////////////////////////////////////////////////////

and dValue stream =
    let p_def = pWsString "def"
    let p_val = pSepString sValKeyword
    let p_typeOp = pWsAroundString "::"
    let p_name = _ws _eident
    let p_binding = pWsAroundString sBinding <!> " (=) "
    let p_typedef = p_def >>. p_val >>. p_typeOp >>. tType .>> pLineEnding <!> "value type"
    let p_value = p_binding >>. ws_ expression .>> pLineEnding

    tuple3 p_typedef p_name p_value
    |>> (fun (t, name, value) ->
            DValue{ Type = t; Name = name; Value = value }
        )
    <!> "value declaration"
    <?> "value declaration"
    <| stream 

and dFunction stream =
    let ld x b = ELambda{ Arg = x; Body = b }
    let mId x = { EIdent.Name = x }

    let p_def = pWsString "def"
    let p_fun = pSepString sFunKeyword
    let p_typeOp = pWsAroundString "::"
    let p_name = ws_ _eident
    let p_binding = pWsAroundString sBinding
    let p_typedef = p_def >>. p_fun >>. p_typeOp >>. tType .>> pLineEnding <!> "function type"
    let p_arg = ws_ _eident
    let p_body = p_binding >>. ws_ expression .>> pLineEnding

    tuple5 p_typedef
           (p_name <!> "function name")
           p_arg
           (many p_arg)
           p_body
    |>> (fun (t, name, x, args, body) ->
             let body' = List.fold (fun acc x -> ld x acc) body args
             DFunction{ Type = t; Name = name; Arg = x; Body = body' }
        )       
    <!> "function declaration"
    <?> "function declaration"
    <| stream  

and dDatatype stream =    
    let pdt = ws_ (pstring sDatatype)
    let p_binding = pWsAroundString sBinding
    let constr =
        let ts = many (ws_ (inbrackets tType <|> basicType))
        let p_pipe = ws_ (pstring sPipe)
        p_pipe >>. ws_ ctor .>>. ts

    let constrs = between p_binding pEndKeyword (many constr)
    
    tuple3 (pdt >>. ws_ ctor)
           (many (ws_ pvartype))
           constrs
    |>> (fun (name, ptypes, clist) ->
             DDatatype{ Name = {EIdent.Name = name}; Params = ptypes; Ctors = clist }
        )      
    <!> "datatype declaration"
    <?> "datatype declaration"
    <| stream

and declaration stream =
    choice [attempt dValue; attempt dFunction; dDatatype]
    <!> "declaration"
    <?> "declaration"
    <| stream

let program stream = 
    _ws_ (many declaration)
    <| stream