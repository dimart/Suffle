module Parser.Structures

open FParsec
open Suffle.Specification.Types
open Suffle.Specification.Syntax
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
            
let pSepString s = skipws_and_comments1 <| pstring s
let pWsString s = skipws_and_comments <| pstring s
let pWsAroundString s = skipws_around <| pstring s 
let pLineEnding = skipws_and_comments <| (pstring sLineEnding >>% '\n') <|> newline
let pEndKeyword = pstring sEndKeyword

let eIdent stream = 
    _eident |>> EIdent
    <| stream

let eLiteral stream =
    literals |>> (fun v -> ELiteral{ Value = v })
    <| stream

let primary stream = 
    any [eLiteral; eIdent]
    <| stream

let rec arg stream =
    any [primary; eListEmpty; eList; inbrackets expression] 
    <| stream

and pSepExpr stream = skipws_and_comments1 expression <| stream 
and pSepWsDecl stream = skipws_and_comments1 declaration <| stream 
and pSepIdent stream = skipws_and_comments1 ident <| stream
and pWsExpr stream = skipws_and_comments expression <| stream      
and pWsDecl stream = skipws_and_comments declaration <| stream      
and pWsIdent stream = skipws_and_comments ident <| stream 

and eUnary stream = 
    skipws_after unaries .>>. arg  |>> (fun (u, e) -> EUnary{ Op = u; Arg = e })
    <| stream

and eBinary stream = 

    let mkbin b arg1 arg2 =
        EBinary{ Op = b; Arg1 = arg1; Arg2 = arg2 }

    let leftAssos p op =
        let wsp = ws_after p
        let op_arg = ws_after op .>>. wsp
        wsp .>>. (many op_arg) |>> (fun (x, xs) -> Seq.fold (fun acc (binOp, y) -> mkbin binOp acc y) x xs)

    let rec mkbinp bin_ops =
        match bin_ops with
        | [] -> arg <|> eFunApp
        | b :: bs -> leftAssos (mkbinp bs) b
            
    mkbinp binPrioritised <| stream

and eLambda stream =
    let ld x b = ELambda{ Arg = x; Body = b }
    let mId x = { EIdent.Name = x }
    let kw_lambda = skipws_after <| pstring sLambda
    let arrow = ws_after <| pstring sArrow
    let arg0 = ws_after ident
    let args = many (one_space >>. ws_after ident)

    let el =
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

    el stream
        
and eFunApp stream =
    let fa f a = EFunApp{ Func = f; Arg = a }
    let pf = ws_after1 <| eIdent <|> inbrackets expression
    let arg0 = ws_after arg
    let args = many (one_space >>. ws_after1 arg)
    tuple3 pf arg0 args
    |>> (fun (f, x, xs) ->
            List.fold (fun f' a' -> fa f' a') 
                      (fa f <| x) 
                      xs
        )
    <| stream

and eIfElse stream =
    let p_if = ws_after1 <| pstring "if"                              
    let p_then = ws_after1 <| pstring "then"
    let p_else = ws_after1 <| pstring "else"
    let pIfElse =
        tuple3 (p_if >>. ws_after expression)
               (p_then >>. ws_after expression)
               (between p_else pEndKeyword (ws_after1 expression))

    pIfElse
    |>> (fun (cond, onTrue, onFalse) -> EIfElse{ Cond = cond; OnTrue = onTrue; OnFalse = onFalse })
    <| stream
    
and eLetIn stream = 
    let mli b e = ELetIn{ Binding = b; Body = e}
    let p_let = ws_after1 <| pstring "let"
    let p_in = ws_after1 <| pstring "in"
    let p_binds = 
        p_let
        >>. many1 (ws_after1 declaration)
        |>> List.rev
    let p_body =
        between p_in pEndKeyword (ws_after1 expression)

    p_binds .>>. p_body
    |>> (fun (bi, bo) ->
            List.fold (fun expr b -> mli b expr)
                      (mli (bi.Head) bo)
                      bi.Tail
        )
    <| stream

and eCaseOf stream =
    let p_pipe = ws_after <| pstring sPipe
    let arrow = ws_after (pstring sArrow)
    let patternLine =
        p_pipe
        >>. ws_after pattern
        .>> arrow
        .>>. ws_after expression

    let p_case = ws_after1 <| pstring "case"
    let p_of = ws_after1 <| pstring "of"
    let plist = many1 patternLine

    p_case
    >>. (ws_after1 expression)
    .>> p_of
    .>>. plist
    .>> (one_space >>. pEndKeyword)
    |>> (fun (sample, plist) ->
            ECaseOf{ Matching = sample; Patterns = plist }
        )
    <| stream

and eListCons stream =
    let fa f a = EFunApp{ Func = f; Arg = a }
    let mkIde name = EIdent{ Name = name }
    let left = ws_after <| primary <|> eFunApp <|> inbrackets expression
    let cons = ws_after <| pstring Lists.consOp
    
    left
    .>> cons
    .>>. expression
    |>> (fun (x, xs) ->
            fa (fa (mkIde Lists.consName) x) xs
        )
    <| stream

and eListEmpty stream =
    let empt = pstring <| Lists.openBracket + Lists.closeBracket 
    empt >>% EIdent{ Name = Lists.emptName } 
    <| stream

and eList stream =
    let fa f a = EFunApp{ Func = f; Arg = a }
    let mkIde name = EIdent{ Name = name }

    let popen = skipws_after <| pstring Lists.openBracket
    let pclose = pstring Lists.closeBracket 
    let psep = ws_after <| pstring Lists.separator

    between popen pclose (ws_after expression .>>. (many (psep >>. ws_after expression)))
    |>> (fun (x, xs) ->
            let ys = List.foldBack (fun x acc -> fa (fa (mkIde Lists.consName) x) acc) xs (mkIde Lists.emptName)
            fa (fa (mkIde Lists.consName) x) ys
        )
    <| stream

and expression stream = 
    let e = any [eLiteral; eIdent; 
                 eIfElse; eLetIn; eCaseOf; 
                 eUnary; eBinary;
                 eLambda; eFunApp;
                 eListCons; eListEmpty; eList]
    e <|> inbrackets expression 
    <| stream
                                                             

    /////////////////////////////////////////////////////////////////////////////////////////////////////////////

and dValue stream =
    let p_def = pWsString "def"
    let p_val = pSepString sValKeyword
    let p_typeOp = pWsAroundString "::"
    let p_name = skipws_and_comments _eident
    let p_binding = pWsAroundString sBinding
    let p_typedef = p_def >>. p_val >>. p_typeOp >>. tType .>> pLineEnding
    let p_value = between p_binding pLineEnding expression

    tuple3 p_typedef p_name p_value
    |>> (fun (t, name, value) ->
            DValue{ Type = t; Name = name; Value = value }
        )
    <| stream 

and dFunction stream =
    let ld x b = ELambda{ Arg = x; Body = b }
    let mId x = { EIdent.Name = x }

    let p_def = pWsString "def"
    let p_fun = pSepString sFunKeyword
    let p_typeOp = pWsAroundString "::"
    let p_name = skipws_and_comments _eident
    let p_binding = pWsAroundString sBinding
    let p_typedef = p_def >>. p_fun >>. p_typeOp >>. tType .>> pLineEnding
    let p_arg = skipws_and_comments1 _eident
    let p_body = between p_binding pLineEnding expression

    tuple5 p_typedef
           p_name
           p_arg
           (many p_arg)
           p_body
    |>> (fun (t, name, x, args, body) ->
             let body' = List.fold (fun acc x -> ld x acc) body args
             DFunction{ Type = t; Name = name; Arg = x; Body = body' }
        )
    <| stream  

and dDatatype stream =    
    let p_binding = pWsAroundString sBinding
    let constr =
        let ts = many <| skipws_and_comments1 tType
        let p_pipe = pWsAroundString sPipe
        p_pipe >>. ctor .>>. ts

    let constrs = between p_binding pEndKeyword (many constr)
    
    tuple3 (pstring sDatatype >>. skipws_and_comments1 ctor)
           (many (skipws_and_comments1 pvartype))
           constrs
    |>> (fun (name, ptypes, clist) ->
             DDatatype{ Name = {EIdent.Name = name}; Params = ptypes; Ctors = clist }
        )
    <| stream

and declaration stream =
    any [dValue; dFunction; dDatatype]
    <| stream

let program stream = 
    skipws_around (many declaration)
    <| stream