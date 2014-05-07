module Parser.Structures

open ParserCombinators.Core
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

let internal _eident =
    parse {
        let! name = ident
        return { EIdent.Name = name }
    }

let pLineEnding = skipws_and_comments (pstr sLineEnding) <|> eol

let eIdent : Parser<Expression> = 
    _eident |>> EIdent

let eLiteral : Parser<Expression> =
    parse {
        let! value = literals
        return ELiteral{ Value = value }
    }

let primary : Parser<Expression> = 
    any [eLiteral; eIdent]

let rec arg pi =
    any [primary; eListEmpty; eList; inbrackets expression] <| pi

and eUnary pi = 
    parse {
        let! op = unaries
        let! e = skipws_and_comments arg
        return EUnary{ Op = op; Arg = e }
    } <| pi

and eBinary pi = 

    let mkbin b arg1 arg2 =
        EBinary{ Op = b; Arg1 = arg1; Arg2 = arg2 }

    let leftAssos p op =
        let op_arg = skipws_and_comments op .>>. skipws_and_comments p
        parse {
            let! x = p
            let! xs = many op_arg
            return Seq.fold (fun acc (binOp, y) -> mkbin binOp acc y) x xs
        }

    let rec mkbinp =
        function
        | [] -> arg <|> eFunApp
        | b :: bs -> leftAssos (mkbinp bs) b
            
    mkbinp binPrioritised <| pi

and eLambda pi =
    let ld x b = ELambda{ Arg = x; Body = b }
    let mId x = { EIdent.Name = x }
    parse {
        let! _ = pstr sLambda
        let! arg0 = skipws_and_comments ident
        let! args' = many <| skipws_and_comments1 ident
        let args = List.rev (arg0 :: args')
        let! _ = between pws_and_comments (pstr sArrow) pws_and_comments
        let! body = expression
        return
            List.fold (fun body lname -> ld (mId lname) body)
                      (ld (mId args.Head) body)
                      args.Tail
    } <| pi
        
and eFunApp pi =
    let fa f a = EFunApp{ Func = f; Arg = a }
    parse {
        let! f = eIdent <|> inbrackets expression
        let! arg0 = skipws_and_comments1 arg
        let! args = many (skipws_and_comments1 arg)
        return 
            List.fold (fun f' a' -> fa f' a') 
                      (fa f <| arg0) 
                      args
    } <| pi

and eIfElse pi =
    parse {
        let! _ = pstr "if"
        let! cond = skipws_and_comments1 expression
        let! _ = skipws_and_comments1 <| pstr "then"
        let! onTrue = skipws_and_comments1 expression
        let! _ = skipws_and_comments1 <| pstr "else"
        let! onFalse = skipws_and_comments1 expression
        let! _ = skipws_and_comments1 <| pstr sEndKeyword
        return EIfElse{ Cond = cond; OnTrue = onTrue; OnFalse = onFalse }
    } <| pi
    
and eLetIn pi = 
    let mli b e = ELetIn{ Binding = b; Body = e}
    parse {
        let! _ = pstr "let"
        let! binds' = many1 <| skipws_and_comments1 declaration
        let binds = List.rev binds'
        let! _ = skipws_and_comments1 <| pstr "in"
        let! body = skipws_and_comments1 expression
        let! _ = skipws_and_comments1 <| pstr sEndKeyword
        return 
            List.fold (fun expr b -> mli b expr)
                      (mli (binds.Head) body)
                      binds.Tail
    } <| pi

and eCaseOf pi =
    let patternLine =
        parse {
            let! _ = skipws_and_comments <| pstr sPipe
            let! p = skipws_and_comments pattern
            let! _ = between pws_and_comments (pstr sArrow) pws_and_comments
            let! e = expression
            return (p, e)
        } 
    parse {
        let! _ = skipws_and_comments1 <| pstr "case"
        let! sample = skipws_and_comments1 expression
        let! _ = skipws_and_comments1 <| pstr "of"
        let! plist = many1 patternLine
        let! _ = skipws_and_comments1 <| pstr sEndKeyword
        return ECaseOf{ Matching = sample; Patterns = plist }
    } <| pi

and eListCons pi =
    let fa f a = EFunApp{ Func = f; Arg = a }
    let mkIde name = EIdent{ Name = name }
    let left = primary <|> eFunApp <|> inbrackets expression
    parse {
        let! x = skipws_and_comments <| left
        let! _ = skipws_and_comments <| pstr Lists.consOp
        let! xs = skipws_and_comments expression
        return fa (fa (mkIde Lists.consName) x) xs
    } <| pi

and eListEmpty pi = 
    skipws_and_comments (pstr <| Lists.openBracket + Lists.closeBracket) 
        >>% EIdent{ Name = Lists.emptName } <| pi

and eList pi =
    let fa f a = EFunApp{ Func = f; Arg = a }
    let mkIde name = EIdent{ Name = name }
    let elem = skipws_and_comments expression
    parse {
        let! _ = skipws_and_comments <| pstr Lists.openBracket
        let! x = elem
        let! xs = many ((skipws_and_comments <| pstr Lists.separator) >>. elem)
        let! _ = skipws_and_comments <| pstr Lists.closeBracket
        let ys = List.foldBack (fun x acc -> fa (fa (mkIde Lists.consName) x) acc) xs (mkIde Lists.emptName)
        return fa (fa (mkIde Lists.consName) x) ys
    } <| pi

and expression pi = 
    let e = any [eLiteral; eIdent; 
                 eIfElse; eLetIn; eCaseOf; 
                 eUnary; eBinary;
                 eLambda; eFunApp;
                 eListCons; eListEmpty; eList]
    skipws_and_comments (e <|> inbrackets expression) <| pi


and dValue pi =
    parse {
        let! _ = skipws_and_comments <| pstr "def"
        let! _ = skipws_and_comments1 <| pstr sValKeyword
        let! _ = skipws_and_comments <| pstr "::"
        let! t = skipws_and_comments <| tType
        let! _ = pLineEnding
        let! name = skipws_and_comments _eident
        let! _ = between pws_and_comments (pstr sBinding) pws_and_comments
        let! value = expression
        let! _ = skipws_and_comments pLineEnding
        return DValue{ Type = t; Name = name; Value = value }
    } <| pi

and dFunction pi =
    let ld x b = ELambda{ Arg = x; Body = b }
    let mId x = { EIdent.Name = x }
    parse {
        let! _ = skipws_and_comments <| pstr "def"
        let! _ = skipws_and_comments1 <| pstr sFunKeyword
        let! _ = skipws_and_comments <| pstr "::"
        let! t = skipws_and_comments <| tType
        let! _ = pLineEnding
        let! name = skipws_and_comments _eident
        let! x = skipws_and_comments1 _eident
        let! args = many (skipws_and_comments1 _eident)
        let! _ = between pws_and_comments (pstr sBinding) pws_and_comments
        let! body = expression
        let! _ = pLineEnding
        let body' = 
            List.fold (fun acc x -> ld x acc)
                      body
                      args
        return 
            DFunction{ Type = t
                       Name = name
                       Arg = x 
                       Body = body' 
            }
    } <| pi  

and dDatatype =
    let constr =
        parse {
            let! _ = between pws_and_comments (pstr sPipe) pws_and_comments
            let! c = skipws_and_comments ctor
            let! ts = many <| skipws_and_comments1 tType
            return (c, ts)
        }
    parse {
        let! _ = pstr sDatatype
        let! name = skipws_and_comments1 ctor
        let! ptypes = many (skipws_and_comments1 pvartype)
        let! _ = between pws_and_comments (pstr sBinding) pws_and_comments
        let! clist = many1 constr
        let! _ = skipws_and_comments1 <| pstr sEndKeyword
        return DDatatype{ Name = {EIdent.Name = name}
                          Params = ptypes
                          Ctors = clist
                        }
    }

and declaration : Parser<Declaration> =
    skipws_and_comments <| any [dValue; dFunction; dDatatype]

let program : Parser<Declaration list> = between pws_and_comments (many declaration) pws_and_comments