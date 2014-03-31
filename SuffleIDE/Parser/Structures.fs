module Parser.Structures

open ParserCombinators.Core
open Specification.Types
open Specification.Syntax
open Parser.Auxiliary
open Parser.Literals
open Parser.Unary
open Parser.Binary
open Parser.Pattern
open Parser.Types

let internal _eident =
    parse {
        let! name = ident
        return { EIdent.Name = name }
    }

let pLineEnding = skipws (pstr sLineEnding) <|> eol

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
    primary <|> inbrackets expression <| pi

and eUnary pi = 
    parse {
        let! op = unaries
        let! e = skipws arg
        return EUnary{ Op = op; Arg = e }
    } <| pi

and eBinary pi = 

    let mkbin b arg1 arg2 =
        EBinary{ Op = b; Arg1 = arg1; Arg2 = arg2 }

    let leftAssos p op =
        let op_arg = skipws op .>>. skipws p
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
        let! arg0 = skipws ident
        let! args' = many <| mws1 ident
        let args = List.rev (arg0 :: args')
        let! _ = between pws (pstr sArrow) pws
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
        let! arg0 = mws1 arg
        let! args = many (mws1 arg)
        return 
            List.fold (fun f' a' -> fa f' a') 
                      (fa f <| arg0) 
                      args
    } <| pi

and eIfElse pi =
    parse {
        let! _ = pstr "if"
        let! cond = mws1 expression
        let! _ = mws1 <| pstr "then"
        let! onTrue = mws1 expression
        let! _ = mws1 <| pstr "else"
        let! onFalse = mws1 expression
        let! _ = mws1 <| pstr sEndKeyword
        return EIfElse{ Cond = cond; OnTrue = onTrue; OnFalse = onFalse }
    } <| pi
    
and eLetIn pi = 
    let mli b e = ELetIn{ Binding = b; Body = e}
    parse {
        let! _ = pstr "let"
        let! binds' = many1 <| mws1 declaration
        let binds = List.rev binds'
        let! _ = mws1 <| pstr "in"
        let! body = mws1 expression
        let! _ = mws1 <| pstr sEndKeyword
        return 
            List.fold (fun expr b -> mli b expr)
                      (mli (binds.Head) body)
                      binds.Tail
    } <| pi

and eCaseOf pi =
    let patternLine =
        parse {
            let! _ = skipws <| pstr sPipe
            let! p = skipws pattern
            let! _ = between pws (pstr sArrow) pws
            let! e = expression
            return (p, e)
        } 
    parse {
        let! _ = mws1 <| pstr "case"
        let! sample = mws1 expression
        let! _ = mws1 <| pstr "of"
        let! plist = many1 patternLine
        let! _ = mws1 <| pstr sEndKeyword
        return ECaseOf{ Matching = sample; Patterns = plist }
    } <| pi

and expression pi = 
    let e = any [eLiteral; eIdent; 
                 eIfElse; eLetIn; eCaseOf; 
                 eUnary; eBinary;
                 eLambda; eFunApp]
    skipws (e <|> inbrackets expression) <| pi


and dValue pi =
    parse {
        let! _ = skipws <| pstr "def"
        let! name = mws1 _eident
        let! _ = skipws <| pstr "::"
        let! t = skipws <| tType
        let! _ = pLineEnding
        let! _ = skipws <| pstr sValKeyword
        let! _ = between pws (pstr sBinding) pws
        let! value = expression
        let! _ = skipws pLineEnding
        return DValue{ Type = t; Name = name; Value = value }
    } <| pi

and dFunction pi =
    let ld x b = ELambda{ Arg = x; Body = b }
    let mId x = { EIdent.Name = x }
    parse {
        let! _ = skipws <| pstr "def"
        let! name = mws1 _eident
        let! _ = skipws <| pstr "::"
        let! t = skipws <| tType
        let! _ = pLineEnding
        let! _ = skipws <| pstr sFunKeyword
        let! x = mws1 _eident
        let! args = many (mws1 _eident)
        let! _ = between pws (pstr sBinding) pws
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
            let! _ = between pws (pstr sPipe) pws
            let! c = skipws ctor
            let! ts = many <| mws1 tType
            return (c, ts)
        }
    parse {
        let! _ = pstr sDatatype
        let! name = mws1 ctor
        let! ptypes = many (mws1 pvartype)
        let! _ = between pws (pstr sBinding) pws
        let! clist = many1 constr
        let! _ = mws1 <| pstr sEndKeyword
        return DDatatype{ Name = {EIdent.Name = name}
                          Params = ptypes
                          Ctors = clist
                        }
    }

and declaration : Parser<Declaration> =
    skipws <| any [dValue; dFunction; dDatatype]


let TypedExpression = ()
    // requires type parser
    // TODO


let program : Parser<Declaration list> = between pws (many1 declaration) pws