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

and internal _elambda pi =
    parse {
        let! _ = pstr sLambda
        let! arg = ident
        let! _ = between pws (pstr sArrow) pws
        let! body = expression
        return { ELambda.Arg = {EIdent.Name = arg}; Body = body }
    } <| pi

and eLambda : Parser<Expression> =
    _elambda |>> ELambda
        
and eFunApp pi =
    parse {
        let! f = eIdent <|> inbrackets expression
        let! args = many1 (mws1 arg)
        let fa f a = EFunApp{ Func = f; Arg = a }
        return 
            List.fold (fun f' arg -> fa f' arg) 
                      (fa f <| List.head args) 
                      (List.tail args)
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
    parse {
        let! _ = pstr "let"
        let! bind = mws1 declaration
        let! _ = mws1 <| pstr "in"
        let! body = mws1 expression
        let! _ = mws1 <| pstr sEndKeyword
        return ELetIn{ Binding = bind; Body = body }
    } <| pi

and eCaseOf pi =
    let patternLine =
        parse {
            let! _ = mws1 <| pstr sPipe
            let! p = pattern
            let! _ = between pws (pstr sArrow) pws
            let! e = expression
            return (p, e)
        } 
    parse {
        let! _ = pstr "case"
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
    e <|> inbrackets expression <| pi


and dValue pi =
    parse {
        let! _ = pstr sValKeyword
        let! name = mws1 _eident
        let! _ = between pws (pstr sBinding) pws
        let! value = expression
        let! _ = skipws <| pstr sLineEnding
        return DValue{ Name = name; Value = value }
    } <| pi

and dFunction pi =
    parse {
        let! _ = pstr sFunKeyword
        let! f = mws1 _eident
        let! x = mws1 _eident
        let! _ = between pws (pstr sBinding) pws
        let! body = expression
        let! _ = skipws <| pstr sLineEnding 
        return DFunction{ Name = f; Arg = x; Body = body }
    } <| pi  

and dDatatype =
    let oft = 
        parse {
            let! _ = mws1 <| pstr "of"
            let! t = tType
            return t
        }
    let constr =
        parse {
            let! _ = mws1 <| pstr sPipe
            let! c = ctor
            let! t = opt oft
            return (c, t)
        }
    parse {
        let! _ = pstr "datatype"
        let! name = mws1 ctor
        let! _ = between pws (pstr sBinding) pws
        let! clist = many1 constr
        let! _ = mws1 <| pstr sEndKeyword
        return DDatatype{ Name = {EIdent.Name = name}; Ctors = clist }
    }

and declaration : Parser<Declaration> =
    any [dValue; dFunction; dDatatype]


let TypedExpression = ()
    // requires type parser
    // TODO