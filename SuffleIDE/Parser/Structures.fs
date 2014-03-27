module Parser.Expression

open ParserCombinators.Core
open Types
open Parser.Auxiliary
open Parser.Literals
open Parser.Unary
open Parser.Binary
open Parser.Pattern

let internal _eident =
    parse {
        let! name = ident
        return { EIdentifier.Name = name }
    }

let eIdentifier : Parser<Expression> = 
    _eident |>> EIdentifier

let eLiteral : Parser<Expression> =
    parse {
        let! value = literals
        return ELiteral{ Value = value }
    }

let rec eUnary : Parser<Expression> = 
    parse {
        let! op = unaries
        let! e = skipws expression
        return EUnary{ Operation = op; Arg = e }
    }

and eBinary : Parser<Expression> = 
    parse {
        let! arg1 = expression
        let! binop = skipws binaries
        let! arg2 = skipws expression
        return EBinary{ Operation = binop; Arg1 = arg1; Arg2 = arg2 }
    }

and eCtorApp : Parser<Expression> =
    parse {
        let! c = ctor
        let! e = mws1 expression
        return EConstrApplying{ ConstrName = c; Value = e }
    }

and internal _elambda =
    parse {
        let! _ = sym '\\'
        let! arg = ident
        let! _ = between pws (pstr "->") pws
        let! body = expression
        return { ELambda.Arg = {EIdentifier.Name = arg}; Body = body }
    }

and eLambda : Parser<Expression> =
    _elambda |>> ELambda
        
and eFunApp : Parser<Expression> =
    let func = (_eident |>> Function) <|> (_elambda |>> Lambda) 
    parse {
        let! f = func
        let! e = mws1 expression
        return EFunApplying{ Func = f; Arg = e }
    }

and eIfElse : Parser<Expression> =
    parse {
        let! _ = pstr "if"
        let! cond = mws1 expression
        let! _ = mws1 <| pstr "then"
        let! onTrue = mws1 expression
        let! _ = mws1 <| pstr "else"
        let! onFalse = mws1 expression
        let! _ = mws1 <| pstr "end"
        return EIfElse{ Condition = cond; OnTrue = onTrue; OnFalse = onFalse }
    }
    
and eLetIn : Parser<Expression> = 
    parse {
        let! _ = pstr "let"
        let! bind = mws1 declaration
        let! _ = mws1 <| pstr "in"
        let! body = mws1 expression
        let! _ = mws1 <| pstr "end"
        return ELetIn{ Binding = bind; Body = body }
    }

and eCaseOf : Parser<Expression> =
    let patternLine =
        parse {
            let! _ = mws1 <| pstr "|"
            let! p = pattern
            let! _ = between pws (pstr "->") pws
            let! e = expression
            let! _ = skipws <| pstr ";" 
            return (p, e)
        }
    parse {
        let! _ = pstr "case"
        let! sample = mws1 expression
        let! _ = mws1 <| pstr "of"
        let! plist = many1 patternLine
        return ECaseOf{ Matching = sample; Patterns = plist }
    }

and expression = 
    let e = any [eIdentifier; eLiteral; eIfElse; eLetIn; eUnary; eBinary;
                 eLambda; eFunApp; eCtorApp; eCaseOf]
    e <|> inbrackets e


and dValue =
    parse {
        let! _ = pstr "val"
        let! name = mws1 _eident
        let! _ = between pws (pstr "=") pws
        let! value = expression
        return DValue{ Name = name; Value = value }
    }

and dFunction =
    parse {
        let! _ = pstr "fun"
        let! f = mws1 _eident
        let! x = mws1 _eident
        let! _ = between pws (pstr "=") pws
        let! body = expression
        return DFunction{ Name = f; Arg = x; Body = body }
    }        

and dDatatype =
    let oft = 
        parse {
            let! _ = mws1 <| pstr "of"
            let! t = tType
            return t
        }
    let constr =
        parse {
            let! _ = mws1 <| pstr "|"
            let! c = ctor
            let! t = opt oft
            let! _ = skipws <| pstr ";" 
            return (c, t)
        }
    parse {
        let! _ = pstr "datatype"
        let! name = mws1 ctor
        let! _ = between pws (pstr "=") pws
        let! clist = many1 constr
        return DDatatype{ Name = {EIdentifier.Name = name}; Constrs = clist }
    }

and declaration : Parser<Declaration> =
    any [dValue; dFunction; dDatatype]


let TypedExpression = ()
    // requires type parser
    // TODO