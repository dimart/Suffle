module Parser

open ParserCombinators.Core
open Types

type AuxiliaryParsers() =
  class
    static let alphas = ['a'..'z'] @ ['A'..'Z']
    static let digits = ['0'..'9']
    static let identSymbols = alphas @ ['_'; '\''] @ digits
    static let _wsc = syms [' '; '\t'; '\n'; '\r']
    static let _ws1 = many1 <| _wsc
    /// White space character
    static member wsc = _wsc
    /// At least one whitespace character
    static member ws1 = _ws1
    /// Keyword parser. Keyword must be surrounded by whitespace
    static member kw (s : string) = between _ws1 (pstr s) _ws1
    
    static member ident = syms alphas >|>> many (syms identSymbols) |>> chars2str
    static member constr = syms ['A'..'Z'] >|>> many (syms (alphas @ digits)) |>> chars2str
  end

type LiteralParser() =
  class
    static member Unit = pstr "()" >>% VUnit
    static member Bool = (pstr "true" >>% VBool true) <|> (pstr "false" >>% VBool true)
    static member Char = between (sym '\'') (symf (fun _ -> true)) (sym '\'') |>> VChar
    static member Int  = pint |>> VInt

    static member All = 
        [
            LiteralParser.Unit
            LiteralParser.Bool
            LiteralParser.Char
            LiteralParser.Int
        ]
  end

type TypeParser() =
  class
    static member Unit = pstr "unit" >>% TUnit
    static member Bool = pstr "bool" >>% TBool
    static member Char = pstr "char" >>% TChar
    static member Int  = pstr "int"  >>% TInt
    static member Datatype = AuxiliaryParsers.ident |>> TDatatype
    static member Var = sym '\'' >|>> many1 (syms <| ['a'..'z'] @ ['A'..'Z']) |>> (TVar << chars2str)
    static member Lambda = 
        parse {
            let! a = TypeParser.Type
            let! _ = between pws (pstr "->") pws
            let! b = TypeParser.Type
            return TLambda(a, b)
        }

    static member All =
        [
            TypeParser.Unit
            TypeParser.Bool
            TypeParser.Char
            TypeParser.Int
            TypeParser.Lambda
            TypeParser.Datatype
            TypeParser.Var
        ]

    static member Type = any TypeParser.All
  end

type UnaryOperationsParsers() =
  class
    static member Negation =        sym '-' >>% UNegation
    static member LogicalNegation = sym '!' >>% ULogicalNegation

    static member All = [
                            UnaryOperationsParsers.Negation
                            UnaryOperationsParsers.LogicalNegation
                        ]
  end

type BinaryOperationsParsers() =
  class
    // Arithmetic
    static member Add = pstr "+" >>% BAdd
    static member Sub = pstr "-" >>% BSub
    static member Div = pstr "/" >>% BDiv
    static member Mul = pstr "*" >>% BMul

    static member AllArithmetic =
        [
            BinaryOperationsParsers.Add
            BinaryOperationsParsers.Sub
            BinaryOperationsParsers.Div
            BinaryOperationsParsers.Mul
        ]

    // Logic
    static member And = pstr "&&" >>% BAnd
    static member Or  = pstr "||" >>% BOr
    static member Eq  = pstr "==" >>% BEq
    static member NEq = pstr "<>" >>% BNEq

    static member AllLogic =
        [
            BinaryOperationsParsers.And
            BinaryOperationsParsers.Or
            BinaryOperationsParsers.Eq
            BinaryOperationsParsers.NEq
        ]

    // Auxiliary
    // TODO

    static member All =
        [
            BinaryOperationsParsers.Add
            BinaryOperationsParsers.Sub
            BinaryOperationsParsers.Div
            BinaryOperationsParsers.Mul
            BinaryOperationsParsers.And
            BinaryOperationsParsers.Or
            BinaryOperationsParsers.Eq
            BinaryOperationsParsers.NEq
        ]
  end

type PatternParsers() =
  class
    static member Identifier = 
        parse {
            let! name = AuxiliaryParsers.ident
            return PIdentifier{ Name = name }
        }

    static member Literal =
        let lit = any LiteralParser.All
        parse {
            let! value = lit
            return PLiteral{ Value = value }
        }

    static member Constructor =
        parse {
            let! c = AuxiliaryParsers.constr 
            let! p = PatternParsers.Pattern
            return PConstructor(c, p)
        }

    static member Wildcard =
        sym '_' >>% Wildcard

    static member All =
        [
            PatternParsers.Identifier
            PatternParsers.Literal
            PatternParsers.Constructor
            PatternParsers.Wildcard
        ]

    static member Pattern = any PatternParsers.All
  end

type ExpressionParsers() =
  class
    
    static let expr = ExpressionParsers.Expression
    static let mws1 p = AuxiliaryParsers.ws1 >>. p

    static let ident = AuxiliaryParsers.ident
    static let eident =
        parse {
            let! name = ident
            return { EIdentifier.Name = name }
        }
    static let elambda =
        parse {
            let! _ = sym '\\'
            let! arg = ident
            let! _ = between pws (pstr "->") pws
            let! body = expr
            return { ELambda.Arg = {EIdentifier.Name = arg}; Body = body }
        }

    static member Identifier = 
        eident |>> EIdentifier

    static member Literal = 
        let lit = any LiteralParser.All
        parse {
            let! value = lit
            return ELiteral{ Value = value }
        }

    static member Unary = 
        let un = any UnaryOperationsParsers.All
        parse {
            let! op = un
            let! e = skipws expr
            return EUnary{ Operation = op; Arg = e }
        }

    static member Binary = 
        let bin = any BinaryOperationsParsers.All
        parse {
            let! arg1 = expr
            let! binop = skipws bin
            let! arg2 = skipws expr
            return EBinary{ Operation = binop; Arg1 = arg1; Arg2 = arg2 }
        }

    static member ConstrApplying =
        parse {
            let! constr = AuxiliaryParsers.constr
            let! e = mws1 expr
            return EConstrApplying{ ConstrName = constr; Value = e }
        }

    static member Lambda =
        elambda |>> ELambda
        
    static member FunApplying =
        let func = (eident |>> Function) <|> (elambda |>> Lambda) 
        parse {
            let! f = func
            let! e = mws1 expr
            return EFunApplying{ Func = f; Arg = e }
        }

    static member IfElse =
        parse {
            let! _ = pstr "if"
            let! cond = mws1 expr
            let! _ = mws1 <| pstr "then"
            let! onTrue = mws1 expr
            let! _ = mws1 <| pstr "else"
            let! onFalse = mws1 expr
            let! _ = mws1 <| pstr "end"
            return EIfElse{ Condition = cond; OnTrue = onTrue; OnFalse = onFalse }
        }
    
    static member LetIn = 
        parse {
            let! _ = pstr "let"
            let! bind = mws1 DeclarationParser.Declaration
            let! _ = mws1 <| pstr "in"
            let! body = mws1 expr
            let! _ = mws1 <| pstr "end"
            return ELetIn{ Binding = bind; Body = body }
        }

    static member CaseOf =
        let patternLine =
            parse {
                let! _ = mws1 <| pstr "|"
                let! p = PatternParsers.Pattern
                let! _ = between pws (pstr "->") pws
                let! e = expr
                let! _ = skipws <| pstr ";" 
                return (p, e)
            }
        parse {
            let! _ = pstr "case"
            let! sample = mws1 expr
            let! _ = mws1 <| pstr "of"
            let! plist = many1 patternLine
            return ECaseOf{ Matching = sample; Patterns = plist }
        }

    static member TypedExpression = ()
        // requires type parser
        // TODO

    static member All =
        [
            ExpressionParsers.Identifier
            ExpressionParsers.Literal
            ExpressionParsers.IfElse
            ExpressionParsers.LetIn
            ExpressionParsers.Unary
            ExpressionParsers.Binary
            ExpressionParsers.Lambda
            ExpressionParsers.FunApplying
            ExpressionParsers.ConstrApplying
            ExpressionParsers.CaseOf
        ]

    static member Expression = 
        let e = any ExpressionParsers.All
        e <|> (between (sym '(') (skipws e) (skipws <| sym ')'))
  end

and DeclarationParser() =
  class
    static let mws1 p = AuxiliaryParsers.ws1 >>. p
    static let expr = ExpressionParsers.Expression
    
    static let eident =
        parse {
            let! name = AuxiliaryParsers.ident
            return { EIdentifier.Name = name }
        }

    static member Value =
        parse {
            let! _ = pstr "val"
            let! name = mws1 eident
            let! _ = between pws (pstr "=") pws
            let! value = expr
            return DValue{ Name = name; Value = value }
        }

    static member Function =
        parse {
            let! _ = pstr "fun"
            let! f = mws1 eident
            let! x = mws1 eident
            let! _ = between pws (pstr "=") pws
            let! body = expr
            return DFunction{ Name = f; Arg = x; Body = body }
        }        

    static member Datatype =
        let oft = 
            parse {
                let! _ = mws1 <| pstr "of"
                let! t = TypeParser.Type
                return t
            }
        let constr =
            parse {
                let! _ = mws1 <| pstr "|"
                let! c = AuxiliaryParsers.constr
                let! t = opt oft
                let! _ = skipws <| pstr ";" 
                return (c, t)
            }
        parse {
            let! _ = pstr "datatype"
            let! name = mws1 AuxiliaryParsers.constr
            let! _ = between pws (pstr "=") pws
            let! clist = many1 constr
            return DDatatype{ Name = {EIdentifier.Name = name}; Constrs = clist }
        }

    static member All =
        [
            DeclarationParser.Value
            DeclarationParser.Function
            DeclarationParser.Datatype
        ]

    static member Declaration = any DeclarationParser.All
  end

and SuffleParsers() =
  class

    static member Program = many DeclarationParser.Declaration

  end