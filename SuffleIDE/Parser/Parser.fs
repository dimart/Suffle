module SomeModule

let xxxxxxx = ()

(* 
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
    let wsc = _wsc
    /// At least one whitespace character
    let ws1 = _ws1
    /// Keyword parser. Keyword must be surrounded by whitespace
    let kw (s : string) = between _ws1 (pstr s) _ws1
    
    let ident = syms alphas >|>> many (syms identSymbols) |>> chars2str
    let constr = syms ['A'..'Z'] >|>> many (syms (alphas @ digits)) |>> chars2str
    
    let inbrackets p = (between (sym '(') (skipws p) (skipws <| sym ')'))
  end

and LiteralParser() =
  class
    let Unit = pstr "()" >>% VUnit
    let Bool = (pstr "true" >>% VBool true) <|> (pstr "false" >>% VBool false)
    let Char = between (sym '\'') (symf (fun _ -> true)) (sym '\'') |>> VChar
    let Int  = pint |>> VInt

    let All = 
        [
            LiteralParser.Unit
            LiteralParser.Bool
            LiteralParser.Char
            LiteralParser.Int
        ]
  end

and TypeParser() =
  class
    let Unit = pstr "unit" >>% TUnit
    let Bool = pstr "bool" >>% TBool
    let Char = pstr "char" >>% TChar
    let Int  = pstr "int"  >>% TInt
    let Datatype = AuxiliaryParsers.constr |>> TDatatype
    let Var = sym '\'' >|>> many1 (syms <| ['a'..'z'] @ ['A'..'Z']) |>> (TVar << chars2str)
    let Lambda = 
        parse {
            let! a = TypeParser.Basic <|> AuxiliaryParsers.inbrackets (TypeParser.Type)
            let! _ = between pws (pstr "->") pws
            let! b = TypeParser.Type
            return TLambda(a, b)
        }

    let Basics =
        [
            TypeParser.Unit
            TypeParser.Bool
            TypeParser.Char
            TypeParser.Int
            TypeParser.Datatype
            TypeParser.Var
        ]

    let Basic = any TypeParser.Basics

    let All =
        TypeParser.Basics @ [TypeParser.Lambda]

    let Type = 
        let t = any TypeParser.All
        t <|> AuxiliaryParsers.inbrackets t
  end

and UnaryOperationParsers() =
  class
    let Negation =        sym '-' >>% UNegation
    let LogicalNegation = sym '!' >>% ULogicalNegation

    let All = [
                            UnaryOperationParsers.Negation
                            UnaryOperationParsers.LogicalNegation
                        ]
  end

and BinaryOperationParsers() =
  class
    // Arithmetic
    let Add = pstr "+" >>% BAdd
    let Sub = pstr "-" >>% BSub
    let Div = pstr "/" >>% BDiv
    let Mul = pstr "*" >>% BMul

    let AllArithmetic =
        [
            BinaryOperationParsers.Add
            BinaryOperationParsers.Sub
            BinaryOperationParsers.Div
            BinaryOperationParsers.Mul
        ]

    // Logic
    let And = pstr "&&" >>% BAnd
    let Or  = pstr "||" >>% BOr
    let Eq  = pstr "==" >>% BEq
    let NEq = pstr "<>" >>% BNEq

    let AllLogic =
        [
            BinaryOperationParsers.And
            BinaryOperationParsers.Or
            BinaryOperationParsers.Eq
            BinaryOperationParsers.NEq
        ]

    // Auxiliary
    // TODO

    let All =
        [
            BinaryOperationParsers.Add
            BinaryOperationParsers.Sub
            BinaryOperationParsers.Div
            BinaryOperationParsers.Mul
            BinaryOperationParsers.And
            BinaryOperationParsers.Or
            BinaryOperationParsers.Eq
            BinaryOperationParsers.NEq
        ]
  end

and PatternParsers() =
  class
    let Identifier = 
        parse {
            let! name = AuxiliaryParsers.ident
            return PIdentifier{ Name = name }
        }

    let Literal =
        let lit = any LiteralParser.All
        parse {
            let! value = lit
            return PLiteral{ Value = value }
        }

    let Constructor =
        parse {
            let! c = AuxiliaryParsers.constr 
            let! p = PatternParsers.Pattern
            return PConstructor(c, p)
        }

    let Wildcard =
        sym '_' >>% Wildcard

    let All =
        [
            PatternParsers.Identifier
            PatternParsers.Literal
            PatternParsers.Constructor
            PatternParsers.Wildcard
        ]

    let Pattern = any PatternParsers.All
  end

and ExpressionParsers() =
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

    let Identifier = 
        eident |>> EIdentifier

    let Literal = 
        let lit = any LiteralParser.All
        parse {
            let! value = lit
            return ELiteral{ Value = value }
        }

    let Unary = 
        let un = any UnaryOperationParsers.All
        parse {
            let! op = un
            let! e = skipws expr
            return EUnary{ Operation = op; Arg = e }
        }

    let Binary = 
        let bin = any BinaryOperationParsers.All
        parse {
            let! arg1 = expr
            let! binop = skipws bin
            let! arg2 = skipws expr
            return EBinary{ Operation = binop; Arg1 = arg1; Arg2 = arg2 }
        }

    let ConstrApplying =
        parse {
            let! constr = AuxiliaryParsers.ctor
            let! e = mws1 expr
            return EConstrApplying{ ConstrName = constr; Value = e }
        }

    let Lambda =
        elambda |>> ELambda
        
    let FunApplying =
        let func = (eident |>> Function) <|> (elambda |>> Lambda) 
        parse {
            let! f = func
            let! e = mws1 expr
            return EFunApplying{ Func = f; Arg = e }
        }

    let IfElse =
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
    
    let LetIn = 
        parse {
            let! _ = pstr "let"
            let! bind = mws1 DeclarationParser.Declaration
            let! _ = mws1 <| pstr "in"
            let! body = mws1 expr
            let! _ = mws1 <| pstr "end"
            return ELetIn{ Binding = bind; Body = body }
        }

    let CaseOf =
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

    let TypedExpression = ()
        // requires type parser
        // TODO

    let All =
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

    let Expression = 
        let e = any ExpressionParsers.All
        e <|> AuxiliaryParsers.inbrackets e
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

    let Value =
        parse {
            let! _ = pstr "val"
            let! name = mws1 eident
            let! _ = between pws (pstr "=") pws
            let! value = expr
            return DValue{ Name = name; Value = value }
        }

    let Function =
        parse {
            let! _ = pstr "fun"
            let! f = mws1 eident
            let! x = mws1 eident
            let! _ = between pws (pstr "=") pws
            let! body = expr
            return DFunction{ Name = f; Arg = x; Body = body }
        }        

    let Datatype =
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

    let All =
        [
            DeclarationParser.Value
            DeclarationParser.Function
            DeclarationParser.Datatype
        ]

    let Declaration = any DeclarationParser.All
  end

and SuffleParsers() =
  class

    let Program = many DeclarationParser.Declaration

  end

*)