namespace Suffle.Specification.Types

// *** Suffle types ***

type Type = 
| TUnit
| TBool
| TChar 
| TInt 
| TLambda of Type * Type
| TDatatype of string * (Type list)
| TVar of string

type Constructor = string * Type list

type DataType = Constructor list

type Context = (string * Value) list

// *** Suffle values ***

and Value = 
| VUnit 
| VBool of bool
| VChar of char
| VInt of int
| VCons of string * Value
| VClosure of Context * Expression

// *** AST ***

and ASTNode = 
| Prog of Declaration list
| Expr of Expression
| Decl of Declaration

and Declaration =
| DValue of DValue
| DDatatype of DDatatype
| DFunction of DFunction

and Expression = 
| EIdent of EIdent
| ELiteral of ELiteral
| EIfElse of EIfElse
| ELetIn of ELetIn
| EUnary of EUnary
| EBinary of EBinary
| ELambda of ELambda
| EFunApp of EFunApp
| ECaseOf of ECaseOf

// Patterns for CaseOf
and Pattern =
| PIdent of EIdent
| PLiteral of ELiteral
| PCtor of string * (Pattern list)
| PWildcard

// Expressions
and EIdent = { Name : string }
and ELiteral = { Value : Value }
and EIfElse = 
    { 
        Cond : Expression
        OnTrue : Expression
        OnFalse : Expression
    }
and ELetIn =
    {
        Binding : Declaration
        Body : Expression
    }
and EUnary =
    {
        Op : UnaryOp
        Arg : Expression
    }
and EBinary =
    {
        Op : BinaryOp
        Arg1 : Expression
        Arg2 : Expression
    }
and ELambda =
    {
        Arg : EIdent
        Body : Expression
    }
and EFunApp =
    {
        Func : Expression
        Arg : Expression
    }
and ECaseOf =
    {
        Matching : Expression
        Patterns : (Pattern * Expression) list
    }

// Declarations
and DValue =
    {
        Type : Type
        Name : EIdent
        Value : Expression
    }
and DDatatype =
    {
        Name : EIdent
        Params : string list
        Ctors : DataType
    }
and DFunction =
    {
        Type : Type
        Name : EIdent
        Arg : EIdent
        Body : Expression
    }

// BinaryOps
and BinaryOp =
// Arithmetics
| BAdd
| BSub
| BDiv
| BMul
// Logic
| BAnd
| BOr
// Comparation
| BEQ
| BNEQ
| BGT
| BLT
| BNGT
| BNLT

and UnaryOp =
| UNeg
| UNot


