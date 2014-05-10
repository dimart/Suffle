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

type Ctor = string * Type list

type Context = (string * Value) list

// *** Suffle values ***

and Value = 
| VUnit 
| VBool of bool
| VChar of char
| VInt of int
| VCtor of string * Value list
| VClosure of Context * Expression

// *** AST ***

and Program = Declaration list

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
| ECtor of ECtor

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
and ECtor =
    {
        CtorName : string
        Args : EIdent list
    }

// Declarations
and DValue =
    {
        Type : Type
        Name : EIdent
        Value : Expression
    }
and DType =
    {
        Type : Type
    }
and DDatatype =
    {
        Name : EIdent
        Params : string list
        Ctors : Ctor list
    }
and DFunction =
    {
        Type : Type
        Name : EIdent
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


