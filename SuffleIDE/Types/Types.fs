namespace Types

// *** Suffle types ***

type Type = 
| TUnit
| TBool
| TChar 
| TInt 
| TLambda of Type * Type
| TDatatype of string
| TVar of string

type DataType = (string * Type) list

type Context = (string * Value) list

// *** Suffle values ***

and Value = 
| VUnit 
| VBool of bool
| VChar of char
| VInt of int
| VFloat of float
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
| EIdentifier of EIdentifier
| ELiteral of ELiteral
| EIfElse of EIfElse
| ELetIn of ELetIn
| EUnary of EUnary
| EBinary of EBinary
| ELambda of ELambda
| EFunApplying of EFunApplying
| EConstrApplying of EConstrApplying

// Patterns for CaseOf
and Pattern =
| PIdentifier of EIdentifier
| PLiteral of ELiteral
| PConstructor of string * Pattern
| Wildcard

and Function =
| Function of EIdentifier
| Lambda of ELambda

// Expressions
and EIdentifier = { Name : string }
and ELiteral = { Value : Value }
and EIfElse = 
    { 
        Condition : Expression
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
        Operation : UnaryOperation
        Arg : Expression
    }
and EBinary =
    {
        Operation : BinaryOperation
        Arg1 : Expression
        Arg2 : Expression
    }
and ELambda =
    {
        Arg : EIdentifier
        Body : Expression
    }
and EFunApplying =
    {
        Func : Function
        Arg : Expression
    }
and EConstrApplying =
    {
        ConstrName : string
        Value : Expression
    }
and ECaseOf =
    {
        Matching : Expression
        Patterns : (Pattern * Expression) list
    }

// Declarations
and DValue =
    {
        Name : EIdentifier
        Value : Expression
    }
and DType =
    {
        Type : Type
    }
and DDatatype =
    {
        Name : EIdentifier
        Constrs : DataType
    }
and DFunction =
    {
        Name : EIdentifier
        Arg : EIdentifier
        Body : Expression
    }

// BinaryOperations

and BinaryOperation =
// Arithmetics
| BAdd
| BSub
| BDiv
| BMul
// Logic
| BAnd
| BOr
| BEq
| BNEq

and UnaryOperation =
| UNegation
| ULogicalNegation
