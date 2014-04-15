module Tests.Interpreter

open NUnit.Framework
open Suffle.Specification.Types
open Suffle.Interpreter

//  Expr (EBinary {Op = BAdd; Arg1 = ELiteral {Value = VInt 5}; Arg2 = ELiteral {Value = VInt 7}})

let x = VUnit = (eval <| Expr (ELiteral {Value = VUnit}))

[<TestFixture>]
type ``Atomic evaluations`` = 
    
    [<Test>]
    member this.Literals () = 
        Assert.True (VUnit = (eval <| Expr (ELiteral {Value = VUnit})))
