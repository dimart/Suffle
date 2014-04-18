module Tests.Interpreter

open NUnit.Framework
open Suffle.Specification.Types
open Suffle.Interpreter

//  Expr (EBinary {Op = BAdd; Arg1 = ELiteral {Value = VInt 5}; Arg2 = ELiteral {Value = VInt 7}})

[<TestFixture>]
type ``Atomic evaluations`` = 
    
    [<Test>]
    member this.Literals () = 
        Assert.True <| (VUnit = (eval <| Expr (ELiteral {Value = VUnit})))
        Assert.True (VBool true = (eval <| Expr (ELiteral {Value = VBool true})))
        Assert.True (VInt 5 = (eval <| Expr (ELiteral {Value = VInt 5})))
        Assert.True (VChar 'c' = (eval <| Expr (ELiteral {Value = VChar 'c'})))
        Assert.True (VFloat 15.0 = (eval <| Expr (ELiteral {Value = VFloat 15.0})))
        Assert.False (VInt 7 = (eval <| Expr (ELiteral {Value = VInt 5})))

    [<Test>]
    member this.``If-else statements`` () = 
    (*
        if (if true then 3 else 5
    *)
        Assert.True <| 
            (VInt 3 = (
                eval <| Expr (
                    EIfElse {
                        Cond = ELiteral {
                            Value = VBool true
                            }
                        OnTrue = ELiteral {
                            Value = VInt 3
                            }
                        OnFalse = ELiteral {
                            Value = VInt 5
                        }
                    })
                )
            )

        (*
            if (if false then true else false) then 3.0 else 5.0
        *)
        Assert.True (
            VFloat 5.0 = (
                eval <| Expr (
                    EIfElse {
                        Cond = EIfElse {
                            Cond = ELiteral {
                                Value = VBool false
                            }
                            OnTrue = ELiteral {
                                Value = VBool true
                            }
                            OnFalse = ELiteral {
                                Value = VBool false
                            }
                        }
                        OnTrue = ELiteral {
                            Value = VFloat 3.0
                        }
                        OnFalse = ELiteral {
                            Value = VFloat 5.0
                        }
                    }
                )
            )
        )

        (*
            if true then () else 5
        *)
        try 
            eval <| Expr (
                EIfElse {
                    Cond = ELiteral {
                        Value = VBool true
                    }
                    OnTrue = ELiteral {
                        Value = VUnit
                    }
                    OnFalse = ELiteral {
                        Value = VInt 5
                    }
                }
            )
        with
        | TypeMismatchException x -> Assert.Equals x "Expected unit in \'else\' evaluation"
