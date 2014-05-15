module Suffle.Tests.Interpreter

open NUnit.Framework
open Suffle.Specification.Types
open Suffle.Interpreter.ExceptionList
open Suffle.Interpreter.Interpreter

[<TestFixture>]
type ``Atomic evaluations``() = 
    [<Test>]
    member this.Literals () = 
        Assert.True <| (VUnit = (eval (ELiteral {Value = VUnit})))
        Assert.True (VBool true = (eval (ELiteral {Value = VBool true})))
        Assert.True (VInt 5 = (eval (ELiteral {Value = VInt 5})))
        Assert.True (VChar 'c' = (eval (ELiteral {Value = VChar 'c'})))
        Assert.False (VInt 7 = (eval (ELiteral {Value = VInt 5})))

    [<Test>]
    member this.``If-else statements`` () = 
    (*
        if true then 3 else 5
    *)
        Assert.True <| 
            (VInt 3 = (
                eval (
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
            if (if false then true else false) then 3 else 5
        *)
        Assert.True (
            VInt 5 = (
                eval (
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
                            Value = VInt 3
                        }
                        OnFalse = ELiteral {
                            Value = VInt 5
                        }
                    }
                )
            )
        )

        (*
            if true then () else 5
        *)
        try 
            eval (
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
            ) |> ignore
            ()
        with
        | TypeMismatchException (x, _) -> Assert.True <| x.Equals "Expected unit in \'else\' evaluation"

    [<Test>]
    member this.``Let in statements`` () = 

        (*
            let x = 5 in 2
        *)
        Assert.True (
            (VInt 2) = ( 
                eval (
                    ELetIn {
                        Binding = DValue {
                            Type = TInt
                            Name = { EIdent.Name = "x" }
                            Value = ELiteral {
                                Value = VInt 5
                            }
                        }
                        Body = ELiteral {
                                Value = VInt 2
                        }
                    }
                )
            )
        )

        (*
            let intName = 5 in intName
        *)
        Assert.True (
            (VInt 5) = (
                eval (
                    ELetIn {
                        Binding = DValue {
                            Type = TInt
                            Name = {EIdent.Name = "intName"}
                            Value = ELiteral { Value = VInt 5 }
                        }
                        Body = EIdent {
                            Name = "intName"
                        }
                    }
                )
            )
        )

    [<Test>]
    member this.``Unary operators``() = 

        (*
            -5
        *)
        Assert.True (
            (VInt -5) = (
                eval (
                    EUnary {
                        Op = UNeg
                        Arg = ELiteral { Value = VInt 5 }
                    }
                )
            )
        )

        (*
            -(-3)
        *)
        Assert.True (
            (VInt 3) = (
                eval (
                    EUnary {
                        Op = UNeg
                        Arg = EUnary {
                            Op = UNeg
                            Arg = ELiteral { Value = VInt 3 }
                        }
                    }
                )
            )
        )

        (*
            ~true
        *)
        Assert.True (
            (VBool false) = (
                eval (
                    EUnary {
                        Op = UNot
                        Arg = ELiteral { Value = VBool true }
                    }
                )
            )
        )

    [<Test>]
    member this.``Binary Arithmetic Operators`` () = 
        
        (*
            5 + 3
        *)
        Assert.True (
            (VInt 8) = (
                eval (
                    EBinary {
                        Op = BAdd
                        Arg1 = ELiteral { Value = VInt 5 }
                        Arg2 = ELiteral { Value = VInt 3 }
                    }
                )
            )
        )

        (*
            5 - 3
        *)
        Assert.True (
            (VInt 2) = (
                eval (
                    EBinary {
                        Op = BSub
                        Arg1 = ELiteral { Value = VInt 5 }
                        Arg2 = ELiteral { Value = VInt 3 }
                    }
                )
            )
        )

        (*
            8 / 2
        *)
        Assert.True (
            (VInt 4) = (
                eval (
                    EBinary {
                        Op = BDiv
                        Arg1 = ELiteral { Value = VInt 8 }
                        Arg2 = ELiteral { Value = VInt 2 }
                    }
                )
            )
        )

        (*
            7 * 5
        *)
        Assert.True (
            (VInt 35) = (
                eval (
                    EBinary {
                        Op = BMul
                        Arg1 = ELiteral { Value = VInt 7 }
                        Arg2 = ELiteral { Value = VInt 5 }
                    }
                )
            )
        )

    [<Test>]
    member this.``Binary Logical Operators`` () = 

        (*
            true && false
        *)
        Assert.True (
            (VBool false) = (
                eval (
                    EBinary {
                        Op = BAnd
                        Arg1 = ELiteral { Value = VBool true }
                        Arg2 = ELiteral { Value = VBool false }
                    }
                )
            )
        )

        (*
            true && true
        *)
        Assert.True (
            (VBool true) = (
                eval (
                    EBinary {
                        Op = BAnd
                        Arg1 = ELiteral { Value = VBool true }
                        Arg2 = ELiteral { Value = VBool true }
                    }
                )
            )
        )

        (*
            false || true
        *)
        Assert.True (
            (VBool true) = (
                eval (
                    EBinary {
                        Op = BOr
                        Arg1 = ELiteral { Value = VBool false }
                        Arg2 = ELiteral { Value = VBool true }
                    }
                )
            )
        )

        (*
            false || false
        *)
        Assert.True (
            (VBool false) = (
                eval (
                    EBinary {
                        Op = BOr
                        Arg1 = ELiteral { Value = VBool false }
                        Arg2 = ELiteral { Value = VBool false }
                    }
                )
            )
        )

    [<Test>]
    member this.``Binary Comparison Expressions`` () = 
                
        (*
            5 = 3
        *)
        Assert.True (
            (VBool false) = (
                eval (
                    EBinary {
                        Op = BEQ
                        Arg1 = ELiteral { Value = VInt 5 }
                        Arg2 = ELiteral { Value = VInt 3 }
                    }
                )
            )
        )

        (*
            5 != 3
        *)
        Assert.True (
            (VBool true) = (
                eval (
                    EBinary {
                        Op = BNEQ
                        Arg1 = ELiteral { Value = VInt 5 }
                        Arg2 = ELiteral { Value = VInt 3 }
                    }
                )
            )
        )
        
        (*
            5 > 3
        *)
        Assert.True (
            (VBool true) = (
                eval (
                    EBinary {
                        Op = BGT
                        Arg1 = ELiteral { Value = VInt 5 }
                        Arg2 = ELiteral { Value = VInt 3 }
                    }
                )
            )
        )
                
        (*
            5 < 3
        *)
        Assert.True (
            (VBool false) = (
                eval (
                    EBinary {
                        Op = BLT
                        Arg1 = ELiteral { Value = VInt 5 }
                        Arg2 = ELiteral { Value = VInt 3 }
                    }
                )
            )
        )
                
        (*
            5 <= 3
        *)
        Assert.True (
            (VBool false) = (
                eval (
                    EBinary {
                        Op = BNGT
                        Arg1 = ELiteral { Value = VInt 5 }
                        Arg2 = ELiteral { Value = VInt 3 }
                    }
                )
            )
        )
                
        (*
            5 >= 3
        *)
        Assert.True (
            (VBool true) = (
                eval (
                    EBinary {
                        Op = BNLT
                        Arg1 = ELiteral { Value = VInt 5 }
                        Arg2 = ELiteral { Value = VInt 3 }
                    }
                )
            )
        )
                
        (*
            5 >= 5
        *)
        Assert.True (
            (VBool true) = (
                eval (
                    EBinary {
                        Op = BNLT
                        Arg1 = ELiteral { Value = VInt 5 }
                        Arg2 = ELiteral { Value = VInt 5 }
                    }
                )
            )
        )

    [<Test>]
    member this.``Lambda evaluation``() = 
    (*
        (\x -> x)
    *)
        Assert.True (
            VClosure ([], ELambda {
                Arg = { EIdent.Name = "x" }
                Body = EIdent { Name = "x" }
            }) = (
                eval (
                    ELambda {
                        Arg = { EIdent.Name = "x" }
                        Body = EIdent { Name = "x" }
                    }
                )
            )
        )
