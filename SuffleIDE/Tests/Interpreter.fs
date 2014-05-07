module Tests.Interpreter

open NUnit.Framework
open Suffle.Specification.Types
open Suffle.Interpreter
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
        Assert.False (VInt 7 = (eval <| Expr (ELiteral {Value = VInt 5})))

    [<Test>]
    member this.``If-else statements`` () = 
    (*
        if true then 3 else 5
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
            if (if false then true else false) then 3 else 5
        *)
        Assert.True (
            VInt 5 = (
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

    member this.``Let in statements`` () = 

        (*
            let x = 5 in 2
        *)
        Assert.True (
            (VInt 2) = ( 
                eval <| Expr (
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
                eval <| Expr (
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

    member this.``Unary operators`` = 

        (*
            -5
        *)
        Assert.True (
            (VInt -5) = (
                eval <| Expr (
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
                eval <| Expr (
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
                eval <| Expr (
                    EUnary {
                        Op = UNot
                        Arg = ELiteral { Value = VBool true }
                    }
                )
            )
        )

    member this.``Binary Arithmetic Operators`` () = 
        
        (*
            5 + 3
        *)
        Assert.True (
            (VInt 8) = (
                eval <| Expr (
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
                eval <| Expr (
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
                eval <| Expr (
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
                eval <| Expr (
                    EBinary {
                        Op = BDiv
                        Arg1 = ELiteral { Value = VInt 7 }
                        Arg2 = ELiteral { Value = VInt 5 }
                    }
                )
            )
        )

    member this.``Binary Logical Operators`` () = 

        (*
            true && false
        *)
        Assert.True (
            (VBool false) = (
                eval <| Expr (
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
                eval <| Expr (
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
                eval <| Expr (
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
                eval <| Expr (
                    EBinary {
                        Op = BOr
                        Arg1 = ELiteral { Value = VBool false }
                        Arg2 = ELiteral { Value = VBool false }
                    }
                )
            )
        )

    member this.``Binary Comparison Expressions`` () = 
                
        (*
            5 = 3
        *)
        Assert.True (
            (VBool false) = (
                eval <| Expr (
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
                eval <| Expr (
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
                eval <| Expr (
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
                eval <| Expr (
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
                eval <| Expr (
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
                eval <| Expr (
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
                eval <| Expr (
                    EBinary {
                        Op = BNLT
                        Arg1 = ELiteral { Value = VInt 5 }
                        Arg2 = ELiteral { Value = VInt 5 }
                    }
                )
            )
        )

