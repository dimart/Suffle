module Suffle.Tests.Interpreter

open NUnit.Framework
open Suffle.Specification.Types
open Suffle.Interpreter.ExceptionList
open Suffle.Interpreter.Interpreter

[<TestFixture>]
type ``Atomic evaluations``() = 
    [<Test>]
    member this.Literals () = 
        Assert.True <| (VUnit = (evalExpression (ELiteral {Value = VUnit})))
        Assert.True (VBool true = (evalExpression (ELiteral {Value = VBool true})))
        Assert.True (VInt 5 = (evalExpression (ELiteral {Value = VInt 5})))
        Assert.True (VChar 'c' = (evalExpression (ELiteral {Value = VChar 'c'})))
        Assert.False (VInt 7 = (evalExpression (ELiteral {Value = VInt 5})))

    [<Test>]
    member this.``If-else statements`` () = 
    (*
        if true then 3 else 5
    *)
        Assert.True <| 
            (VInt 3 = (
                evalExpression (
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
                evalExpression (
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
            evalExpression (
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
                evalExpression (
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
                evalExpression (
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
                evalExpression (
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
                evalExpression (
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
                evalExpression (
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
                evalExpression (
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
                evalExpression (
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
                evalExpression (
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
                evalExpression (
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
                evalExpression (
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
                evalExpression (
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
                evalExpression (
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
                evalExpression (
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
                evalExpression (
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
                evalExpression (
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
                evalExpression (
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
                evalExpression (
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
                evalExpression (
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
                evalExpression (
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
                evalExpression (
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
              VClosure ([],ELambda {Arg = {Name = "x";};
                        Body = EIdent {Name = "x";};})
                = (
                evalExpression (
                    ELambda {
                        Arg = { EIdent.Name = "x" }
                        Body = EIdent { Name = "x" }
                    }
                )
            )
        )

    (*
        (\x -> 5) 3
    *)
        Assert.True (
            (VInt 5) = (
                    evalExpression (
                        EFunApp {
                            Func = ELambda {
                                Arg = { EIdent.Name = "x"}
                                Body = ELiteral { Value = VInt 5 }
                            }
                            Arg = ELiteral { Value = VInt 3 }
                        }
                    )
                )
            )
            
    [<Test>]
    member this.``Case .. of .. evaluation``() = 
    (*
        case 5 of
        | 3 -> 7
        | 4 -> 8
        | 5 -> 6
        end
    *)
        Assert.True (
            VInt 6 = (
                evalExpression (
                    ECaseOf {
                        Matching = ELiteral { Value = VInt 5 }
                        Patterns = 
                            [
                                (PLiteral { ELiteral.Value = VInt 3}, ELiteral { Value = VInt 7 }); 
                                (PLiteral { ELiteral.Value = VInt 4}, ELiteral { Value = VInt 8 });
                                (PLiteral { ELiteral.Value = VInt 5}, ELiteral { Value = VInt 6 })
                            ]
                    }
                )
            )
        )

    (*
        case 5 of
        | x -> x + 3
        end
    *)
        Assert.True (
            VInt 8 = (
                evalExpression (
                    ECaseOf {
                        Matching = ELiteral { Value = VInt 5 }
                        Patterns = 
                            [
                                (PIdent { EIdent.Name = "x" }, EBinary { 
                                    Op = BAdd
                                    Arg1 = EIdent { Name = "x" } 
                                    Arg2 = ELiteral { Value = VInt 3 }
                                })
                            ]
                    }
                )
            )
        )
