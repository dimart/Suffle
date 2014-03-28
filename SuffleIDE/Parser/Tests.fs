module Tests

open NUnit.Framework
open ParserCombinators.Core
open Specification.Types
open Parser.Literals
open Parser.Types
open Parser.Pattern
open Parser.Structures

let isSucc (value : 'a) =
    function 
    | S(v, _) -> v = value
    | _ -> false

let isFail =
    function
    | F _ -> true
    | _ -> false

[<TestFixture>]
type ``Literal parsing``() =
    
    [<Test>]
    member x.``Unit`` () =
        Assert.True(isSucc VUnit <| run lUnit "()")

        Assert.True(isFail <| run lUnit "(]")
        Assert.True(isFail <| run lUnit "[)")
        Assert.True(isFail <| run lUnit "(())")

    [<Test>]
    member x.``Bool`` () =
        Assert.True(isSucc (VBool true) <| run lBool "true")
        Assert.True(isSucc (VBool false) <| run lBool "false")

        Assert.True(isFail <| run lBool "True")
        Assert.True(isFail <| run lBool "False")

    [<Test>]
    member x.``Char`` () =
        let r = run lChar
        Assert.True(isSucc (VChar 'a') <| r "'a'")
        Assert.True(isSucc (VChar 'x') <| r "'x'")
        Assert.True(isSucc (VChar '7') <| r "'7'")
        Assert.True(isSucc (VChar '\n') <| r "'\n'")
        Assert.True(isSucc (VChar '\t') <| r "'\t'")
        
        Assert.True(isFail <| r "a")
        Assert.True(isFail <| r "'a")
        Assert.True(isFail <| r "a'")
        Assert.True(isFail <| r "\"a\"")
        Assert.True(isFail <| r "'\k'")

    [<Test>]
    member x.``Int`` () =
        let r = run lInt
        Assert.True(isSucc (VInt 42) <| r "42")
        Assert.True(isSucc (VInt 42) <| r "+42")
        Assert.True(isSucc (VInt -7) <| r "-7")
        Assert.True(isSucc (VInt 0) <| r "0")
        Assert.True(isSucc (VInt 0) <| r "00")
        Assert.True(isSucc (VInt 123) <| r "0123")
        Assert.True(isSucc (VInt 0) <| r "-0")
        
        Assert.True(isFail <| r "3.14")
        Assert.True(isFail <| r "++7")
        Assert.True(isFail <| r "--7")

[<TestFixture>]
type ``Type parsing``() =
    
    [<Test>]
    member x.``Unit`` () =
        Assert.True(isSucc TUnit <| run tUnit "unit")

    [<Test>]
    member x.``Bool`` () =
        Assert.True(isSucc TBool <| run tBool "bool")

    [<Test>]
    member x.``Char`` () =
        Assert.True(isSucc TChar <| run tChar "char")

    [<Test>]
    member x.``Int`` () =
        Assert.True(isSucc TInt <| run tInt "int")

    [<Test>]
    member x.``Var`` () =
        let r = run tVar
        Assert.True(isSucc (TVar "'a") <| r "'a")
        Assert.True(isSucc (TVar "'T") <| r "'T")
        Assert.True(isSucc (TVar "'Type") <| r "'Type")
        
        Assert.True(isFail <| r "A")
        Assert.True(isFail <| r "a")
        Assert.True(isFail <| r "_type")

    [<Test>]
    member x.``Datatype`` () =
        let r = run tDatatype
        Assert.True(isSucc (TDatatype "A") <| r "A")
        Assert.True(isSucc (TDatatype "Tt") <| r "Tt")
        Assert.True(isSucc (TDatatype "Option") <| r "Option")
        Assert.True(isSucc (TDatatype "A1") <| r "A1")

        Assert.True(isFail <| r "A'")
        Assert.True(isFail <| r "A_B")
        
    [<Test>]
    member x.``Lambda`` () =
        let r = run tLambda
        Assert.True(isSucc (TLambda(TInt, TInt)) <| r "int -> int")
        Assert.True(isSucc (TLambda(TInt, TInt)) <| r "(int -> int)")
        Assert.True(isSucc (TLambda(TChar, TBool)) <| r "((char -> bool))")
        Assert.True(isSucc (TLambda(TVar "'a", TVar "'b")) <| r "'a -> 'b")
        
        Assert.True(isSucc (TLambda(TInt, TLambda(TInt, TInt))) <| r "int -> int -> int")
        Assert.True(isSucc (TLambda(TLambda(TInt, TInt), TInt)) <| r "(int -> int) -> int")
        Assert.True(isSucc (TLambda(TInt, TLambda(TInt, TInt))) <| r "int -> (int -> int)")
        Assert.True(isSucc (TLambda(TDatatype "A", TBool)) <| r "A -> bool")
        Assert.True(isSucc (TLambda(TDatatype "A", TDatatype "B")) <| r "A -> B")
        Assert.True(isSucc (TLambda(
                                TInt,
                                TLambda(
                                    TLambda(TChar, TBool),
                                    TLambda(
                                        TLambda(TVar "'a", TDatatype "A"),
                                        TLambda(TInt, TInt)
                                    )
                                )
                            ) 
                           ) <| r "int -> (char -> bool) -> ('a -> A) -> (int -> int)"
                    )
        
        Assert.True(isFail <| r "int")
        Assert.True(isFail <| r "int <- int")
        Assert.True(isFail <| r "a -> int")
        Assert.True(isFail <| r "(int -> int")
        Assert.True(isFail <| r "(int) -> ((int)))")
        Assert.True(isFail <| r "int -> char -> bool -> A -> B -> C -> 'a -> 'b -> 'c ->")

[<TestFixture>]
type ``Pattern parsing``() =
    
    [<Test>]
    member x.``Identifier`` () =
        let r = run pIdentifier
        Assert.True(isSucc (PIdent{Name = "function1"}) <| r "function1")
        Assert.True(isSucc (PIdent{Name = "_arg1"}) <| r "_arg1")
        Assert.True(isSucc (PIdent{Name = "g'"}) <| r "g'")
        Assert.True(isSucc (PIdent{Name = "F"}) <| r "F")

        Assert.True(isFail <| r "1a")
        Assert.True(isFail <| r "a-b")
        Assert.True(isFail <| r "'a")
        Assert.True(isFail <| r "@x")
        Assert.True(isFail <| r "x@")

    [<Test>]
    member x.``Literal`` () =
        let r = run pLiteral
        Assert.True(isSucc (PLiteral{Value = VUnit}) <| r "()")
        Assert.True(isSucc (PLiteral{Value = VInt 123}) <| r "123")
        Assert.True(isSucc (PLiteral{Value = VInt -42}) <| r "-42")
        Assert.True(isSucc (PLiteral{Value = VInt 0}) <| r "0")
        Assert.True(isSucc (PLiteral{Value = VChar 'a'}) <| r "'a'")
        Assert.True(isSucc (PLiteral{Value = VChar '\n'}) <| r "'\n'")
        Assert.True(isSucc (PLiteral{Value = VBool true}) <| r "true")
        Assert.True(isSucc (PLiteral{Value = VBool false}) <| r "false")

    [<Test>]
    member x.``Wildcard`` () =
        Assert.True(isSucc PWildcard <| run pWildcard "_")

    [<Test>]
    member x.``Constructor`` () =
        let r = run pCtor
        Assert.True(isSucc (PCtor("A", PLiteral{Value = VInt 123})) <| r "A 123")
        Assert.True(isSucc (PCtor("A", PLiteral{Value = VChar 'x'})) <| r "A 'x'")
        Assert.True(isSucc (PCtor("A", PWildcard)) <| r "A _")
        Assert.True(isSucc (PCtor("A", PLiteral{Value = VBool true})) <| r "A true")
        Assert.True(isSucc (PCtor("X", PLiteral{Value = VBool false})) <| r "X(false)")
        Assert.True(isSucc (PCtor("X", PIdent{Name = "x"})) <| r "X(x)")
        Assert.True(isSucc (PCtor("X", PIdent{Name = "y"})) <| r "X y")


[<TestFixture>]
type ``Expression parsing``() =
    
    [<Test>]
    member x.``Binary`` () =      
        let r = run eBinary
        // Arithmetic
        Assert.True(isSucc (EBinary{Op = BAdd
                                    Arg1 = ELiteral{Value = VInt 4}
                                    Arg2 = ELiteral{Value = VInt 5}}) 
                        <| r "4+5")
        Assert.True(isSucc (EBinary{Op = BSub
                                    Arg1 = EIdent{ Name = "x" }
                                    Arg2 = ELiteral{Value = VInt 7}}) 
                        <| r "x - 7")
        Assert.True(isSucc (EBinary{Op = BDiv
                                    Arg1 = EIdent{ Name = "x" }
                                    Arg2 = ELiteral{Value = VInt 7}}) 
                        <| r "x / 7")
        Assert.True(isSucc (EBinary{Op = BMul
                                    Arg1 = EIdent{ Name = "x" }
                                    Arg2 = ELiteral{Value = VInt 7}}) 
                        <| r "x * 7")
        Assert.True(isSucc (EBinary{Op = BAdd
                                    Arg1 = EBinary{
                                                Op = BMul
                                                Arg1 = EIdent{ Name = "x" }
                                                Arg2 = EIdent{ Name = "y" }
                                           }
                                    Arg2 = EIdent{ Name = "z" }}) 
                        <| r "x * y + z")
        Assert.True(isSucc (EBinary{Op = BSub
                                    Arg2 = EBinary{
                                                Op = BDiv
                                                Arg1 = EIdent{ Name = "x" }
                                                Arg2 = EIdent{ Name = "y" }
                                           }
                                    Arg1 = EIdent{ Name = "z" }}) 
                        <| r "z - x / y")

        
        // Logic
        Assert.True(isSucc (EBinary{Op = BAnd
                                    Arg1 = EIdent{ Name = "x" }
                                    Arg2 = ELiteral{Value = VInt 7}}) 
                        <| r "x && 7")
        Assert.True(isSucc (EBinary{Op = BOr
                                    Arg1 = EIdent{ Name = "x" }
                                    Arg2 = ELiteral{Value = VInt 7}}) 
                        <| r "x || 7")

        // Comparation
        Assert.True(isSucc (EBinary{Op = BEQ
                                    Arg1 = EIdent{ Name = "x" }
                                    Arg2 = ELiteral{Value = VInt 7}}) 
                        <| r "x == 7")
        Assert.True(isSucc (EBinary{Op = BNEQ
                                    Arg1 = EIdent{ Name = "x" }
                                    Arg2 = ELiteral{Value = VInt 7}}) 
                        <| r "x <> 7")
        Assert.True(isSucc (EBinary{Op = BGT
                                    Arg1 = EIdent{ Name = "x" }
                                    Arg2 = ELiteral{Value = VInt 7}}) 
                        <| r "x > 7")
        Assert.True(isSucc (EBinary{Op = BLT
                                    Arg1 = EIdent{ Name = "x" }
                                    Arg2 = ELiteral{Value = VInt 7}}) 
                        <| r "x < 7")
        Assert.True(isSucc (EBinary{Op = BNGT
                                    Arg1 = EIdent{ Name = "x" }
                                    Arg2 = ELiteral{Value = VInt 7}}) 
                        <| r "x <= 7")
        Assert.True(isSucc (EBinary{Op = BNLT
                                    Arg1 = EIdent{ Name = "x" }
                                    Arg2 = ELiteral{Value = VInt 7}}) 
                        <| r "x >= 7")

        // Mixed
        Assert.True(isSucc (EBinary{Op = BAdd
                                    Arg1 = EFunApp{
                                                Func = EIdent{ Name = "f" }
                                                Arg = EIdent{ Name = "x" }
                                           }
                                    Arg2 = EFunApp{
                                                Func = EIdent{ Name = "g" }
                                                Arg = EIdent{ Name = "y" }
                                           }}) 
                        <| r "f x + g y")
        Assert.True(isSucc (
                              EBinary
                               {Op = BOr;
                                Arg1 =
                                 EBinary
                                   {Op = BNGT;
                                    Arg1 =
                                     EBinary
                                       {Op = BAdd;
                                        Arg1 =
                                         EBinary
                                           {Op = BMul;
                                            Arg1 =
                                             EBinary
                                               {Op = BMul;
                                                Arg1 = ELiteral {Value = VInt 4;};
                                                Arg2 = EBinary {Op = BAdd;
                                                                Arg1 = ELiteral {Value = VInt 2;};
                                                                Arg2 = ELiteral {Value = VInt 3;};};};
                                            Arg2 = ELiteral {Value = VInt 7;};};
                                        Arg2 = ELiteral {Value = VInt 5;};};
                                    Arg2 = EIdent {Name = "x";};};
                                Arg2 =
                                 EBinary
                                   {Op = BAnd;
                                    Arg1 = EBinary {Op = BEQ;
                                                    Arg1 = ELiteral {Value = VBool false;};
                                                    Arg2 = EIdent {Name = "b";};};
                                    Arg2 =
                                     EBinary
                                       {Op = BOr;
                                        Arg1 =
                                         EBinary
                                           {Op = BLT;
                                            Arg1 = EIdent {Name = "k";};
                                            Arg2 = EBinary {Op = BDiv;
                                                            Arg1 = EIdent {Name = "n";};
                                                            Arg2 = ELiteral {Value = VInt 2;};};};
                                        Arg2 = EFunApp {Func = EIdent {Name = "f";};
                                                        Arg = ELiteral {Value = VChar 'x';};};};};}
                           ) 
                        <| r "4 * (2 + 3) * 7 + 5 <= x || false == b && (k < n / 2 || f 'x')")
