module Tests

open NUnit.Framework
open ParserCombinators.Core
open Types
open Parser.Literals
open Parser.Types
open Parser.Unary

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
        Assert.True(isSucc (TLambda(TChar, TBool)) <| r "char -> bool")
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