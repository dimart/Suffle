module Tests.Parser
               
open Suffle.Specification.Syntax
open Suffle.Specification.Keywords   
open Suffle.Specification.Types
open Suffle.Specification.OperationPriority
                     
open FParsec
open NUnit.Framework

open Parser.Literals
open Parser.Types
open Parser.Pattern
open Parser.Structures
open Suffle.Parser

let run' p s = run (p .>> eof) s

let isSucc (value : 'a) =
    function 
    | FParsec.CharParsers.Success(v, _, _) -> v = value
    | _ -> false

let isFail =
    function
    | FParsec.CharParsers.Failure _ -> true
    | _ -> false

[<TestFixture>]
type ``Literal parsing``() =
    
    [<Test>]
    member x.``Unit`` () =
        Assert.True(isSucc VUnit <| run' lUnit "()")
        Assert.True(isFail <| run' lUnit "(]")
        Assert.True(isFail <| run' lUnit "[)")
        Assert.True(isFail <| run' lUnit "(())")

    [<Test>]
    member x.``Bool`` () =
        Assert.True(isSucc (VBool true) <| run' lBool "true")
        Assert.True(isSucc (VBool false) <| run' lBool "false")

        Assert.True(isFail <| run' lBool "True")
        Assert.True(isFail <| run' lBool "False")

    [<Test>]
    member x.``Char`` () =
        let r = run' lChar
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
        let r = run' lInt
        Assert.True(isSucc (VInt 42) <| r "42")
        Assert.True(isSucc (VInt 42) <| r "+42")
        Assert.True(isSucc (VInt -7) <| r "-7")
        Assert.True(isSucc (VInt 0) <| r "0")
        Assert.True(isSucc (VInt 0) <| r "00")
        Assert.True(isSucc (VInt 123) <| r "0123")
        Assert.True(isSucc (VInt 0) <| r "-0")
        
        Assert.True(isFail <| r "3.14")

[<TestFixture>]
type ``Type parsing``() =
    
    [<Test>]
    member x.``Unit`` () =
        Assert.True(isSucc TUnit <| run' tUnit "unit")

    [<Test>]
    member x.``Bool`` () =
        Assert.True(isSucc TBool <| run' tBool "bool")

    [<Test>]
    member x.``Char`` () =
        Assert.True(isSucc TChar <| run' tChar "char")

    [<Test>]
    member x.``Int`` () =
        Assert.True(isSucc TInt <| run' tInt "int")

    [<Test>]
    member x.``Var`` () =
        let r = run' tVar
        Assert.True(isSucc (TVar "'a") <| r "'a")
        Assert.True(isSucc (TVar "'T") <| r "'T")
        Assert.True(isSucc (TVar "'Type") <| r "'Type")
        
        Assert.True(isFail <| r "A")
        Assert.True(isFail <| r "a")
        Assert.True(isFail <| r "_type")

    [<Test>]
    member x.``Datatype`` () =
        let r = run' tDatatype
        Assert.True(isSucc (TDatatype ("A", [])) <| r "A")
        Assert.True(isSucc (TDatatype ("Tt", [])) <| r "Tt")
        Assert.True(isSucc (TDatatype ("Option", [])) <| r "Option")
        Assert.True(isSucc (TDatatype ("A1", [])) <| r "A1")

        Assert.True(isFail <| r "A'")
        Assert.True(isFail <| r "A_B")
        
    [<Test>]
    member x.``Lambda`` () =
        let r = run' tLambda
        Assert.True(isSucc (TLambda(TInt, TInt)) <| r "int -> int")
        Assert.True(isSucc (TLambda(TChar, TBool)) <| r "char -> bool")
        Assert.True(isSucc (TLambda(TVar "'a", TVar "'b")) <| r "'a -> 'b")
        
        Assert.True(isSucc (TLambda(TInt, TLambda(TInt, TInt))) <| r "int -> int -> int")
        Assert.True(isSucc (TLambda(TLambda(TInt, TInt), TInt)) <| r "(int -> int) -> int")
        Assert.True(isSucc (TLambda(TInt, TLambda(TInt, TInt))) <| r "int -> (int -> int)")
        Assert.True(isSucc (TLambda(TDatatype ("A", []), TBool)) <| r "A -> bool")
        Assert.True(isSucc (TLambda(TDatatype ("A", []), TDatatype ("B", []))) <| r "A -> B")
        Assert.True(isSucc (TLambda(
                                TInt,
                                TLambda(
                                    TLambda(TChar, TBool),
                                    TLambda(
                                        TLambda(TVar "'a", TDatatype ("A", [])),
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
        let r = run' pIdent
        Assert.True(isSucc (PIdent{Name = "function1"}) <| r "function1")
        Assert.True(isSucc (PIdent{Name = "_arg1"}) <| r "_arg1")
        Assert.True(isSucc (PIdent{Name = "g'"}) <| r "g'")

        Assert.True(isFail <| r "1a")
        Assert.True(isFail <| r "a-b")
        Assert.True(isFail <| r "'a")
        Assert.True(isFail <| r "@x")
        Assert.True(isFail <| r "x@")

    [<Test>]
    member x.``Literal`` () =
        let r = run' pLiteral
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
        Assert.True(isSucc PWildcard <| run' pWildcard "_")

    [<Test>]
    member x.``Constructor`` () =
        let r = run' pCtor
        Assert.True(isSucc (PCtor("A", [PLiteral{Value = VInt 123}; PIdent{Name = "a"}])) <| r "A   123 a")
        Assert.True(isSucc (PCtor("A", [PLiteral{Value = VChar 'x'}])) <| r "A 'x'")
        Assert.True(isSucc (PCtor("A", [PWildcard])) <| r "A _")
        Assert.True(isSucc (PCtor("A", [PLiteral{Value = VBool true}])) <| r "A true")
        Assert.True(isSucc (PCtor("X", [PLiteral{Value = VBool false}])) <| r "X(false)")
        Assert.True(isSucc (PCtor("X", [PIdent{Name = "x"}])) <| r "X  (x)")
        Assert.True(isSucc (PCtor("X", [PIdent{Name = "y"}])) <| r "X y")


[<TestFixture>]
type ``Expression parsing``() =

    let mkIde x = EIdent{ Name = x }
    let mkIdt x = { EIdent.Name = x }
    let mkLit i = ELiteral{ Value = VInt i } 
    
    [<Test>]
    member x.``Binary`` () =      
        let r = run' eBinary
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
                                            EBinary {Op = BMul;
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
                                         EBinary {Op = BLT;
                                                  Arg1 = EIdent {Name = "k";};
                                                  Arg2 = EBinary {Op = BDiv;
                                                                  Arg1 = EIdent {Name = "n";};
                                                                  Arg2 = ELiteral {Value = VInt 2;};};};
                                        Arg2 = EFunApp {Func = EIdent {Name = "f";};
                                                        Arg = ELiteral {Value = VChar 'x';};};};};}
                            
                           ) 
                        <| r "4 * (2 + 3) * 7 + 5 <= x || false == b && (k < n / 2 || f 'x')")
    [<Test>]
    member x.``Unary`` () =      
        let r = run' eUnary
        Assert.True(isSucc (EUnary{ Op = UNeg
                                    Arg = ELiteral{ Value = VInt 5 }
                           }) <| r "-(5)")
        Assert.True(isSucc (EUnary{ Op = UNeg
                                    Arg = EIdent{ Name = "x" }
                           }) <| r "-x")
        Assert.True(isSucc (EUnary{ Op = UNot
                                    Arg = EIdent{ Name = "f" }
                           }) <| r "!f")
        Assert.True(isSucc (EUnary{ Op = UNot
                                    Arg = EBinary { Op = BLT
                                                    Arg1 = EIdent{ Name = "a" }
                                                    Arg2 = ELiteral{ Value = VInt 10 }
                                               }
                           }) <| r "!(a < 10)")

    [<Test>]
    member x.``Lambda`` () =      
        let r = run' eLambda
        Assert.True(isSucc (ELambda{ Arg = {EIdent.Name = "x"}
                                     Body = EBinary{Op = BAdd
                                                    Arg1 = EIdent{ Name = "x" }
                                                    Arg2 = ELiteral{Value = VInt 1}
                                            }
                                   })
                      <| r "\x -> x + 1")
        Assert.True(isSucc (ELambda{ Arg = {EIdent.Name = "x"}
                                     Body = ELambda{ Arg = {EIdent.Name = "y"}
                                                     Body = EBinary{Op = BMul
                                                                    Arg1 = EIdent{ Name = "x" }
                                                                    Arg2 = EIdent{ Name = "y" }
                                                            }
                                            }
                                   })
                      <| r "\ x y -> x * y")

        Assert.True(isSucc (ELambda{ Arg = {EIdent.Name = "x"}
                                     Body = ELambda{ Arg = {EIdent.Name = "y"}
                                                     Body = EBinary{Op = BMul
                                                                    Arg1 = EIdent{ Name = "x" }
                                                                    Arg2 = EIdent{ Name = "y" }
                                                            }
                                            }
                                   })
                      <| r "\x -> \y -> x * y")

    [<Test>]
    member x.``Function Applying`` () =      
        let r = run' eFunApp
        Assert.True(isSucc 
                     (EFunApp{ Func = EIdent{ Name = "f" }
                               Arg = EIdent{ Name = "x" }
                                   
                      }) <| r "f x")
        Assert.True(isSucc 
                     (EFunApp{ Func = EFunApp{ Func = EIdent{ Name = "f" }
                                               Arg = EIdent{ Name = "x" }
                                             }
                               Arg = EIdent{ Name = "y" }
                                   
                      }) <| r "f x y")
        Assert.True(isSucc 
                     (EFunApp{ Func = EFunApp{ Func = EIdent{ Name = "f" }
                                               Arg = EIdent{ Name = "x" }
                                             }
                               Arg = EIdent{ Name = "y" }
                                   
                      }) <| r "(f x) y")
        Assert.True(isSucc 
                     (EFunApp{ Func = EIdent{ Name = "f" }
                               Arg = EFunApp{ Func = EIdent{ Name = "x" }
                                              Arg = EIdent{ Name = "y" }
                                            }
                      }) <| r "f (x y)")

    [<Test>]
    member x.``If Then Else`` () =      
        let r = run' eIfElse
        let mkId x = EIdent{ Name = x }
        Assert.True(isSucc (
                                EIfElse{
                                    Cond = mkId "a"
                                    OnTrue = mkId "b"
                                    OnFalse = mkId "c"
                                }
                           ) <| r "if a then b else c end"
                   )
        Assert.True(isSucc (
                                EIfElse{
                                    Cond = EBinary{
                                            Op = BLT
                                            Arg1 = mkId "x"
                                            Arg2 = ELiteral{ Value = VInt 0 } 
                                           }
                                    OnTrue = ELiteral{ Value = VInt 0 } 
                                    OnFalse = EFunApp { Func = mkId "g"
                                                        Arg = mkId "x"
                                                      }
                                }
                           ) <| r "if x < 0 then 0 else g x end"
                   )
                   
    [<Test>]
    member x.``Let in`` () =      
        let r = run' eLetIn
        let mkIde x = EIdent{ Name = x }
        let mkIdt x = { EIdent.Name = x }
        let mkLit i = ELiteral{ Value = VInt i } 
        Assert.True(isSucc (
                                ELetIn{
                                    Binding = DValue{ Type = TInt; Name = mkIdt "x"; Value = mkLit 5 }
                                    Body = EBinary{ Op = BAdd; Arg1 = mkIde "x"; Arg2 = mkLit 1 }
                                }
                           ) <| r "let def val :: int; x = 5; in x + 1 end"
                   )
        Assert.True(isSucc (
                                ELetIn
                                  {Binding = DValue {Type = TInt;
                                                     Name = {Name = "x";};
                                                     Value = ELiteral {Value = VInt 5;};};
                                   Body =
                                    ELetIn
                                      {Binding =
                                        DFunction
                                          {Type = TLambda (TInt,TInt);
                                           Name = {Name = "f";};
                                           Body =
                                            ELambda {Arg = {Name = "x";};
                                                     Body = EBinary {Op = BAdd;
                                                                     Arg1 = EIdent {Name = "x";};
                                                                     Arg2 = ELiteral {Value = VInt 1;};};};};
                                       Body = EFunApp {Func = EIdent {Name = "f";};
                                                       Arg = EIdent {Name = "x";};};};}
                           ) <| r """let 
                                        def val :: int
                                        x = 5

                                        def fun :: int -> int
                                        f x = x + 1
                                     in 
                                        f x
                                     end"""
                   )
                      
    [<Test>]
    member x.``Case Of`` () =      
        let r = run' eCaseOf
        Assert.True(isSucc (
                                ECaseOf{
                                    Matching = mkIde "x"
                                    Patterns =
                                        [
                                            (PLiteral{ Value = VInt 1 }, mkLit 0)
                                            (PCtor("A", [PIdent{ Name = "y" }]), mkLit 1)
                                            (PIdent{ Name = "z" }, mkLit 2)
                                            (PWildcard, mkLit -1)
                                        ]
                                }
                           ) <| r """case x of
                                  | 1 -> 0
                                  | A y -> 1
                                  | z -> 2
                                  | _ -> -1
                                  end"""
                   )

[<TestFixture>]
type ``Declaration parsing``() =

    let mkIde x = EIdent{ Name = x }
    let mkIdt x = { EIdent.Name = x }
    let mkLit i = ELiteral{ Value = VInt i } 

    [<Test>]
    member x.``Value`` () =      
        let r = run' dValue
        Assert.True(isSucc (
                                DValue{
                                    Type = TInt
                                    Name = mkIdt "x"
                                    Value = mkLit 5
                                }
                           ) <| r "def val :: int; x = 5;")
        Assert.True(isSucc (
                                DValue{
                                    Type = TInt
                                    Name = mkIdt "abc1"
                                    Value = EBinary{
                                                Op = BMul
                                                Arg1 = EFunApp {
                                                            Func = mkIde "f"
                                                            Arg = mkIde "x"
                                                       }
                                                Arg2 = mkLit 2
                                            }
                                }
                           ) <| r "def val :: int abc1 = f x * 2")
    [<Test>]
    member x.``Function`` () =      
        let r = run' dFunction
        Assert.True(isSucc (
                                DFunction
                                  {Type = TLambda (TInt,TInt);
                                   Name = {Name = "f";};
                                   Body = ELambda {Arg = {Name = "x";};
                                                   Body = EBinary {Op = BAdd;
                                                                   Arg1 = EIdent {Name = "x";};
                                                                   Arg2 = ELiteral {Value = VInt 1;};};};}
                           ) <| r "def fun :: int -> int f x = x + 1")
        Assert.True(isSucc (
                                DFunction
                                  {Type = TLambda (TInt,TLambda (TInt,TInt));
                                   Name = {Name = "f";};
                                   Body =
                                    ELambda {Arg = {Name = "x";};
                                             Body = ELambda {Arg = {Name = "y";};
                                                             Body = EBinary {Op = BAdd;
                                                                             Arg1 = EIdent {Name = "x";};
                                                                             Arg2 = EIdent {Name = "y";};};};};}
                           ) <| r "def fun :: int -> int -> int f x y = x + y")
        Assert.True(isSucc (
                                DFunction
                                  {Type = TLambda (TInt,TLambda (TInt,TInt));
                                   Name = {Name = "f";};
                                   Body =
                                    ELambda {Arg = {Name = "x";};
                                             Body = ELambda {Arg = {Name = "y";};
                                                             Body = EBinary {Op = BAdd;
                                                                             Arg1 = EIdent {Name = "x";};
                                                                             Arg2 = EIdent {Name = "y";};};};};}
                           ) <| r "def fun :: int -> int -> int f x = \y -> x + y")
        Assert.True(isSucc (
                                DFunction
                                  {Type = TUnit;
                                   Name = {Name = "f";};
                                   Body =
                                    ELambda
                                      {Arg = {Name = "a";};
                                       Body =
                                        ELambda
                                          {Arg = {Name = "b";};
                                           Body =
                                            ELambda
                                              {Arg = {Name = "c";};
                                               Body =
                                                ELambda {Arg = {Name = "d";};
                                                         Body = ELambda {Arg = {Name = "e";};
                                                                         Body = ELiteral {Value = VUnit;};};};};};};}
                           ) <| r "def fun :: unit f a b c d e = ()")
                           
    [<Test>]
    member x.``Datatype`` () =      
        let r = run' dDatatype
        Assert.True(isSucc (
                               DDatatype
                                {Name = {Name = "A";};
                                 Params = ["'a"; "'b"];
                                 Ctors =
                                    [
                                        ("X", []); ("Y", [TInt]); ("Z", [TLambda (TBool,TUnit)]);
                                        ("W", [TInt; TVar "'a"]); ("U", [TDatatype ("T1",[]); TDatatype ("T2",[])]);
                                        ("M", [TDatatype ("List",[TVar "'b"])])
                                    ];
                                }
                           ) <| r """datatype A 'a 'b = 
                                     | X 
                                     | Y int
                                     | Z (bool -> unit)
                                     | W int 'a
                                     | U T1 T2
                                     | M (List 'b)
                                     end""")
                                   
                                   
                                 
[<TestFixture>]
type ``Program parsing``() =

    let mkIde x = EIdent{ Name = x }
    let mkIdt x = { EIdent.Name = x }
    let mkLit i = ELiteral{ Value = VInt i } 
    
    [<Test>]
    member x.``Program1`` () =      
        let r = run' program
        Assert.True(isSucc (
                             [DDatatype
                                 {Name = {Name = "List";};
                                  Params = ["'a"];
                                  Ctors =
                                   [("Cons", [TVar "'a"; TDatatype ("List",[TVar "'a"])]); ("Nil", [])];};
                               DFunction
                                 {Type =
                                   TLambda
                                     (TLambda (TDatatype ("List",[TVar "'a"]),TVar "'a"),
                                      TDatatype ("List",[TVar "'a"]));
                                  Name = {Name = "Cons";};
                                  Body =
                                   ELambda
                                     {Arg = {Name = "arg0";};
                                      Body =
                                       ELambda
                                         {Arg = {Name = "arg1";};
                                          Body = ECtor {CtorName = "Cons";
                                                        Args = [{Name = "arg0";}; {Name = "arg1";}];};};};};
                               DValue {Type = TDatatype ("List",[TVar "'a"]);
                                       Name = {Name = "Nil";};
                                       Value = ELiteral {Value = VCtor ("Nil",[]);};};
                               DFunction
                                 {Type = TLambda (TVar "'a",TDatatype ("List",[TVar "'a"]));
                                  Name = {Name = "mk";};
                                  Body =
                                   ELambda
                                     {Arg = {Name = "x";};
                                      Body = EFunApp {Func = EFunApp {Func = EIdent {Name = "Cons";};
                                                                      Arg = EIdent {Name = "x";};};
                                                      Arg = EIdent {Name = "Nil";};};};};
                               DFunction
                                 {Type = TLambda (TDatatype ("List",[TVar "'a"]),TInt);
                                  Name = {Name = "len";};
                                  Body =
                                   ELambda
                                     {Arg = {Name = "list";};
                                      Body =
                                       ECaseOf
                                         {Matching = EIdent {Name = "list";};
                                          Patterns =
                                           [(PCtor ("Nil",[]), ELiteral {Value = VInt 0;});
                                            (PCtor ("Cons",[PWildcard; PIdent {Name = "rest";}]),
                                             EBinary {Op = BAdd;
                                                      Arg1 = EFunApp {Func = EIdent {Name = "len";};
                                                                      Arg = EIdent {Name = "rest";};};
                                                      Arg2 = ELiteral {Value = VInt 1;};})];};};};
                               DFunction
                                 {Type =
                                   TLambda (TDatatype ("List",[TVar "'a"]),TDatatype ("List",[TVar "'a"]));
                                  Name = {Name = "rev";};
                                  Body =
                                   ELambda
                                     {Arg = {Name = "xs";};
                                      Body =
                                       ELetIn
                                         {Binding =
                                           DFunction
                                             {Type =
                                               TLambda
                                                 (TDatatype ("List",[TVar "'a"]),
                                                  TLambda
                                                    (TDatatype ("List",[TVar "'a"]),
                                                     TDatatype ("List",[TVar "'a"])));
                                              Name = {Name = "rev'";};
                                              Body =
                                               ELambda
                                                 {Arg = {Name = "xs";};
                                                  Body =
                                                   ELambda
                                                     {Arg = {Name = "rest";};
                                                      Body =
                                                       ECaseOf
                                                         {Matching = EIdent {Name = "rest";};
                                                          Patterns =
                                                           [(PCtor ("Nil",[]), EIdent {Name = "xs";});
                                                            (PCtor
                                                               ("Cons",
                                                                [PIdent {Name = "x";};
                                                                 PIdent {Name = "rs";}]),
                                                             EFunApp
                                                               {Func =
                                                                 EFunApp
                                                                   {Func = EIdent {Name = "rev'";};
                                                                    Arg =
                                                                     EFunApp
                                                                       {Func =
                                                                         EFunApp
                                                                           {Func =
                                                                             EIdent {Name = "Cons";};
                                                                            Arg = EIdent {Name = "x";};};
                                                                        Arg = EIdent {Name = "xs";};};};
                                                                Arg = EIdent {Name = "rs";};})];};};};};
                                          Body = EFunApp {Func = EFunApp {Func = EIdent {Name = "rev'";};
                                                                          Arg = EIdent {Name = "Nil";};};
                                                          Arg = EIdent {Name = "xs";};};};};};
                               DValue {Type = TDatatype ("List",[TInt]);
                                       Name = {Name = "xs";};
                                       Value = EFunApp {Func = EIdent {Name = "mk";};
                                                        Arg = ELiteral {Value = VInt 5;};};}]
                           ) <| r """

datatype List 'a = 
| Cons 'a (List 'a)
| Nil
end  

def fun :: 'a -> (List 'a)
mk x = [x]

def fun :: (List 'a) -> int
len list = 
    case list of
    | [] -> 0
    | _ : rest -> len rest + 1
    end

def fun :: (List 'a) -> (List 'a)
rev xs =
    let 
        def fun :: (List 'a) -> (List 'a) -> (List 'a)
        rev' xs rest = 
            case rest of
            | [] -> xs
            | x : rs -> rev' (x : xs) rs
            end
    in
        rev' [] xs
    end

def val :: (List int)
xs = mk 5     
    
                           """)                     
            

[<TestFixture>]
type ``Sugar of Lists``() =

    [<Test>]
    member x.``Pattern-matching`` () =      
        let r = run' eCaseOf
        Assert.True(isSucc (
                                ECaseOf
                                   {Matching = EIdent {Name = "x";};
                                    Patterns =
                                     [(PCtor ("Cons",[PIdent {Name = "y";}; PIdent {Name = "ys";}]),
                                       ELiteral {Value = VUnit;});
                                      (PCtor
                                         ("Cons",
                                          [PIdent {Name = "x";};
                                           PCtor ("Cons",[PIdent {Name = "y";}; PIdent {Name = "xs";}])]),
                                       ELiteral {Value = VUnit;});
                                      (PCtor ("Nil",[]), ELiteral {Value = VUnit;});
                                      (PCtor ("Cons",[PIdent {Name = "x";}; PCtor ("Nil",[])]),
                                       ELiteral {Value = VUnit;});
                                      (PCtor ("Cons",[PLiteral {Value = VInt 1;}; PCtor ("Nil",[])]),
                                       ELiteral {Value = VUnit;});
                                      (PCtor
                                         ("Cons",
                                          [PIdent {Name = "x";};
                                           PCtor
                                             ("Cons",
                                              [PLiteral {Value = VInt 2;};
                                               PCtor
                                                 ("Cons",[PLiteral {Value = VInt 3;}; PCtor ("Nil",[])])])]),
                                       ELiteral {Value = VUnit;});
                                      (PCtor ("Cons",[PIdent {Name = "x";}; PCtor ("Nil",[])]),
                                       ELiteral {Value = VUnit;})];}
                           ) <| r """case x of
                                    | y : ys -> ()
                                    | x : y : xs -> ()
                                    | [] -> ()
                                    | [x] -> ()
                                    | [1] -> ()
                                    | [x, 2, 3] -> ()
                                    | x : [] -> ()
                                    end"""
                )
