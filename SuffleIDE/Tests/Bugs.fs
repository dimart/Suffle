module Tests.Bugs

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

[<Test>]
let ``Keyword as beginning of name bug``() = 

    let mkIde x = EIdent{ Name = x }
    let mkIdt x = { EIdent.Name = x }
    let mkLit i = ELiteral{ Value = VInt i } 

    let r = run' dValue

    Assert.True(
        isSucc (
            DValue{
                Type = TInt
                Name = mkIdt "inside"
                Value = EBinary{
                            Op = BMul
                            Arg1 = EFunApp {
                                        Func = mkIde "f"
                                        Arg = mkIde "x"
                                    }
                            Arg2 = mkLit 2
                        }
            }
        ) <| r "val :: int inside = f x * 2")
        
    Assert.True(
        isSucc (
            DValue {
                Type = TInt
                Name = mkIdt "ifi"
                Value = mkLit 5
            }
        ) <| r "val :: int; ifi = 5;")

    let r = run' dFunction
    Assert.True(
        isSucc (
            DFunction
                {Type = TUnit;
                Name = {Name = "cased";};
                Body = ELambda {Arg = {Name = "x";};
                                Body = ELiteral { Value = VUnit } ;};}
        ) <| r "fun :: unit cased x = ()")

    Assert.True(
        isSucc (
            DFunction
                {Type = TLambda (TInt,TInt);
                Name = {Name = "letter";};
                Body = ELambda {Arg = {Name = "x";};
                                Body = EBinary {Op = BAdd;
                                                Arg1 = EIdent {Name = "x";};
                                                Arg2 = ELiteral {Value = VInt 1;};};};}
        ) <| r "fun :: int -> int letter x = x + 1")


[<Test>]
let ``Bracketed constructor in case .. of .. statement not parsing`` () = 

    let mkIde x = EIdent{ Name = x }
    let mkIdt x = { EIdent.Name = x }
    let mkLit i = ELiteral{ Value = VInt i } 

    let r = run' eCaseOf

    Assert.True(
        isSucc (
            ECaseOf{
                Matching = mkIde "x"
                Patterns =
                    [
                        (PCtor("Cons", [PCtor("Pair", [PIdent{ Name = "x" }; PIdent { Name = "y"}]); PIdent({ Name = "xs"})]), mkLit 1)
                        (PCtor("Cons", [PIdent {Name = "x" }; PCtor("Cons", [PIdent { Name = "y"}; PIdent {Name = "xs"}])]), mkLit 2)
                        (PCtor("Cons", [PIdent {Name = "x" }; PCtor("Cons", [PIdent { Name = "y"}; PIdent {Name = "xs"}])]), mkLit 0)
                        (PWildcard, mkLit -1)
                    ]
            }
        ) <| r """case x of
                | (Pair x y):xs -> 1
                | x:y:xs -> 2
                | x:(y:xs) -> 0
                | _ -> -1
                end"""
    )
