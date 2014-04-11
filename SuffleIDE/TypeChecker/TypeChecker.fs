module Suffle.TypeChecker

open Suffle.Specification.Types

type TypeContextItem = EIdent * Type

type TypeContext(ctx : TypeContextItem list) =
  class
    member x.Add(p : TypeContextItem) = 
        new TypeContext(p :: ctx)

    member x.Lookup(i : EIdent) =
        List.tryFind (fun (i' : EIdent, _) -> i'.Name = i.Name) ctx
  end

type TypeCheckerResult = bool * string

let typeValue tctx =
    function
    | VUnit    -> TUnit
    | VBool _  -> TBool
    | VChar _  -> TChar
    | VInt _   -> TInt
    | VCons(name, _) -> TUnit
    | VClosure(ctx, e) -> TUnit
    | _ -> failwith "Pattern match faliure"