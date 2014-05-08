module Suffle.TypeChecker

open Suffle.Specification.Types

type TypeContextItem = EIdent * Type

type TypeContext(ctx : TypeContextItem list) =
  class
    member x.Add(p : TypeContextItem) = 
        new TypeContext(p :: ctx)
                
    member x.Add(i : EIdent, t : Type) =
        x.Add((i, t))

    member x.Add(name : string, t : Type) =
        x.Add(({ Name = name }, t))

    member x.Lookup(i : EIdent) =
        List.tryFind (fun (i' : EIdent, _) -> i'.Name = i.Name) ctx

    member x.Lookup(name : string) =
        List.tryFind (fun (i' : EIdent, _) -> i'.Name = name) ctx
  end

type Reply = (Type option) * string

let tLiteral tctx =
    function
    | VUnit    -> TUnit
    | VBool _  -> TBool
    | VChar _  -> TChar
    | VInt _   -> TInt
    | _ -> failwith "Literal cannot be closure"