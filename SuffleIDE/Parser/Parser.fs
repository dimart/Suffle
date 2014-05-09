module Suffle.Parser

open FParsec             
open Suffle.Specification.Types
open Parser.Structures

let dtProcessor (p : Program) =
    let mkId name = { EIdent.Name = name }    
    let mkLd x b = ELambda{ Arg = x; Body = b }
    let mkDecl (type' : Type) (ctr : Ctor) =
        let (ctorName, types) = ctr
        match types with
        | [] -> DValue{ 
                        Type = type';
                        Name = mkId ctorName;
                        Value = ELiteral{ Value = VCtor(ctorName, []) };
                      }
        | t::ts ->
            let arg0 = mkId <| "arg0"
            let args = List.init (ts.Length) (fun i -> mkId <| "arg" + (i + 1).ToString())
            let body = List.fold (fun acc x -> mkLd x acc) (ECtor{ Args = args }) args
            let ftype = List.fold (fun acc x -> TLambda(x, acc)) t ts
            DFunction{ Type = TLambda(ftype, type'); Name = mkId ctorName; Arg = arg0; Body = body }
                 
    let rec proc (p : Program) : Program =
        match p with 
        | [] -> []
        | d :: ds ->
            let res = match d with
                      | DValue _ | DFunction _ -> proc ds
                      | DDatatype dt ->
                          let tlist = List.map TVar dt.Params
                          let type' = TDatatype(dt.Name.Name, tlist)
                          List.map (mkDecl type') dt.Ctors
            d :: res
    proc p 

let parse (s : string) =
    let reply = run (program .>> eof |>> dtProcessor) s
    match reply with
    | Success(res, _, _) -> res
    | Failure(errMsg, _, _) -> printfn "ERROR:\n%A" errMsg; []

