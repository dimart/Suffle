module Suffle.Parser

open FParsec             
open Suffle.Specification.Types
open Parser.Structures

let dtProcessor (p : Program) =
    let mkId name = { EIdent.Name = name }    
    let mkLd x b = ELambda{ Arg = x; Body = b }
    let mkDecl (type' : Type) (ctr : Ctor) =
        let dtn = match type' with | TDatatype (name, _) -> name | _ -> ""
        let (ctorName, types) = ctr
        match types with
        | [] -> DValue{ 
                        Type = type';
                        Name = mkId ctorName;
                        Value = ELiteral{ Value = VCtor(ctorName, []) };
                      }
        | t::ts ->
            let args = List.init (types.Length) (fun i -> mkId <| "arg" + i.ToString())
            let body = 
                List.foldBack (fun x acc -> mkLd x acc) 
                              args 
                              (ECtor{ DatatypeName = dtn; CtorName = ctorName; Args = args }) 
            let ftype = List.fold (fun acc x -> TLambda(x, acc)) t ts
            DValue{ Type = TLambda(ftype, type'); Name = mkId ctorName; Value = body }
                 
    let rec proc (p : Program) : Program =
        match p with 
        | [] -> []
        | d :: ds ->
            let res = match d with
                      | DValue _ | DFunction _ -> proc ds
                      | DDatatype dt ->
                          let tlist = List.map TVar dt.Params
                          let type' = TDatatype(dt.Name.Name, tlist)
                          let newDecls = List.map (mkDecl type') dt.Ctors
                          newDecls @ proc ds
            d :: res
    proc p 

let program = declarations .>> eof |>> dtProcessor

let parse (s : string) =
    let reply = run program s
    match reply with
    | Success(res, _, _) -> res
    | Failure(errMsg, _, _) -> printfn "ERROR:\n%A" errMsg; []

