module Translator

open Suffle.Specification.Types

//let sps n = String.replicate n " "

let bin2str =
    function
    // Arithmetics
    | BAdd -> "+"
    | BSub -> "-"
    | BDiv -> "/"
    | BMul -> "*"
    | BMod -> "%"
    // Logic
    | BAnd -> "&&"
    | BOr -> "||"
    // Comparation
    | BEQ -> "="
    | BNEQ -> "<>"
    | BGT  -> ">"
    | BLT  -> "<"
    | BNGT -> "<="
    | BNLT -> ">="

let rec transValue (v : Value) =
    match v with
    | VUnit -> "()"
    | VBool b -> if b then "true" else "false"
    | VChar c -> c.ToString()
    | VInt i -> i.ToString()
    | VCtor(name, vlist) -> name + (List.fold (fun acc v -> acc + " (" + transValue v + ")") "" vlist)
    | _ -> failwith "Unexpected value to translate"

and transPattern (p : Pattern) =
    match p with
    | PIdent i -> i.Name
    | PLiteral lit -> transValue lit.Value
    | PCtor (name, plist) ->
        let ps =
            match plist with 
            | [] -> ""
            | pp::ps -> 
                "(" + transPattern pp + 
                (List.fold (fun acc p -> acc + ", " + transPattern p) "" ps) + 
                ")"
        name + ps 
    | PWildcard -> "_" 

and transExpr (n : int) (expr : Expression) =
    let te = transExpr (n+1)
    let res =
        match expr with
        | EIdent e -> e.Name
        | ELiteral e -> transValue e.Value
        | EIfElse e -> 
            "if (" + te e.Cond + 
            ") then (" + te e.OnTrue + 
            ") else (" + te e.OnFalse + ")"
        | ELetIn e -> 
            transDecl (n + 1) e.Binding +
            " in " + te e.Body
        | EUnary e -> (match e.Op with UNeg -> "-" | UNot -> "not ") + te e.Arg
        | EBinary e -> te e.Arg1 + " " + bin2str e.Op + " " + te e.Arg2    
        | ELambda e -> "fun " + e.Arg.Name + " -> " + te e.Body
        | EFunApp e -> te e.Func + " " + te e.Arg
        | ECaseOf e -> 
            "match " + te e.Matching + " with " + 
             (List.fold (fun acc (p, b) -> acc + "| " + transPattern p + " -> " + te b) "" e.Patterns)
        | ECtor e ->
            let ps = 
                match e.Args with
                | [] -> ""
                | p::ps -> 
                    "(" + p.Name + 
                    (List.fold (fun acc (i : EIdent) -> acc + ", " + i.Name) "" ps) + 
                    ")" 
            e.DatatypeName + "." + e.CtorName + ps
            
    "(" + res + ")"

and transDValue (n : int) (v : DValue) =
    "let " + v.Name.Name + " = " + transExpr (n + 1) v.Value

and transDFunction (n : int) (f : DFunction) =
    "let rec " + f.Name.Name + " = " + transExpr (n + 1) f.Body

and transType (t : Type) = 
    match t with
    | TUnit -> "unit"
    | TBool -> "bool"
    | TChar -> "char"
    | TInt  -> "int"
    | TLambda(t1, t2) -> transType t1 + " -> " + transType t2
    | TDatatype (name, tlist) ->
        let tps = 
            match tlist with
            | [] -> ""
            | tp::tps -> 
                "<" + transType tp + 
                (List.fold (fun acc tp' -> acc + "," + transType tp') "" tps) + 
                ">" 
        name + tps
    | TVar v -> v

and transDDatatype (n : int) (dt : DDatatype) =
    let tps = 
        match dt.Params with
        | [] -> ""
        | tp::tps -> "<" + tp + (List.fold (fun acc tp' -> acc + "," + tp') "" tps) + ">" 
    "type " + dt.Name.Name + tps +
    " = " + (List.fold 
                (fun acc (cname, t) ->
                    let t' = 
                        match t with
                        | [] -> ""
                        | tt::ts -> 
                            " of " + transType tt + 
                            (List.fold (fun acc tt' -> acc + " * " + transType tt') "" ts) 
                    acc + "| " + cname + t' + " "
                ) "" dt.Ctors)

and transDecl (n : int) (d : Declaration) =
    match d with
    | DValue v -> transDValue n v
    | DFunction f -> transDFunction n f
    | DDatatype dt -> transDDatatype n dt

let transProgram (p : Program) =
    List.fold (fun text d -> text + "\n\n" + (transDecl 0 d)) "" p
