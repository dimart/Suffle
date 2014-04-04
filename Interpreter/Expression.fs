module Interpreter.Expression

open System.Collections.Generic
open Types

//  List of exceptions.
//  Exception consists of err message and line number
exception VariableNotFoundException of string * int
exception TypeMismatchException of string * int

let lineNum = 0

let vars = new Dictionary<string, Value list>()
let funcs = new Dictionary<string, EFunApp list>()

//  Evaluate expression
let rec evalExpr (expr: Expression) = 
    //  Return binary operator
    let retNumBin (op: BinaryOp): int -> int -> int = 
        match op with
        | BAdd -> (+)
        | BSub -> (-)
        | BDiv -> (/)
        | BMul -> (*)
        | BEQ -> (=)
        | BNEQ -> (<>)
        | BGT -> (>)
        | BLT -> (<)
        | BNGT -> (<=)
        | BNLT -> (>=)
        | _ -> raise (TypeMismatchException ("Wrong operation", lineNum))

    //  Evaluate type expression
    //  ETyped
    let evalType (x: Expression) = 
        //toWrite
        VUnit

    //  Eval identifier
    //  EIdentifier
    let evalIdent (id: EIdent) = 
//            try
        try 
            match vars.[id.Name] with
            | a :: _ -> a
            | [] -> raise <| KeyNotFoundException ()
        with
        | :? KeyNotFoundException -> raise (VariableNotFoundException (id.Name, lineNum))
//            with
//                //  if identifier isn't defined - we fail to interpret (? - mb need to do on top level)
//                | VariableNotFoundException name -> printfn "Variable \"%A\" not found" name

    //  Eval literal
    let evalLiteral (liter: ELiteral) = 
        liter.Value
        
    //  Eval "if" statement 
    let evalIf (stmnt: EIfElse) = 
        match (evalExpr stmnt.Cond) with
        | VBool cond -> evalExpr (if cond then stmnt.OnTrue else stmnt.OnFalse)
        | _ -> raise (TypeMismatchException ("Expected type \'bool\'.", lineNum))
        
    //  Eval "let" stmnt
    let evalLet (stmnt: ELetIn) = 
        match stmnt.Binding with
        | DValue x -> vars.TryGetValue x.Name.Name = evalExpr x.Value
        //  May be need to pass closure or lambda - dunno now, check on it later
        | DFunction x -> funcs.Add (x.Name.Name, x) |> ignore
        | _ -> ()
            
    //  Eval unary stmnt
    let evalUnary (stmnt: EUnary) = 
        let evaluated = evalExpr stmnt.Arg vars funcs
        match stmnt.Op with
        | UNeg ->   match evaluated with
                    | VInt x -> VInt -x
                    | _ -> raise (TypeMismatchException ("Int expected", lineNum))
        | UNot ->   match evaluated with
                    | VBool x -> VBool <| not x
                    | _ -> raise (TypeMismatchException ("Bool expected", lineNum))

    //  Eval binary statement
    let evalBinary (stmnt: EBinary) = 
        let evaluated1 = repack <| evalExpr stmnt.Arg1 vars funcs
        let evaluated2 = repack <| evalExpr stmnt.Arg2 vars funcs
        match evaluated1 with
        | Some x -> match evaluated2 with
                    | Some y -> ()
                            
(*                            
| ELambda of ELambda
| EFunApp of EFunApp
| ECaseOf of ECaseOf
*)

    match expr with
    | EIdentifier -> evalIdent expr
    | ELiteral -> evalLiteral expr
    | EIfElse -> evalIf expr
    | ELetIn -> evalLet expr
    

let a = Expr (EBinary {Op = BAdd; Arg1 = ELiteral {Value = VInt 5}; Arg2 = ELiteral {Value = VInt 7}})
//eval a Map.empty Map.empty
