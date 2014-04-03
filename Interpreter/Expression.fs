module Interpreter.Expression

open System.Collections.Generic
open Types
open Declaration

//  List of exceptions.
//  Exception consists of err message and line number
exception VariableNotFoundException of string * int
exception TypeMismatchException of string * int

let lineNum = 0

let eval (program: ASTNode) = 

    //  Evaluate expression
    let rec evalExpr (expr: Expression) (vars: Map<string, Value>) (funcs: Map<string, DFunction>) = 
        //  Repack to option
        let repack (var: Value): obj option = 
            match var with
            | VUnit -> Some (() :> obj)
            | VBool x -> Some (x :> obj)
            | VChar x -> Some (x :> obj)
            | VInt x -> Some (x :> obj)
            | VFloat x -> Some (x :> obj)
            | VCons (x, y) -> Some ((x, y) :> obj)
            | VClosure (x, y) -> Some ((x, y) :> obj)

        //  Pack to Value
        let pack (x: obj) = 
            match x with
            | :? unit -> VUnit
            | :? bool -> VBool (x :?> bool)
            | :? char -> VChar (x :?> char)
            | :? int -> VInt (x :?> int)
            | :? float -> VFloat (x :?> float)
            | :? (string * Value) -> VCons (x :?> string * Value)
            | :? (Context * Expression) -> VClosure (x :?> Context * Expression)
            | _ -> raise (TypeMismatchException ("Wrong type packing", lineNum))
        
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
            match (vars.TryFind id.Name) with
            | Some a -> a
            | None -> raise (VariableNotFoundException (id.Name, lineNum))
//            with
//                //  if identifier isn't defined - we fail to interpret (? - mb need to do on top level)
//                | VariableNotFoundException name -> printfn "Variable \"%A\" not found" name

        //  Eval literal
        let evalLiteral (liter: ELiteral) = 
            liter.Value
        
        //  Eval "if" statement 
        let evalIf (stmnt: EIfElse) = 
            match (evalExpr stmnt.Cond vars funcs) with
            | VBool cond -> evalExpr (if cond then stmnt.OnTrue else stmnt.OnFalse) vars funcs
            | _ -> raise (TypeMismatchException ("Excpected type \'bool\'.", lineNum))
        
        //  Eval "let" stmnt
        let evalLet (stmnt: ELetIn) = 
            match stmnt.Binding with
            | DValue x -> vars.Add (x.Name, x.Value)
            //  May be need to pass closure or lambda - dunno now, check on it later
            | DFunction x -> vars.Add (x.Name, x)
            
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
                        | Some y -> pack <| (retNumBin stmnt.Op) (x :?> int) (y :?> int)
                            
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
    
    evalExpr program

let a = Expr (EBinary {Op = BAdd; Arg1 = ELiteral {Value = VInt 5}; Arg2 = ELiteral {Value = VInt 7}})
eval a Map.empty Map.empty
