module Interpreter.Expression

open System.Collections.Generic
open Suffle.Specification.Types
open Interpreter.ExceptionList

let lineNum = 0

type Interpreter () = 
    let vars = new Dictionary<string, Stack<Value>>()
    let funcs = new Dictionary<string, Stack<DFunction>>()

    ///  Return binary operator
    let rec retNumBin (op: BinaryOp): int -> int -> int = 
        match op with
        | BAdd -> (+)
        | BSub -> (-)
        | BDiv -> (/)
        | BMul -> (*)
        | _ -> raise (TypeMismatchException ("Wrong operation", lineNum))

    and retBoolNumBin (op: BinaryOp): int -> int -> bool = 
        match op with
        | BEQ -> (=)
        | BNEQ -> (<>)
        | BGT -> (>)
        | BLT -> (<)
        | BNGT -> (<=)
        | BNLT -> (>=)
        | _ -> raise (TypeMismatchException ("Wrong operation", lineNum))

    and retBoolBin (op: BinaryOp): bool -> bool -> bool = 
        match op with
        | BAnd -> (&&)
        | BOr -> (||)
        | _ -> raise (TypeMismatchException ("Wrong operation", lineNum))

    ///  Evaluate type expression
    ///  ETyped
    and evalType (x: Expression) = 
        //toWrite
        VUnit

    ///  Eval identifier
    and evalIdent (id: EIdent) = 
//            try
        try 
            (vars.Item id.Name).Peek()
        with
        | :? KeyNotFoundException -> raise (VariableNotFoundException (id.Name, lineNum))
        | :? System.InvalidOperationException -> raise (VariableNotFoundException (id.Name, lineNum))
//            with
//                //  if identifier isn't defined - we fail to interpret (? - mb need to do on top level)
//                | VariableNotFoundException name -> printfn "Variable \"%A\" not found" name

    ///  Eval literal
    and evalLiteral (liter: ELiteral) = 
        liter.Value
        
    ///  Eval "if" statement 
    and evalIf (stmnt: EIfElse) = 
        match (evalExpr stmnt.Cond) with
        | VBool cond -> evalExpr (if cond then stmnt.OnTrue else stmnt.OnFalse)
        | _ -> raise (TypeMismatchException ("Expected type \'bool\'.", lineNum))
        
    ///  Eval "let" stmnt
    and evalLet (stmnt: ELetIn) = 
        match stmnt.Binding with
        | DValue x -> (vars.Item x.Name.Name).Push <| evalExpr x.Value
        //  May be need to pass closure or lambda - dunno now, check on it later
        | DFunction x -> (funcs.Item x.Name.Name).Push <| x
        | _ -> ()
        evalExpr stmnt.Body
            
    ///  Eval unary stmnt
    and evalUnary (stmnt: EUnary) = 
        let evaluated = evalExpr stmnt.Arg
        match stmnt.Op with
        | UNeg ->   match evaluated with
                    | VInt x -> VInt -x
                    | _ -> raise (TypeMismatchException ("Int expected", lineNum))
        | UNot ->   match evaluated with
                    | VBool x -> VBool <| not x
                    | _ -> raise (TypeMismatchException ("Bool expected", lineNum))

    ///  Eval binary statement
    and evalBinary (stmnt: EBinary) = 
        let evaluated1 = evalExpr stmnt.Arg1
        let evaluated2 = evalExpr stmnt.Arg2
        match evaluated1 with
        | VInt x -> match evaluated2 with
        //  FIXME: Must also evaluate bool statements. Mb need to know argument somewhere from outside (typecheck)
                    | VInt y -> VInt <| retNumBin stmnt.Op x y
                    | _ -> raise (TypeMismatchException ("Int expected", lineNum))
        | VBool x -> match evaluated2 with
                        | VBool y -> VBool<| retBoolBin stmnt.Op x y
                        | _ -> raise (TypeMismatchException ("Bool expected", lineNum))
        | _ -> raise (TypeMismatchException ("Int or Bool expected", lineNum))

    ///  Eval lambda expression
    and evalLambda (stmnt: ELambda) = 
        let arg = stmnt.Arg.Name
        

    ///  Evaluate expression
    and evalExpr (expr: Expression) = 
                            
    (*                            
    | ELambda of ELambda
    | EFunApp of EFunApp
    | ECaseOf of ECaseOf
    *)

        match expr with
        | EIdent x -> evalIdent x
        | ELiteral x -> evalLiteral x
        | EIfElse x -> evalIf x
        | ELetIn x -> evalLet x
        | EBinary x -> evalBinary x
        | EUnary x -> evalUnary x
        | ELambda x -> evalLambda x
        | _ -> failwith "Not Implemented Pattern"
    
let a = Expr (EBinary {Op = BAdd; Arg1 = ELiteral {Value = VInt 5}; Arg2 = ELiteral {Value = VInt 7}})
//eval a Map.empty Map.empty
