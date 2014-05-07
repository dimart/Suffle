module Interpreter.Expression

open System.Collections.Generic
open Suffle.Specification.Types
open Interpreter.ExceptionList

let lineNum = 0

type Interpreter () = 
    ///  Current function context representation
    let vars = new Dictionary<string, Value list>()

    ///  Returns current function context
    let retContext () = 
        let mutable toRet = []
        for x in vars do
            toRet <- (x.Key, List.head x.Value)::toRet
        toRet

    ///  Renews current context
    let setContext (x: 'a list) = 
        vars.Clear()
        for (key, value) in x do
            vars.Add (key, (value::[]))

    ///  Add variable to context
    let addToContext (key, value) = 
        try
            let tmp = vars.Item key
            vars.Remove key |> ignore
            vars.Add (key, value::tmp)
        with
        | :? KeyNotFoundException -> raise (VariableNotFoundException (key, lineNum))
//        | :? System.ArgumentException -> raise (VariableNotFoundException (key, lineNum))

    ///  Get variable from context
    let getFromContext name = 
        try 
            List.head (vars.Item name)
        with
        //  Returns an exception to IDE to solve
        | :? KeyNotFoundException -> raise (VariableNotFoundException (name, lineNum))
        | :? System.ArgumentException -> raise (VariableNotFoundException (name, lineNum))

    let removeFromContext name =
        try
            let tmp = vars.Item name
            match tmp with
            | _::[] -> vars.Remove name |> ignore
            | [] -> vars.Remove name |> ignore      //  Не должно случиться
            | _::t ->
                vars.Remove name |> ignore
                vars.Add (name, t)
        with
        | :? KeyNotFoundException -> ()

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

    ///  Evaluate closure
    and evalClosure (x: Value) =
        match x with
        | VClosure (variables, expr) -> 
            let x = vars
            setContext variables
            let toRet = evalExpr expr
            vars.Clear()
            for i in x do
                vars.Add (i.Key, i.Value)
            toRet

        | _ -> raise (TypeMismatchException ("Closure expected", lineNum))


    ///  Evaluate type expression
    ///  ETyped
    and evalType (x: Expression) = 
        //toWrite
        VUnit

    ///  Evaluate identifier
    and evalIdent (id: EIdent) = 
        getFromContext id.Name

    ///  Evaluate literal
    and evalLiteral (liter: ELiteral) = 
        liter.Value
        
    ///  Evaluate "if" statement 
    and evalIf (stmnt: EIfElse) = 
        match (evalExpr stmnt.Cond) with
        | VBool cond -> evalExpr (if cond then stmnt.OnTrue else stmnt.OnFalse)
        | _ -> raise (TypeMismatchException ("Bool expected", lineNum))
        
    ///  Evaluate "let" stmnt
    and evalLet (stmnt: ELetIn) = 
        match stmnt.Binding with
        | DValue x -> 
            addToContext (x.Name.Name, (evalExpr x.Value))
            let toRet = evalExpr stmnt.Body
            removeFromContext x.Name.Name
            toRet
        //  May be need to pass closure or lambda - dunno now, check on it later
//        | DFunction x -> (funcs.Item x.Name.Name).Push <| x
        | _ -> raise (TypeMismatchException ("Declaration expected", lineNum))
            
    ///  Evaluate unary stmnt
    and evalUnary (stmnt: EUnary) = 
        let evaluated = evalExpr stmnt.Arg
        match stmnt.Op with
        | UNeg ->   match evaluated with
                    | VInt x -> VInt -x
                    | _ -> raise (TypeMismatchException ("Int expected", lineNum))
        | UNot ->   match evaluated with
                    | VBool x -> VBool <| not x
                    | _ -> raise (TypeMismatchException ("Bool expected", lineNum))

    ///  Evaluate binary statement
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

    ///  Evaluate lambda expression
    and evalLambda (stmnt: ELambda) = 
        VClosure (retContext(), ELambda stmnt)

    ///  Evaluate function application
    and evalFunApp (stmnt: EFunApp) = 
        let arg = evalExpr stmnt.Arg
        match stmnt.Func with
        | ELambda x -> 
            let clos = evalLambda x
            let arg = (x.Arg.Name, arg)
            match clos with
            | VClosure (a, b) -> evalClosure <| VClosure (arg::a, b)
            | _ -> raise (TypeMismatchException ("Closure expected", lineNum))
        | _ -> raise (TypeMismatchException ("Lambda function excpected", lineNum))


    ///  Evaluate 'case ... of ...' expression
    and evalCaseOf (stmnt: ECaseOf) = 
        VInt 5


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
