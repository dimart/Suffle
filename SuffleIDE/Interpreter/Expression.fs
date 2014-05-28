module Suffle.Interpreter.Expression

open System.Collections.Generic
open Suffle.Specification.Types
open Suffle.Interpreter.ExceptionList

let lineNum = 0

type Interpreter () = 
    /// Current function context representation
    let vars = new Dictionary<string, Stack<Value>> ()
    
    /// Declarations in current program
    let decls = new Dictionary<Declaration, Value> ()

    /// Returns given context as list
    let getContext (context: Dictionary<string, Stack<Value>>) = 
        let mutable toRet = []
        for x in context do
            toRet <- (x.Key, x.Value.Peek())::toRet
        toRet

    /// Sets closure context
    let setClosureContext (x: (string * Value) list) = 
        let closure = new Dictionary<string, Stack<Value>> ()
        for (key, value) in x do
            let stack = new Stack<Value>()
            stack.Push value
            closure.Add (key, stack)
        closure

    /// Add variable to context
    let addToContext (key, value) (context: Dictionary<string, Stack<Value>>) = 
        if not <| context.ContainsKey key then
            let tmp = new Stack<Value>()
            tmp.Push value
            context.Add (key, tmp)
        else 
            try
                (context.Item key)
                    .Push value
            with
            | :? KeyNotFoundException -> raise (VariableNotFoundException (key, lineNum))
//            | :? System.ArgumentException -> raise (VariableNotFoundException (key, lineNum))

    /// Get variable from context
    let getFromContext name (context: Dictionary<string, Stack<Value>>) = 
        try 
            (context.Item name)
                .Peek()
        with
        //  Returns an exception to IDE to solve
        | :? KeyNotFoundException -> raise (VariableNotFoundException (name, lineNum))
        | :? System.ArgumentException -> raise (VariableNotFoundException (name, lineNum))

    /// Remove Variable from context
    let removeFromContext name (context: Dictionary<string, Stack<Value>>) =
        try
            let tmp = context.Item name
            tmp.Pop() |> ignore
        with
        | :? KeyNotFoundException -> ()
        | :? System.ArgumentNullException -> ()

    /// Return binary numeric (returning int) operator
    let retNumBin (op: BinaryOp): int -> int -> int = 
        match op with
        | BAdd -> (+)
        | BSub -> (-)
        | BDiv -> (/)
        | BMul -> (*)
        | _ -> raise (TypeMismatchException ("Wrong operation", lineNum))

    /// Return binary comparison operator
    let retBoolNumBin (op: BinaryOp): int -> int -> bool = 
        match op with
        | BEQ -> (=)
        | BNEQ -> (<>)
        | BGT -> (>)
        | BLT -> (<)
        | BNGT -> (<=)
        | BNLT -> (>=)
        | _ -> raise (TypeMismatchException ("Wrong operation", lineNum))

    /// Return binary boolean operator
    let retBoolBin (op: BinaryOp): bool -> bool -> bool = 
        match op with
        | BAnd -> (&&)
        | BOr -> (||)
        | _ -> raise (TypeMismatchException ("Wrong operation", lineNum))

    /// Evaluate closure
    let rec evalClosure (x: Value) (context: Dictionary<string, Stack<Value>>) =
        match x with
        | VClosure (_, expr) ->
            match expr with
            | ELambda expr ->
                evalExpr expr.Body context
            | _ -> evalExpr expr context
        | _ -> raise (TypeMismatchException ("Closure expected", lineNum))

    /// Evaluate type expression
    /// ETyped
    and evalType (x: Expression) (context: Dictionary<string, Stack<Value>>) = 
        //toWrite
        VUnit

    /// Evaluate identifier
    and evalIdent (id: EIdent) (context: Dictionary<string, Stack<Value>>) = 
        getFromContext id.Name context

    /// Evaluate literal
    and evalLiteral (liter: ELiteral) (_: Dictionary<string, Stack<Value>>) = 
        liter.Value
        
    /// Evaluate "if" statement 
    and evalIf (stmnt: EIfElse) (context: Dictionary<string, Stack<Value>>) = 
        match evalExpr stmnt.Cond context with
        | VBool cond -> evalExpr (if cond then stmnt.OnTrue else stmnt.OnFalse) context
        | _ -> raise (TypeMismatchException ("Bool expected", lineNum))
        
    /// Evaluate "let" stmnt
    //  TODO: Implement function declarations
    and evalLet (stmnt: ELetIn) (context: Dictionary<string, Stack<Value>>) = 
        match stmnt.Binding with
        | DValue x -> 
            //  Add to current context, eval, delete from context
            addToContext (x.Name.Name, (evalExpr x.Value context)) context
            let toRet = evalExpr stmnt.Body context
            removeFromContext x.Name.Name context
            toRet

        | DFunction x -> 
            addToContext (x.Name.Name, (evalExpr x.Body context)) context
            let toRet = evalExpr stmnt.Body context
            removeFromContext x.Name.Name context
            toRet
        | _ -> raise (TypeMismatchException ("Declaration expected", lineNum))
            
    /// Evaluate unary stmnt
    and evalUnary (stmnt: EUnary) (context: Dictionary<string, Stack<Value>>) = 
        let evaluated = evalExpr stmnt.Arg context
        match stmnt.Op with
        | UNeg ->   match evaluated with
                    | VInt x -> VInt -x
                    | _ -> raise (TypeMismatchException ("Int expected", lineNum))
        | UNot ->   match evaluated with
                    | VBool x -> VBool <| not x
                    | _ -> raise (TypeMismatchException ("Bool expected", lineNum))

    /// Evaluate binary statement
    and evalBinary (stmnt: EBinary) (context: Dictionary<string, Stack<Value>>) = 
        let evaluated1 = evalExpr stmnt.Arg1 context
        let evaluated2 = evalExpr stmnt.Arg2 context
        match evaluated1 with
        | VInt x -> 
            match evaluated2 with
                | VInt y -> 
                    try
                        VInt <| retNumBin stmnt.Op x y
                    with
                    | :? TypeMismatchException -> VBool <| retBoolNumBin stmnt.Op x y
                | _ -> raise (TypeMismatchException ("Int expected", lineNum))
        | VBool x -> 
            match evaluated2 with
                | VBool y -> VBool<| retBoolBin stmnt.Op x y
                | _ -> raise (TypeMismatchException ("Bool expected", lineNum))
        | _ -> raise (TypeMismatchException ("Int or Bool expected", lineNum))

    /// Evaluate lambda expression
    and evalLambda (stmnt: ELambda) (context: Dictionary<string, Stack<Value>>) = 
        VClosure (getContext context, ELambda stmnt)

    /// Evaluate function application
    and evalFunApp (stmnt: EFunApp) (context: Dictionary<string, Stack<Value>>) = 
        let arg = evalExpr stmnt.Arg context
        let func = evalExpr stmnt.Func context
        match func with
        | VClosure (cont, ex) as it ->
            let closureContext = setClosureContext cont
            let argName = 
                match ex with
                    | ELambda x -> x.Arg.Name
                    | _ -> raise (TypeMismatchException("Type mismatch", lineNum))
            addToContext (argName, arg) closureContext
            match stmnt.Func with
            | EIdent x -> 
                addToContext (x.Name, it) closureContext
            | _ -> 
                ()
            let toRet = evalClosure it closureContext
//            removeFromContext argName context
            toRet
        | _ -> raise (TypeMismatchException("Type mismatch", lineNum))

    /// Evaluate 'case ... of ...' expression
    and evalCaseOf (stmnt: ECaseOf) (context: Dictionary<string, Stack<Value>>) = 
        let matchable = evalExpr stmnt.Matching context
        let mutable toRet = VUnit
        let mutable finish = false
        let names = new List<string>()
        let matchPattern pattern =
            names.Clear()
            let rec tryMatch (value, pattern) =
                match pattern with
                | PIdent x ->
                    addToContext (x.Name, value) context
                    names.Add x.Name
                    true
                | PLiteral x ->
                    value = x.Value
                | PCtor (namePat, patterns) ->
                    match value with
                    | VCtor (name, values) when name = namePat ->
                        let pairs = List.zip values patterns
                        List.fold (fun acc x -> x && acc) true [for pairs in pairs -> tryMatch pairs]
                    | _ -> false
                | PWildcard ->
                    true
            tryMatch (matchable, pattern)
        for (pattern, expression) in stmnt.Patterns do
            if not finish then
                if matchPattern pattern then
                    toRet <- evalExpr expression context
                    for id in names do
                        removeFromContext id context
                    finish <- true
            else ()
        toRet

    ///  Evaluate constructor application
    and evalConstr (stmnt: ECtor) (context: Dictionary<string, Stack<Value>>) = 
        VCtor (stmnt.CtorName, [ for ident in stmnt.Args -> getFromContext ident.Name context ])

    /// Evaluate expression
    and evalExpr (expr: Expression) (context: Dictionary<string, Stack<Value>>) = 
        match expr with
        | EIdent x -> evalIdent x context
        | ELiteral x -> evalLiteral x context
        | EIfElse x -> evalIf x context
        | ELetIn x -> evalLet x context
        | EBinary x -> evalBinary x context
        | EUnary x -> evalUnary x context
        | ELambda x -> evalLambda x context
        | EFunApp x -> evalFunApp x context
        | ECaseOf x -> evalCaseOf x context
        | ECtor x -> evalConstr x context

    let evalDecl decl = 
        match decl with
        | DValue x ->
            let variable = evalExpr x.Value vars
            let name = x.Name.Name
            addToContext (name, variable) vars
            decls.Add (decl, variable)
        | DFunction x ->
            let variable = evalExpr x.Body vars
            let name = x.Name.Name
            addToContext (name, variable) vars
            decls.Add (decl, variable)
        | DDatatype x ->
//            for (name, types) in x.Ctors do
            ()
                

    let evalProgram (prog: Program) = 
        for item in prog do
            evalDecl item

    let rec printType (typo: Type) = 
        match typo with
        | TUnit -> "unit"
        | TBool -> "bool"
        | TChar -> "char"
        | TInt -> "int"
        | TLambda (arg, body) -> sprintf "%s -> %s" (printType arg) (printType body)
        | TDatatype (name, types) -> 
            sprintf "%s:%s" name 
            <| List.reduce (fun l r -> sprintf " %s ->%s" l r) (List.map (printType) types)
        | TVar x -> x

    member this.EvaluateExpression (expr: Expression) = 
        evalExpr expr vars

    member this.EvaluateProgram (prog: Program) = 
        evalProgram prog
        this.Print()

    member this.Print () = 
        let mutable toRet = ""
        for pair in decls do
            let s = 
                match pair.Key with
                | DFunction x -> x.Name.Name
                | DValue x -> x.Name.Name
                | DDatatype x -> ""
            let t = 
                match pair.Key with
                | DFunction x -> printType x.Type
                | DValue x -> printType x.Type
                | DDatatype x -> List.reduce (fun h x -> sprintf "%s -> %s" h x) x.Params
            let y =
                let rec print' x = 
                    match x with
                    | VInt x -> x.ToString()
                    | VUnit _ -> "()"
                    | VBool x -> x.ToString()
                    | VChar x -> x.ToString()
                    | VCtor (a, b) ->
                        if b = [] then 
                            ""
                        else
                            sprintf "%s: (%s)" a 
                            <| List.reduce (fun l r -> sprintf "%s; %s" l r) (List.map print' b)
                    | VClosure _ -> ""
                print' pair.Value
            match pair.Value with
            | VClosure _ -> toRet <- sprintf "%sfun %s: %A\n" toRet s t
            | _ -> toRet <- sprintf "%svar %s = %A\n" toRet s y
        toRet
