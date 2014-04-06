// Дополнительные сведения о F# см. по адресу http://fsharp.net. Для получения дополнительных рекомендаций по программированию на языке F# см. проект
// "F# Tutorial".

#load "types.fs"
#load "Expression.fs"
open Interpreter.Expression
open Types

// Определите здесь код скрипта библиотеки
let a = Expr (EBinary {Op = BAdd; Arg1 = ELiteral {Value = VInt 5}; Arg2 = ELiteral {Value = VInt 7}})

let x = 
    match a with
    | Expr x -> evalExpr x
