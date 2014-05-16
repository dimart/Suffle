// Дополнительные сведения о F# см. по адресу http://fsharp.net. Для получения дополнительных рекомендаций по программированию на языке F# см. проект
// "F# Tutorial".

#load "..\Specification\Types.fs"
#load "ExceptionList.fs"
#load "Expression.fs"
#load "Interpreter.fs"
open Suffle.Interpreter.Expression
open Suffle.Specification.Types
open Suffle.Interpreter.Interpreter

// Определите здесь код скрипта библиотеки
let a = (EBinary {Op = BAdd; Arg1 = ELiteral {Value = VInt 5}; Arg2 = ELiteral {Value = VInt 7}})

let x = eval a
