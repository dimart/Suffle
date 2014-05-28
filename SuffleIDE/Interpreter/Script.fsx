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

let x = evalExpression (
                        EFunApp {
                            Func = ELambda {
                                Arg = { EIdent.Name = "x"}
                                Body = ELiteral { Value = VInt 5 }
                            }
                            Arg = ELiteral { Value = VInt 3 }
                        }
                    )
