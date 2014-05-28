module Suffle.Interpreter.Interpreter

open Suffle.Specification.Types
open Suffle.Interpreter.Expression

let evalExpression (x: Expression) = 
    let inter = new Interpreter()
    inter.EvaluateExpression x

let evalProgram (prog: Program) =
    let inter = new Interpreter()
    inter.EvaluateProgram prog
