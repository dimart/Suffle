module Suffle.Interpreter.Interpreter

open Suffle.Specification.Types
open Suffle.Interpreter.Expression

let inter = new Interpreter()

let evalExpression (x: Expression) = 
    inter.EvaluateExpression x

let evalProgram (prog: Program) =
    inter.EvaluateProgram prog
