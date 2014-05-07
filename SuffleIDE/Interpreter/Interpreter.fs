module Suffle.Interpreter

open Suffle.Specification.Types
open Interpreter.Expression

let eval x = 
    match x with
    | Expr x -> evalExpr x
