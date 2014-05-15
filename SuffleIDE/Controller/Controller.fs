module Suffle.Controller.Controller

open Suffle.Interpreter.Interpreter
open Suffle.Parser
open System.IO

[<EntryPoint>]
let main argv = 
    if argv.Length = 1 then
        let program = File.ReadAllText argv.[0]
        let ast = parse program
        evalProgram ast
    0 // возвращение целочисленного кода выхода
