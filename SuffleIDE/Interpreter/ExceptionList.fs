module Interpreter.ExceptionList

//  List of exceptions.
//  Exception consists of err message and line number
exception VariableNotFoundException of string * int
exception TypeMismatchException of string * int
