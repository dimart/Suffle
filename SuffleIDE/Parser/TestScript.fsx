
#r @"M:\projects\Suffle\SuffleIDE\Parser\bin\Debug\Types.dll"
#r @"M:\projects\Suffle\SuffleIDE\Parser\bin\Debug\ParserCombinator.dll"
#load "Auxiliary.fs"
#load "Literals.fs"
#load "Binary.fs"
#load "Types.fs"

open ParserCombinators.Core
open Types
open Parser.Types

let x = run (tLambda) "int -> (char -> bool) -> ('a -> A) -> ((int -> int))"