
#r @"M:\projects\Suffle\SuffleIDE\Parser\bin\Debug\Types.dll"
#r @"M:\projects\Suffle\SuffleIDE\Parser\bin\Debug\ParserCombinator.dll"
#load "Auxiliary.fs"
#load "Literals.fs"
#load "Binary.fs"
#load "Pattern.fs"
#load "Types.fs"

open ParserCombinators.Core
open Types
open Parser.Types
open Parser.Pattern

let x = run pCtor "A true"