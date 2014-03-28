
#r @"M:\projects\Suffle\SuffleIDE\Parser\bin\Debug\Specification.dll"
#r @"M:\projects\Suffle\SuffleIDE\Parser\bin\Debug\ParserCombinator.dll"
#load "Auxiliary.fs"
#load "Literals.fs"
#load "Types.fs"
#load "Unary.fs"
#load "Binary.fs"
#load "Pattern.fs"
#load "Structures.fs"

open ParserCombinators.Core
open Parser.Structures

(*
let x = run eBinary "x + y"
let x2 = run eBinary "x * y"
let x3 = run eBinary "1 * 2 + 3"
let x4 = run eBinary "1 * 2 + 3 > 5"
let x5 = run eBinary "1 * 2 + 3 > 5 && x <= y"
let x6 = run eBinary "((1 * 2 + 3 > 5) && x <= y) || 2 + 3 * 4 <> n / 0"
let y = run eBinary "f x + g y"
*)
let t = run eBinary "4 * (2 + 3) * 7 + 5 <= x || false == b && (k < n / 2 || f 'x')"
        