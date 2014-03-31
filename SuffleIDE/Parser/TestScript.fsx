
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
open Parser.Pattern

#time "on"

let s = """
case bb of
| x : xs -> ()
| x : y : xs -> ()
| [] -> ()
| [x] -> ()
| [1] -> ()
| [x, 2, 3] -> ()
| x : [] -> ()
end"""

let x = run eCaseOf """
case x of
| y : ys -> ()
| x : y : xs -> ()
| [] -> ()
| [x] -> ()
| [1] -> ()
| [x, 2, 3] -> ()
| x : [] -> ()
end"""
#time "off"

let check x =
 match x with
 | S(_, _) -> printfn "Succ"
 | F(pi) -> printfn "%A" <| sprintf "Ln %d Col %d" pi.Position.Line pi.Position.Column

check x