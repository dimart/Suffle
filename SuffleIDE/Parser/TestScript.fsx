
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

#time "on"

// cannot parser 
let x = run program """
datatype List 'a = // this datatype represent familiar lists as in other PLs
| Cons 'a (List 'a)
| Nil
end 
// olol
"""
#time "off"

let check x =
 match x with
 | S(_, _) -> printfn "Succ"
 | F(pi) -> printfn "%A" <| sprintf "Ln %d Col %d" pi.Position.Line pi.Position.Column

check x