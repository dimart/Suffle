(*
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
            *)     
#load "Preprocessing.fs"

open Parser.Preprocessing

#time "on"

let lib = """
/*
  This is Lists module, which'll be concatenated with user's program before it's interpretation
*/

// Standard recursive list type
datatype List 'a =
| Cons /* OLOLOLOLOLOLO */'a (List 'a)
| Nil     /* sdjga;sldgasj;lgksg */
end  

// Returns first element of nonempty list
def fun :: (List 'a) -> 'a
head list =
    case list of
    | x : _ -> x
    end

// Returns all elements of nonempty list except the first one
def fun :: (List 'a) -> (List 'a)
tail list = 
    case list of
    | _ : xs -> xs
    end
                  
   
"""                 

open System.Text.RegularExpressions

let p = preprocess lib
printfn "%A" p

(*
let x = run program """

def val :: (List int)           // comment
y = [1, 2, 3]                            
   
"""
#time "off"

let check x =
 match x with
 | S(_, _) -> printfn "Succ"
 | F(pi) -> printfn "%A" <| sprintf "Ln %d Col %d" pi.Position.Line pi.Position.Column

check x
*)