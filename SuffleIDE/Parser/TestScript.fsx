
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

let x = run program """
datatype List 'a = 
| Cons 'a (List 'a)
| Nil
end  
                                        // just add single line comments :)
def fun :: 'a -> (List 'a)
mk x = [x]

def fun :: (List 'a) -> int
len list = 
    case list of
    | [] -> 0
    | _ : rest -> len rest + 1
    end

def fun :: (List 'a) -> (List 'a)
rev xs =
    let 
        def fun :: (List 'a) -> (List 'a) -> (List 'a)
        rev' xs rest = 
            case rest of
            | [] -> xs
            | x : rs -> rev' (x : xs) rs
            end
    in
        rev' [] xs
    end

def val :: (List int)
xs = mk 5     
   
"""
#time "off"

let check x =
 match x with
 | S(_, _) -> printfn "Succ"
 | F(pi) -> printfn "%A" <| sprintf "Ln %d Col %d" pi.Position.Line pi.Position.Column

check x