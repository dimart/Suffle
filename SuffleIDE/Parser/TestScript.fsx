
#r @"M:\projects\Suffle\SuffleIDE\Parser\bin\Debug\Specification.dll"                    
#r @"M:\projects\Suffle\SuffleIDE\packages\FParsec.1.0.1\lib\net40-client\FParsecCS.dll"
#r @"M:\projects\Suffle\SuffleIDE\packages\FParsec.1.0.1\lib\net40-client\FParsec.dll"

open FParsec
open Suffle.Specification.Syntax  
open Suffle.Specification.Types

#load "Auxiliary.fs"
#load "Literals.fs"          
#load "Types.fs"
#load "Pattern.fs" 
#load "Unary.fs"
#load "Binary.fs"
#load "Structures.fs"
#load "Parser.fs"

open Parser.Auxiliary
open Parser.Literals     
open Parser.Types
open Parser.Pattern
open Parser.Structures
open Suffle.Parser

let run' p = run (p .>> eof)


//let x = parse """
//
//
//datatype List 'a = 
//| Cons 'a (List 'a)
//| Nil
//end  
//
//def fun :: 'a -> (List 'a)
//mk x = [x]
//
//def fun :: (List 'a) -> int
//len list = 
//    case list of
//    | [] -> 0
//    | _ : rest -> len rest + 1
//    end
//
//def fun :: (List 'a) -> (List 'a)
//rev xs =
//    let 
//        def fun :: (List 'a) -> (List 'a) -> (List 'a)
//        rev' xs rest = 
//            case rest of
//            | [] -> xs
//            | x : rs -> rev' (x : xs) rs
//            end
//    in
//        rev' [] xs
//    end
//
//def val :: (List int)
//xs = mk 5     
//    

//"""  


let y = run' eIfElse "if (a == 0) then b else gcd (b % a) a end"