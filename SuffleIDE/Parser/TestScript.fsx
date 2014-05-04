
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

open Parser.Auxiliary
open Parser.Literals     
open Parser.Types
open Parser.Pattern
open Parser.Structures

//#load "Auxiliary.fs"
            
      (*  
open FParsec
open Suffle.Specification.Types
open Parser.Literals
open Parser.Types
open Parser.Pattern
open Parser.Structures     
*)

let run' p s = run (p .>> eof) s

let isSucc (value : 'a) =
    function 
    | FParsec.CharParsers.Success(v, _, _) -> v = value
    | _ -> false

let isFail =
    function
    | FParsec.CharParsers.Failure _ -> true
    | _ -> false

let x = run' eUnary """!(a < 10)"""