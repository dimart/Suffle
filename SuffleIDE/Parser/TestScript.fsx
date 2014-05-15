
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


let t1 = run' pattern """()"""