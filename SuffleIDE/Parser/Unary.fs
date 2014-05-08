module Parser.Unary

open FParsec
open Suffle.Specification.Types
open Suffle.Specification.Syntax
open Parser.Auxiliary

let uNeg stream = 
    pstring sNeg >>% UNeg
    <| stream

let uNot stream = 
    pstring sNot >>% UNot
    <| stream

let unaries stream = 
    choice [uNeg; uNot]  
    <| stream
