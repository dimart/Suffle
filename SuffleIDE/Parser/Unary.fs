module Parser.Unary

open ParserCombinators.Core
open Suffle.Specification.Types
open Suffle.Specification.Syntax

let uNeg : Parser<UnaryOp> = 
    pstr sNeg >>% UNeg

let uNot : Parser<UnaryOp> = 
    pstr sNot >>% UNot

let unaries : Parser<UnaryOp> = any [uNeg; uNot]
