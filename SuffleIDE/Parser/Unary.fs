module Parser.Unary

open ParserCombinators.Core
open Specification.Types
open Specification.Syntax

let uNeg : Parser<UnaryOp> = 
    pstr sNeg >>% UNeg

let uNot : Parser<UnaryOp> = 
    pstr sNot >>% UNot

let unaries : Parser<UnaryOp> = any [uNeg; uNot]
