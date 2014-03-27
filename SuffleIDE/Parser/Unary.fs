module Parser.Unary

open ParserCombinators.Core
open Types

let uNegation : Parser<UnaryOperation> = 
    sym '-' >>% UNegation

let uLogicalNegation : Parser<UnaryOperation> = 
    sym '!' >>% ULogicalNegation

let unaries : Parser<UnaryOperation> = any [uNegation; uLogicalNegation]
