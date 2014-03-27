module Parser.Binary

open ParserCombinators.Core
open Types

// Arithmetic
let bAdd : Parser<BinaryOperation> = 
    pstr "+" >>% BAdd

let bSub : Parser<BinaryOperation> = 
    pstr "-" >>% BSub

let bDiv : Parser<BinaryOperation> = 
    pstr "/" >>% BDiv

let bMul : Parser<BinaryOperation> = 
    pstr "*" >>% BMul


// Logic
let bAnd : Parser<BinaryOperation> = 
    pstr "&&" >>% BAnd

let bOr  : Parser<BinaryOperation> = 
    pstr "||" >>% BOr

let bEq  : Parser<BinaryOperation> = 
    pstr "==" >>% BEq

let bNEq : Parser<BinaryOperation> = 
    pstr "<>" >>% BNEq


let binaries : Parser<BinaryOperation> = 
    any [bAdd; bSub; bDiv; bMul; bAnd; bOr; bEq; bNEq]