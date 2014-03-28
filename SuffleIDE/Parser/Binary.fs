module Parser.Binary

open ParserCombinators.Core
open Specification.Types
open Specification.Syntax
open Specification.OperationPriority

// Arithmetic
let bAdd : Parser<BinaryOp> = 
    pstr sAdd >>% BAdd

let bSub : Parser<BinaryOp> = 
    pstr sSub >>% BSub

let bDiv : Parser<BinaryOp> = 
    pstr sDiv >>% BDiv

let bMul : Parser<BinaryOp> = 
    pstr sMul >>% BMul


// Logic
let bAnd : Parser<BinaryOp> = 
    pstr sAnd >>% BAnd

let bOr  : Parser<BinaryOp> = 
    pstr sOr >>% BOr


// Comparation
let bEQ  : Parser<BinaryOp> = 
    pstr sEQ >>% BEQ

let bNEQ : Parser<BinaryOp> = 
    pstr sNEQ >>% BNEQ

let bGT : Parser<BinaryOp> = 
    pstr sGT >>% BGT
    
let bLT : Parser<BinaryOp> = 
    pstr sLT >>% BLT

let bNGT : Parser<BinaryOp> = 
    pstr sNGT >>% BNGT

let bNLT : Parser<BinaryOp> = 
    pstr sNLT >>% BNLT    

let binaries : Parser<BinaryOp> = 
    any [bAdd; bSub; bDiv; bMul; 
         bAnd; bOr; 
         bEQ; bNEQ; bGT; bLT; bNGT; bNLT]

let binOp2Parser b =
    match b with
    // Arithmetics
    | BAdd -> bAdd
    | BSub -> bSub
    | BDiv -> bDiv
    | BMul -> bMul
    // Logic
    | BAnd -> bAnd
    | BOr  -> bOr
    // Comparation
    | BEQ  -> bEQ
    | BNEQ -> bNEQ
    | BGT  -> bGT
    | BLT  -> bLT
    | BNGT -> bNGT
    | BNLT -> bNLT

let binPrioritised : Parser<BinaryOp> list =
    binaryOps
    |> Seq.sortBy priority
    |> Seq.groupBy priority
    |> Seq.map snd
    |> Seq.map (Seq.map binOp2Parser)
    |> Seq.map any
    |> Seq.toList