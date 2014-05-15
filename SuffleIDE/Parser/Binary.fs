module Parser.Binary

open FParsec
open Suffle.Specification.Types
open Suffle.Specification.Syntax
open Suffle.Specification.OperationPriority
open Parser.Auxiliary

// Arithmetic
let bAdd stream = 
    pstring sAdd >>% BAdd
    <| stream

let bSub stream = 
    pstring sSub >>% BSub   
    <| stream

let bDiv stream = 
    pstring sDiv >>% BDiv 
    <| stream

let bMul stream = 
    pstring sMul >>% BMul   
    <| stream

let bMod stream = 
    pstring sMod >>% BMod   
    <| stream

// Logic
let bAnd stream = 
    pstring sAnd >>% BAnd    
    <| stream

let bOr  stream = 
    pstring sOr >>% BOr    
    <| stream


// Comparation
let bEQ  stream = 
    pstring sEQ >>% BEQ        
    <| stream

let bNEQ stream = 
    pstring sNEQ >>% BNEQ     
    <| stream

let bGT stream = 
    pstring sGT >>% BGT     
    <| stream
    
let bLT stream = 
    pstring sLT >>% BLT      
    <| stream

let bNGT stream = 
    pstring sNGT >>% BNGT    
    <| stream

let bNLT stream = 
    pstring sNLT >>% BNLT   
    <| stream 

let binaries stream = 
    choice [bAdd; bSub; bDiv; bMul; bMod;
            bAnd; bOr; 
            bEQ; bNEQ; attempt bNGT; attempt bNLT; bGT; bLT]

let binOp2Parser b =
    match b with
    // Arithmetics
    | BAdd -> bAdd
    | BSub -> bSub
    | BDiv -> bDiv
    | BMul -> bMul
    | BMod -> bMod
    // Logic
    | BAnd -> bAnd
    | BOr  -> bOr
    // Comparation
    | BEQ  -> bEQ
    | BNEQ -> bNEQ
    | BGT  -> bGT
    | BLT  -> bLT
    | BNGT -> attempt bNGT
    | BNLT -> attempt bNLT

let binPrioritised : Parser<BinaryOp, unit> list =
    binaryOps
    |> Seq.sortBy priority
    |> Seq.groupBy priority
    |> Seq.map (snd >> (Seq.map binOp2Parser) >> choice)
    |> Seq.toList