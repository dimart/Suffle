module Suffle.Specification.OperationPriority

open Suffle.Specification.Types

let binaryOps = 
    [BAdd; BSub; BDiv; BMul; BMod;
     BAnd; BOr;
     BEQ; BNEQ; BNGT; BNLT; BGT; BLT ]

let priority binOp =
    match binOp with
    // Arithmetics
    | BAdd | BSub -> 6
    | BDiv | BMul | BMod -> 7
    // Logic
    | BAnd -> 4
    | BOr  -> 3
    // Comparation
    | BEQ | BNEQ | BGT | BLT | BNGT | BNLT -> 5