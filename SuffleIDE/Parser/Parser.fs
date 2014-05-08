module Suffle.Parser

open FParsec
open Parser.Structures
open Parser.Preprocessing

let parse (s : string) =
    run program (preprocess s)

