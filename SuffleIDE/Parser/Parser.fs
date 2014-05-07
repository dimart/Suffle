module Suffle.Parser

open ParserCombinators.Core
open Parser.Structures
open Parser.Preprocessing

let parse (s : string) =
    run program (preprocess s)

