module Suffle.Parser

open FParsec
open Parser.Structures

let parse (s : string) =
    run program s

