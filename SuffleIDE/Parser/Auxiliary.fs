module Parser.Auxiliary

open ParserCombinators.Core
open Specification.Syntax

let alphas = ['a'..'z'] @ ['A'..'Z']
let digits = ['0'..'9']
let identSymbols = alphas @ ['_'; '\''] @ digits

//let commentSingleL : Parser<char list> = (skipws <| between (pstr sSingleLineCommentOpen) (many <| symf (fun c -> c <> '\n')) (pstr sSingleLineCommentClose))
//let commentMultiL : Parser<char list> = (skipws <| between (pstr sMultiLineCommentOpen) (many <| symf (fun _ -> true)) (pstr sMultiLineCommnetClose))

let wsc = syms [' '; '\t'; '\n'; '\r']
let ws1 = many1 <| wsc

let mws1 p = ws1 >>. p

let pws_and_comments = pws
let skipws_and_comments p = pws_and_comments >>. p
let skipws_and_comments1 p = ws1 >>. p
    
let ident : Parser<string> = 
    (optf id "" <| pstr "_") .>>. (syms ['a'..'z'] |>> string) .>>. (many (syms identSymbols) |>> chars2str) |>> (fun ((a, b), c) -> a + b + c)

let ctor : Parser<string> = 
    syms ['A'..'Z'] >|>> many (syms (alphas @ digits)) |>> chars2str
    
let inbrackets p : Parser<'a> = 
    (between (sym '(') (skipws p) (skipws <| sym ')'))
  
let eol : Parser<string> = skipws <| pstr "\n"

let pvartype : Parser<string> = 
    sym '\'' >|>> many1 (syms <| ['a'..'z'] @ ['A'..'Z']) |>> chars2str

