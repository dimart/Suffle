module Parser.Auxiliary

open ParserCombinators.Core

let alphas = ['a'..'z'] @ ['A'..'Z']
let digits = ['0'..'9']
let identSymbols = alphas @ ['_'; '\''] @ digits

let wsc = syms [' '; '\t'; '\n'; '\r']
let ws1 = many1 <| wsc
let mws1 p = ws1 >>. p

let kw (s : string) : Parser<string> = 
    between ws1 (pstr s) ws1
    
let ident : Parser<string> = 
    syms alphas >|>> many (syms identSymbols) |>> chars2str

let ctor : Parser<string> = 
    syms ['A'..'Z'] >|>> many (syms (alphas @ digits)) |>> chars2str
    
let inbrackets p : Parser<'a> = 
    (between (sym '(') (skipws p) (skipws <| sym ')'))
  