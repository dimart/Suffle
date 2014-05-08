module Parser.Auxiliary

open FParsec
open Suffle.Specification.Syntax  

// For Debugging

let (<!>) (p: Parser<_,_>) label : Parser<_,_> =                           
    fun stream ->
        printfn "%A: Entering %s" stream.Position label
        let reply = p stream
        printfn "%A: Leaving %s (%A)" stream.Position label reply.Status
        printfn "Result: %A" (reply.Result)
        reply


let chars2str cs = string (new System.String (cs |> List.toArray))
let chars2int s = int (new System.String(s |> List.toArray))
let str2chars (s : string) = List.ofArray <| s.ToCharArray()  

let (>|>>) phead ptail =
    phead .>>. ptail |>> (fun (x, xs) -> x::xs)

let alphas = ['a'..'z'] @ ['A'..'Z']
let digits = ['0'..'9']
let pAlpha s = anyOf alphas <??> "alphabet letter" <| s
let pIdentSymbols s = anyOf (alphas @ ['_'; '\''] @ digits) <??> "ident symbols (letter, digit, '_' or ')" <| s 

let pquote stream = pchar '\'' <| stream

let ws s = spaces <| s
let _ws p = ws >>? p
let ws_ p = p .>>? ws
let _ws_ p = ws >>? p .>>? ws       
let ws1 = spaces1
let _ws1 p = ws1 >>? p
let ws1_ p = p .>>? ws1
let _ws1_ p = ws1 >>? p .>>? ws1
    
let ident stream =
    let head = pchar '_' <|> (anyOf ['a'..'z'])
    let tail = many pIdentSymbols
    head >|>> tail |>> chars2str
    <??> "identifier" 
    <| stream

let ctor stream = 
    upper .>>. many (anyOf (alphas @ digits)) |>> (fun (c, cs) -> chars2str <| c::cs)
    <??> "constructor name"
    <| stream
                                                 
let pvartype stream = 
    pquote >|>> many1 pAlpha |>> chars2str
    <??> "variable type name"
    <| stream
                  
let inbrackets p = between (ws_ (pchar '(' <??> "open bracket")) (ws_ (pchar ')' <??> "close bracket")) (ws_ p)
