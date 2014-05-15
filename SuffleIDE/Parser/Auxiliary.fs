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
let pAlpha s = anyOf alphas <| s
let pIdentSymbols s = anyOf (alphas @ ['_'; '\''] @ digits) <| s 

let pquote stream = pchar '\'' <| stream

let ws s = spaces <| s
let _ws p = ws >>? p
let ws_ p = p .>>? ws
let _ws_ p = ws >>? p .>>? ws       
let ws1 = spaces1
let _ws1 p = ws1 >>? p
let ws1_ p = p .>>? ws1
let _ws1_ p = ws1 >>? p .>>? ws1       
  
let pstr s = ws_ (pstring s)
    
let ident stream =
    let unders = many (pchar '_') |>> chars2str
    let p = 
        unders .>>. anyOf ['a'..'z'] .>>. many pIdentSymbols
        |>> (fun ((us, c), cs) -> us + chars2str (c::cs))
    ws_ p
    <| stream

let ctor stream = 
    let p = upper .>>. many (anyOf (alphas @ digits)) |>> (fun (c, cs) -> chars2str <| c::cs)
    ws_ p
    <| stream
                                                 
let pvartype stream = 
    let p = pquote >|>> many1 pAlpha |>> chars2str
    ws_ p
    <| stream
                  
let inbrackets p = between (ws_ (pchar '(')) (ws_ (pchar ')')) p
