module Parser.Auxiliary

open FParsec
open Suffle.Specification.Syntax  

let chars2str cs = string (new System.String (cs |> List.toArray))
let chars2int s = int (new System.String(s |> List.toArray))
let str2chars (s : string) = List.ofArray <| s.ToCharArray()  

let (>|>>) phead ptail =
    phead .>>. ptail |>> (fun (x, xs) -> x::xs)

let any = choice

let alphas = ['a'..'z'] @ ['A'..'Z']
let digits = ['0'..'9']
let pAlphas stream = anyOf alphas stream 
let pIdentSymbols stream = anyOf (alphas @ ['_'; '\''] @ digits) stream 

let pquote stream = pchar '\'' stream

//let commentSingleL : Parser<char list> = (skipws <| between (pstr sSingleLineCommentOpen) (many <| symf (fun c -> c <> '\n')) (pstr sSingleLineCommentClose))
//let commentMultiL : Parser<char list> = (skipws <| between (pstr sMultiLineCommentOpen) (many <| symf (fun _ -> true)) (pstr sMultiLineCommnetClose))

let pws_and_comments = spaces     
let pws_and_comments1 = spaces1
let skipws_before p = pws_and_comments >>. p        
let skipws_before1 p = pws_and_comments1 >>. p
let skipws_after p = p .>> pws_and_comments
let skipws_around p = skipws_after (skipws_before p)

let ws_after p = p .>> spaces
let ws_after1 p = p .>> spaces1
let one_space stream = skipAnyOf " \t\r\n" stream
 
let skipws_and_comments p = skipws_before p 
let skipws_and_comments1 p = spaces1 >>. p 
    
let ident stream =
    (pchar '_' <|> (anyOf ['a'..'z'])) >|>> (many pIdentSymbols) |>> chars2str 
    <| stream

let ctor stream = 
    upper .>>. many (anyOf (alphas @ digits)) |>> (fun (c, cs) -> chars2str <| c::cs)
    <| stream

let betweenStrings openStr closeStr p = between (pstring openStr) (pstring closeStr) p
let betweenBrackets p = between (skipws_after <| pstring "(") (pstring ")") (skipws_after p)


let inbrackets p = betweenBrackets p
                                                 
let pvartype stream = 
    pquote >|>> many1 pAlphas |>> chars2str
    <| stream


