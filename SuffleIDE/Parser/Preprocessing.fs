module Parser.Preprocessing

open System.Text.RegularExpressions

let internal ws = "( |\t|\r)"

let refactors =
    [
        ("/\*((.|\n)*?)\*/", "")
        ("//.*\n", "\n")
        (sprintf "\n(%s*\n)*" ws, "\n")
        (sprintf "%s*\n%s*" ws ws, "\n")
        (sprintf "%s+" ws, " ")
    ]

let preprocess (s : string) = s
(*
    let rec prep s =
        function
        | [] -> s
        | (pt : string, re : string) :: xs -> prep (Regex.Replace(s, pt, re)) xs
    prep s refactors
    *)