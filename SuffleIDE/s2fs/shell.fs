
open Translator
open Suffle.Parser

let prog = 
    parse """

datatype List 'a = 
| Cons 'a (List 'a)
| Nil
end 

def fun :: 'a -> (List 'a)
mk x = [x]

def fun :: (List 'a) -> int
len list = 
    case list of
    | [] -> 0
    | _ : rest -> len rest + 1
    end

def fun :: (List 'a) -> (List 'a)
rev xs =
    let 
        def fun :: (List 'a) -> (List 'a) -> (List 'a)
        rev' xs rest = 
            case rest of
            | [] -> xs
            | x : rs -> rev' (x : xs) rs
            end
    in
        rev' [] xs
    end

def val :: (List int)
xs = mk 5     
    
    """

open System.IO

[<EntryPoint>]
let main argv = 
    //printfn "%A\n\n\n" prog
    //printfn "F# program:\n\n%A" <| transProgram prog
    use wr = new StreamWriter(@"C:\Users\alllex\Documents\Visual Studio 2012\Projects\GeneratingCode\Program.fs");
    wr.WriteLine(transProgram prog)
    0 // return an integer exit code

