
open Translator
open Suffle.Parser

let lists = 
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

def fun :: ('a -> 'b) -> List 'a -> List 'b
map f xs =
    case xs of
    | [] -> []
    | x : xs -> (f x) : map f xs
    end 
    
def val :: List int
list = [1, 2, 3, 4, 5] 

def val :: int
length = len list

def val :: List int
doubled = map (\x -> x * 2) list

def fun :: ('a -> 'b -> 'a) -> 'a -> List 'b -> 'a
foldl f acc xs =
    case xs of
    | [] -> acc
    | x:xs -> foldl f (f acc x) xs
    end
    
def val :: int
sum = foldl (\ acc x -> acc + x) 0 [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] 

    """

let factorial =
 parse """

def fun :: int -> int
fact n =
   case n of
   | 0 -> 1
   | _ -> n * fact (n - 1)
   end

def val :: int
f10 = fact 10

 """

let gcd =
 parse """

def fun :: int -> int -> int
gcd a b = if (a == 0) then b else gcd (b % a) a end

def val :: int
g1 = gcd 7 15

def val :: int
g2 = gcd 9 15

 """

open System.IO

[<EntryPoint>]
let main argv = 
    //printfn "%A\n\n\n" prog
    //printfn "F# program:\n\n%A" <| transProgram prog
    use wr = new StreamWriter(@"C:\Users\alllex\Documents\Visual Studio 2012\Projects\GeneratingCode\Program.fs");
    wr.WriteLine(transProgram lists)
    0 // return an integer exit code

