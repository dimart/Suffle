
#r @"M:\projects\Suffle\SuffleIDE\Parser\bin\Debug\Specification.dll"
#r @"M:\projects\Suffle\SuffleIDE\Parser\bin\Debug\ParserCombinator.dll"
#load "Auxiliary.fs"
#load "Literals.fs"
#load "Types.fs"
#load "Unary.fs"
#load "Binary.fs"
#load "Pattern.fs"
#load "Structures.fs"

open ParserCombinators.Core
open Parser.Structures

#time "on"

let x = run program """
datatype List 'a =
| Cons 'a (List 'a)
| Nil
end  

def mk :: 'a -> (List 'a)
fun x = Cons x Nil

def len :: (List 'a) -> int
fun list = 
    case list of
    | Nil -> 0
    | Cons _ rest -> len rest + 1
    end

def push :: 'a -> (List 'a) -> (List 'a)
fun x list = 
    case list of
    | Nil -> Cons x Nil
    | Cons x' xs -> Cons x' (push x xs)
    end

def rev :: (List 'a) -> (List 'a)
fun xs =
    let 
        def rev' :: (List 'a) -> (List 'a) -> (List 'a)
        fun rev' xs rest = 
            case rest of
            | Nil -> xs
            | Cons x rs -> rev' (Con x xs) rs
    in
        rev' Nil xs
    end
         
def map :: ('a -> 'b) -> (List 'a) -> (List 'b)               
fun f xs =
    case xs of
    | Nil -> Nil
    | Cons x xs' -> Cons (f x) (map f xs')
    end
    
def foldl :: ('a -> 'b -> 'a) -> 'a -> (List 'b) -> 'a
fun f acc xs =
    case xs of
    | Nil -> acc
    | Cons x xs' -> foldl f (f acc x) xs'
    end
    
def foldl :: ('b -> 'a -> 'a) -> (List 'b) -> 'a -> 'a
fun f xs acc =
    case xs of
    | Nil -> acc
    | Cons x xs' -> f x (foldr f xs' acc)
    end  
            
def xs :: (List int)
val = mk 1

def y :: (List int)
val = push 3 (push 2 xs)          
                           """
#time "off"

let check x =
 match x with
 | S(_, _) -> printfn "Succ"
 | F(pi) -> printfn "%A" <| sprintf "Ln %d Col %d" pi.Position.Line pi.Position.Column

check x