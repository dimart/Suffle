module Suffle.Specification.Libs.Lists

let typeName = "List"
let consName = "Cons"
let emptName = "Nil"

let lib = """

datatype List 'a =
| Cons 'a (List 'a)
| Nil
end  

def fun :: (List 'a) -> 'a
head list =
    case list of
    | x : _ -> x
    end

def fun :: (List 'a) -> (List 'a)
tail list = 
    case list of
    | _ : xs -> xs
    end

def fun :: (List 'a) -> int
length list = 
    case list of
    | [] -> 0
    | _ : rest -> 1 + length rest
    end

def fun :: (List 'a) -> (List 'a)
rev xs =
    let 
        def fun :: (List 'a) -> (List 'a) -> (List 'a)
        rev' xs rest = 
            case rest of
            | [] -> xs
            | x : rs -> rev' (x : xs) rs
    in
        rev' [] xs
    end
    
def fun :: ('a -> 'b) -> (List 'a) -> (List 'b)               
map f xs =
    case xs of
    | [] -> []
    | x : xs' -> f x : (map f xs')
    end

def fun :: ('a -> bool) -> (List 'a) -> (List 'a)
filter f xs =
    case xs of
    | [] -> []
    | x : xs' -> 
        let 
            def val :: (List 'a)
            rest = filter f xs'
        in 
            if f x then x : rest else rest
        end 

def fun :: ('a -> 'b -> 'a) -> 'a -> (List 'b) -> 'a
foldl f acc xs =
    case xs of
    | [] -> acc
    | x : xs' -> foldl f (f acc x) xs'
    end

def fun :: ('b -> 'a -> 'a) -> (List 'b) -> 'a -> 'a
foldr f xs acc =
    case xs of
    | [] -> acc
    | x : xs' -> f x (foldr f xs' acc)
    end   

                  
   
"""