#r "FsCheck"
open FsCheck

//es 1.1 
let rec dotProduct l1 l2 = 
    match l1,l2 with
    | [],[] -> 0
    | l,[] | [],l -> failwith "lists must have same length"
    | h1::t1,h2::t2 -> h1*h2 + (dotProduct t1 t2);;
     
//es 1.2
let dotProduct2 l1 l2 = 
    List.fold2 (fun acc x y -> acc + x*y) 0 l1 l2;; 

//tail recursive
let dotProduct3 l1 l2 =
    let rec aux acc l1 l2 =
        match l1,l2 with
        | [],[] -> acc
        | l,[] | [],l -> failwith "lists must have same length"
        | h1::t1,h2::t2 -> aux (acc + h1*h2) t1 t2
    aux 0 l1 l2;;

//es 1.3
let prop_dotProduct (l1 : int list) (l2 : int list) =
    List.length l1 = List.length l2 ==>
        lazy(dotProduct l1 l2 = dotProduct2 l1 l2;;

do Check.Quick prop_dotProduct;;

//es 2.1
let rec takeWhile pred = function
    | [] -> []
    | h::t when pred h -> h::takeWhile pred t
    | any -> [];;

let takeWhile2 pred l =
    let rec aux pred acc = function
        | [] -> List.rev acc
        | h::t when pred h -> aux pred (h::acc) t
        | any -> List.rev acc
    aux pred [] l;;

//es 2.2
let rec dropWhile pred = function
    | [] -> []
    | h::t when pred h -> dropWhile pred t
    | any -> any;; 
     
//es 2.3
let prop_while pred (l : int list) =
   takeWhile pred l = takeWhile2 pred l && (takeWhile pred l)@(dropWhile  pred l) = l;;

do Check.Quick prop_while;;

//es 3.1
let safeDiv a b =
    match a,b with
    | None,_ | _,None | _,Some(0) -> None
    | Some x,Some y -> Some (x/y);;
 
//es 3.2
let optMapBinary f a b =
    match a,b with
    | None,_ | _,None -> None
    | Some a,Some b -> Some (f a b);; 

//es 3.3
let optPlus a b = optMapBinary (+) a b;;
let optTimes a b = optMapBinary ( * ) a b;;

//es 4.1
type form =
    | Const of bool
    | Neg of form
    | And of form*form
    | Or of form*form;;

//es 4.2
let rec eval = function
    | Const x ->  x
    | Neg (x) -> not (eval x)
    | And(x,y) -> eval x && (eval y)
    | Or(x,y) -> eval x || (eval y);;  
   
let a = eval (Neg(And(Const true, Const false)));; 

//es 4.3
let rec toString = function
    | Const x -> string x
    | Neg x -> "(" + "not " + toString x + ")"
    | And(x,y) -> "(" + toString x + " /\ " + toString y + ")"
    | Or(x,y) -> "(" + toString x + " \/ " + toString y + ")";;

//es 4.4
let main expr = toString expr + " = " + string(eval expr);; 

let m = main (Neg(Or(Const true, Const false)));;

