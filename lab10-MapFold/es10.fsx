#r "FsCheck"
open FsCheck
open System
//exercise 1
let concat l  = List.foldBack (@) l [];;

let prop_concat (l : int list) =
    concat l = List.concat l;;

do Check.Quick prop_concat;;

let filter p l = List.foldBack (fun h tl -> if (p h) then h::tl else tl  ) l [];; 

let prop_filter p (l : 'a list list) =
    filter p l = List.filter p l;;

do Check.Quick prop_filter;;

//exercise 2

let rec reduceBack f = function
    | []  -> failwith "empty list"
    | [x] -> x 
    | h::t -> f h (reduceBack f t);;

let prop_reduce f (l : int list) =
    match l with 
    | []  -> true 
    | ls -> reduceBack f ls = List.reduceBack f ls;;

do Check.Quick prop_reduce;;


let last l = reduceBack (fun h tl -> tl) l;;  

let reduceBack2 f l = List.foldBack f (List.tail(List.rev l)) (last l);;

reduceBack2 (+) [3;2;4];;   

let prop_reduce2 f (l : int list) =
    match l with 
    | []  -> true 
    | ls -> reduceBack2 f ls = List.reduceBack f ls;;

do Check.Quick prop_reduce2;;  

//unzip using foldBack

let unzip l = List.foldBack (fun (x1,x2) (l1,l2)-> x1::l1,x2::l2) l ([],[]);;   

let prop_unzip (l : (int * int) list) =
   List.unzip l = unzip l;;

do Check.Quick prop_unzip;; 

//List,map using foldBack
let map f l = List.foldBack (fun elemlist acc -> f elemlist::acc) l [] ;; 

let prop_map f (l : int list) =
    List.map f l = map f l;;

do Check.Quick prop_map;;



  