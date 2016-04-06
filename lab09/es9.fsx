#r "FsCheck"
open FsCheck

let f = fun x -> x + 1 ;;
let g = fun x -> x  +1 ;;

//exercise 2
let rec map fx = function
    | [] -> []
    | h::t -> fx h::map fx t;;

let l1 = [1..10];; 

let l2 = map (fun x -> x*x) l1;;

let l3 = map (fun x -> if x%2 = 0 then (x, "pari") else (x, "dispari")) l1;;

let names = [ ("Mario", "Rossi") ; ("Anna Maria", "Verdi") ; ("Giuseppe", "Di Gennaro")] ;;

let names1 = map (fun (x,y) -> "Dott. " + x + " " + y) names;; 

let prop_map f (ls : int list) =
    map f ls = List.map f ls;;

do Check.Quick prop_map;;

let prop_map_len f (ls: int list) =
    List.length (map f ls) = List.length ls;;

do Check.Quick prop_map_len;;

//exercise 3
let rec filter pred = function
    | [] -> []
    | h::t -> if pred h then h::filter  pred t else filter pred t;;

let mul3 n = filter (fun x -> x%3 = 0) [1..n];;

let prop_filter pred (ls : int list) =
    filter pred ls = List.filter pred ls;;

do Check.Quick prop_filter;;

let prop_filter_len pred (ls: int list) =
    List.length (filter pred ls) <= List.length ls;;

do Check.Quick prop_filter_len;;

//ecercise 4

let rec filter1 pred = function
    | [] -> [],[]
    | h::t -> let (l1,l2) = filter1 pred t 
              if pred h then (h::l1,l2) 
              else (l1,h::l2);;

let p1 = filter1 (fun x -> x % 3 = 0) [1..20];;

let multNonMult n = filter1 (fun x -> x%3 = 0) [1..n] ;; 

let p2 = multNonMult 16;;

let prop_filter1_len pred (ls : int list) =
    let (l1,l2) = filter1 pred ls
    List.length (l1@l2) = List.length ls;;

do Check.Quick prop_filter1_len;;

let prop_filter1_app pred (ls : int list) =
    let (l1,l2) = filter1 pred ls
    List.sort (l1@l2) = List.sort ls;;
  
do Check.Quick prop_filter1_app;;     

//exercise 5
let divisori n = filter (fun x -> n%x = 0) [1..n];;
let d100 = divisori 100;;

let isPrime n =
    List.length (divisori n) = 2;;
   
let a = isPrime 25;;
let b = isPrime 97;;
//a list of all prime number from 2 and n
let primeList  n = filter isPrime [2..n];;
let pl = primeList 100;;