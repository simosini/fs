#r "FsCheck"
open FsCheck

let f = fun x -> x + 1 ;;
let g = fun x -> x  +1 ;;

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