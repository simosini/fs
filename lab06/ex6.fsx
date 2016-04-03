//remove even numbers
let rec rmEven = function
	| [] -> []
	| h::t when h%2 <> 0 -> h:: rmEven t
	| _::t -> rmEven t;;

//remove element in odd position
let rec rmOdd = function
	| [] -> []
	| [h] -> [h]
	| h1::h2::t -> h1::rmOdd t;;

 //split the list according to odd or even position
let rec split l =
   match l with
       | [] -> ([],[])
       | h::[] -> ([h],[])
       | h1::h2::tl -> 
           let (l1,l2)=split tl 
           (h1::l1, h2::l2);;

 (*func to test rmEven*)
let rec rmOddNum = function
 	| [] -> []
 	| h::t when h%2 = 0 ->  h:: rmOddNum t 
 	| _::t -> rmOddNum t;;

let prop_rmEven (l : int list) =
 	rmOddNum (rmEven l) = [];;

do Check.Quick prop_rmEven;;  	 

(*function to test rmOdd*)
let prop_rmOdd (l : 'a list) = 
	match List.length l % 2 with
	| 0 -> List.length l / 2 = List.length (rmOdd l)
	| _ -> List.length l / 2 + 1 = List.length (rmOdd l);;

do Check.Quick prop_rmOdd;;
(*function to test split*)
let rec invertSplit (l1,l2) = 
	match l1,l2 with
	| [],any | any,[] -> any 
	| h1::t1, h2::t2 -> h1::h2::invertSplit (t1,t2);;

let prop_split (l : 'a list) =
	invertSplit (split l) = l;; 

do Check.Quick prop_split;;

(*let makeSet (l : 'a list) =
	Set.ofList l;;

let prop_rmOdd2 (l: 'a list) =
	Set.isSubset (makeSet (rmOdd l)) (makeSet l);;*)	 