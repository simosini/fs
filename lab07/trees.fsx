 (*Beware this file does not work on Fsharpi because of indentation problems
 though the code works fine, you just cannot check it  *)

 
 
#r "FsCheck";;
open FsCheck
Type 'a binTree = Null | Node of 'a * 'a binTree * 'a binTree;;

let t2 = Node (2, Null, Node ( 4 , Null, Null ) );; 
let t7 = Node (7, Null, Node (10, Null, Node ( 13 , Null, Null ))) ;; 
let t8 = Node ( 8, Node ( 11, Null, Null), Null ) ;; 
let t5 = Node ( 5, t7, t8 ) ;;
let t9 = Node ( 9, Null,   Node (12, Null, Null) );
let t6 = Node( 6, t9, Null) ;;
let t3 = Node(3, t5, t6 ) ;;
let t1 = Node (1, t2, t3 );; 

let rec intToFloatTree = function
	| Null -> Null
	| Node(a, left, right) -> Node( float a, intToFloatTree left, intToFloatTree right);;

let bt7 = intToFloatTree t7;;

let rec inorderToList = function
	| Null -> []
	| Node(a, left, right) -> inorderToList left @ (a::inorderToList right);; 

let t1inord = inorderToList t1;;

let rec preorderToList = function
	| Null -> []
	| Node(a, left, right) -> a::preorderToList left @ (preorderToList right);;

let prop_visit (btree : int binTree) =
  let l1 = inorderToList btree  |> List.sort  
  let l2 = preorderToList btree |> List.sort
  l1 = l2 ;;

Check.Quick prop_visit;;

let rec search (e,tree) = 
	match tree with
	| Null -> false
	| Node (a, left, right) -> a = e || search (e,left) || search (e,right);;

search(2,t1) ;;
search(3,t1) ;;
search(4,t1) ;; 
search(5,t1) ;; 
search(100,t1) ;; 

let rec mem (x , ls) =  
    match ls with 
    | [] -> false 
    | y::ys -> x=y || mem (x, ys) ;;

let prop_search ( x : int, btree : int binTree) =
  	let l1 = inorderToList btree
  	mem(x,l1) ==> search(x,btree) ;;  // se x appartiene a l1, valuta  search(x,btree)

Check.Quick  prop_search ;;

let rec filterToList (p, tree)  =
	match tree with
	| Null -> []
	| Node (a, left, right) -> if p a then a::filterToList (p, left) @ filterToList (p, right)
							   else filterToList (p, left) @ filterToList (p, right);; 
let filterToList2 (p, tree) =
	let l = inorderToList tree
	let rec filter acc lst = 
		match lst with
		| [] -> acc
		| h::t when p h -> filter (h::acc) t
		| _::t -> filter acc t
	filter [] (List.rev l);;  

let isEven n = n%2 = 0;;
let isSmall n = n < 5;;

let t1even = filterToList (isEven,t1 ) ;;

let t1small = filterToList (isSmall,t1 ) ;;

let rec countLeaf = function
	| Null -> 0
	| Node (_, Null, Null) -> 1
	| Node (_, left, right) -> countLeaf left + (countLeaf right);; 

let rec countNode = function
	| Null -> 0 
	| Node (_, Null, Null) -> 1
	| Node (_, left, right) -> 1 + (countNode left) + (countNode right);; 

let count2 tree = (countNode tree, countLeaf tree);;

//with only one function more efficient
let rec count = function
	| Null -> (0, 0)
	| Node (_, Null, Null) -> (1, 1)
	| Node (_, left, right) -> 
		let n1, n2 = count left
		let m1, m2 = count right
		(1 + n1 + m1, n2 + m2);; 

let n1 = count t2 ;;  
let n2 = count t7 ;;  
let n3 = count t6 ;;  
let n4 = count t1 ;;

let rec depthToList (h, tree) =
	match h, tree with
	| _, Null -> []
	| 0, Node (a, _, _) -> [a]
	| n, Node(_, left, right) -> depthToList (h-1, left) @ depthToList (h-1, right);;

let d0 = depthToList (0, t1) ;;   
let d1 = depthToList (1, t1) ;; 
let d2 = depthToList (2, t1) ;; 
let d3 = depthToList (3, t1) ;; 
let d4 = depthToList (4, t1) ;; 
let d5 = depthToList (5, t1) ;; 
let d6 = depthToList (100, t1) ;;

type direction = L | R;;

let rec getElement (path, btree) = 
	match path, btree with
	| _, Null -> None
 	| [], Node(a, _, _) -> Some a
 	| h::t, Node(a, left, _) when h = L -> getElement (t, left)
 	| _::t, Node(a, _, right) -> getElement (t, right);;

let g1 = getElement ( [], t1 ) ;; 
let g2 = getElement ( [L], t1 ) ;; 
let g3 = getElement ( [L ; L], t1 ) ;; 
let g4 = getElement ( [L ; R], t1 ) ;; 
let g5 = getElement ( [L ; R ; R], t1 );;
let g6 = getElement ( [R ; L ; L], t1 ) ;; 
let g7 = getElement ( [R ; L ; L ; L ; L], t1 ) ;; 
let g8 = getElement ( [R ; L ; L ; R ; R], t1 ) ;; 

(*binary search tree*)

let rec insert (el, btree) =
	match btree with
	| Null -> Node (el, Null, Null)
	| Node (a, _, _) when a = el -> btree
	| Node (a, left, right) when el < a -> Node (a, insert (el, left), right) 
	| Node (a, left, right) -> Node (a, left, insert (el, right));; 

let rec insertFromList (ls, btree) =
	match ls with
	| [] -> btree
	| h::t -> insertFromList (t, insert (h, btree));;

let intList = [ 20 ; 10 ; 60 ; 15 ; 40 ; 100 ; 30 ; 50 ; 70 ; 35 ; 42 ; 58 ; 75 ; 32 ; 37 ] ;;
let strList1 = [ "pesca" ; "banana" ; "uva" ; "albicocca" ; "nocciola" ; "ribes" ] ;;
let strList2 = [ "limone" ; "ciliegia" ; "mela" ; "pera" ; "noce"  ] ;;

let intTree = insertFromList (intList, Null);;
let strTree1 = insertFromList (strList1, Null);;
let strTree2 = insertFromList (strList2, strTree1);;

let rec search1 (el, btree) =
	match btree with 
	| Null -> false
	| Node (a, _, _) when a = el -> true
	| Node (a, left, _) when el < a -> search1 (el, left)
	| Node (a, _, right) -> search1 (el, right);;

let searchPath (el, btree) =
	let rec aux acc btree =
		match  btree with
		| Null -> []
		| Node (a, _, _) when a = el -> acc@[a] 
		| Node (a, left, _) when el < a -> aux (acc@[a]) left
		| Node (a, _, right) -> aux (acc@[a]) right
	aux [] btree;;

let rec min = function
	| Null -> None
	| Node (a, Null, _) -> Some a
	| Node (a, left, _) -> min left;;

min intTree ;;   
min strTree2;;   
min ( Null : int binTree) ;; 

let rec subtree (el, btree) =
	match btree with
	| Null -> Null
	| Node (a, left, right) as tree when a = el -> tree
	| Node (a, left, _) when el < a -> subtree (el, left)
	| Node (_, _, right) -> subtree (el, right);;

let m1 = min ( subtree(10, intTree) )  ;;    
let m2 = min ( subtree(15, intTree) )  ;;   
let m3 = min ( subtree(60, intTree) )  ;;   
let m4 = min ( subtree(40, intTree) ) ;;
let m5 = min ( subtree(100, intTree) ) ;;
let m6 = min ( subtree(1000, intTree) ) ;;  
let m7 = min ( ( subtree ("limone",  strTree2) ) ) ;;  
let m8 = min ( ( subtree ("ribes",  strTree2) ) )  ;;