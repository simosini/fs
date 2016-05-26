#r "FsCheck"
open FsCheck
    
(*
 1. Give an iterative defintion of List.length
    - via an accumulator
    - via fold
    - via continuations *)
//with acc
let lengthI l =
    let rec aux acc = function
        | []    -> acc
        | h::tl -> aux (acc+1) tl
    aux 0 l;;
//with fold
let lengthFold l = List.fold (fun x _-> x + 1) 0 l;; 
//with continuation
let lengthCont l =
  let rec aux l k =
      match l with
      | []   -> k 0
      | h::t -> aux t (fun res -> k (res + 1))
  aux l (fun x -> x)
//QuickCheck
let prop_length (l : int list) =
    lengthCont l = lengthFold l && lengthFold l = lengthI l;;
do Check.Quick prop_length;;

(*
2. Give an iterative defintion of List.Map
    - via an accumulator
    - via continuations

    Quickcheck their equivalence and informally evaluate their performances
    in terms of time and space 

*)
//with acc
let myMap f l =
    let rec aux acc = function
        | [] -> acc
        | h::t -> aux (f h::acc) t
    aux [] (List.rev l);;

//with continuation
let myMap2 f l =
    let rec aux l k =
        match l with
        | [] -> k []
        | h::t -> aux t (fun res -> k (f h::res))
    aux l (fun x -> x);;     

let prop_map f (l : int list) =
    myMap f l = myMap2 f l;;
   
do Check.Quick prop_map;;
(*
3. Give definitions  with accumulators and continuations 
of the pre/in/post-order tree traversal
functions. Quickcheck their equivalence.
*)

type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree;; 
 //regular
let rec preorder = function
    | Leaf -> []
    | Node(a, left, right) -> (a::preorder left)@preorder right;;

let t = Node(3, Node(2, Leaf, Leaf), Node(1, Leaf, Leaf));;

preorder t;;

//continuation
let preorder2 t =
    let rec aux t k = 
        match t with
        | Leaf -> k []
        | Node(a, left, right) -> aux left (fun res -> aux right (fun res2 -> k (a::res@res2)))
    aux t (fun x -> x);;
preorder2 t;;

//with acc