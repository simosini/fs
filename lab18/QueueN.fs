module Queue

type Queue<'a> =  Q of  'a list
exception EmptyQueue

let empty = Q [] 
// val empty : Queue<'a>

let put el = function
    | Q [] -> Q [el]
    | Q l  -> Q (l@[el]) 

// val put : 'a -> Queue<'a> -> Queue<'a>

let get = function
    | Q [] -> raise EmptyQueue
    | Q (h::t) -> (h, Q t)  
// val get : Queue<'a> -> 'a * Queue<'a>

let isEmpty (Q q) = (Q q = Q [])
// val isEmpty : Queue<'a> -> bool when 'a : equality

let toList (Q q) = q 
// val toList : Queue<'a> -> 'a list

let put_list l (Q q) = Q (q@l)

let ofList l = empty |> put_list l

