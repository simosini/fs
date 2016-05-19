module Queue

exception EmptyQueue
type Queue<'a> =  {front: 'a list; rear: 'a list}


let empty = {front = []; rear = []}

let put el {front = l1 ; rear = l2} = {front = l1; rear = el::l2}
    
let rec get = function
    | {front = [] ; rear = []} -> raise EmptyQueue
    | {front = [] ; rear = l} -> get {front = List.rev l ; rear = []}
    | {front = h::t ; rear = l} -> (h, {front = t ; rear = l})

let isEmpty q = (q = empty) 
// val isEmpty : Queue<'a> -> bool when 'a : equality

let toList q = q.rear@(List.rev q.front) 
// val toList : Queue<'a> -> 'a list

let put_list l queue = List.fold (fun q el -> put el q) queue l
// val put_list : l:'a list -> queue:Queue<'a> -> Queue<'a>

let ofList l = empty |> put_list l
// val ofList : l:'a list -> Queue<'a>

