module FSet

type FSet<'a when 'a : equality> = S of ('a -> bool) 

let empty = S(fun _ -> false)

let contains el (S set) = set el

let singleton el = S(fun x -> x = el)

let add el set = S(fun x -> contains x (singleton el) || contains x set)  

let union set1 set2 = S(fun x -> contains x set1 || contains x set2)

let ofList l = List.fold (fun set el -> add el set) empty l     

let toList s max =  
    [ for x in 0 .. max do if (contains x s) then yield x ]

let b = empty |> add 5 |> add 6 |> add 2
let lst = toList b 7 // int list = [2; 5; 6]

