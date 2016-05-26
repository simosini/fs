module Stack

exception EmptyStack
type Stack<'a> = S of 'a list

let empty = S []

let push el (S s) = S (el::s)

let pop = function
    | S [] -> raise EmptyStack
    | S (h::t) -> (h, S t)

let top st = 
    let a,s = pop st
    a

let size (S s) = List.length s