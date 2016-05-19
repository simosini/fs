#r "Stack.dll"
open Stack

type operator = 
    | Add
    | Prod
    | Minus

type token = 
    | Op of operator
    | C of int

type rpn = token list

let rpn1 = [ C 7 ; C 5 ;  Op Minus ] 
// 7 5 -   
// 7 - 5 

let rpn2 = [ C 10 ; C 3 ; C 2 ; Op Prod ; Op Add ]
// 10 3 2 * +
// 10 + 3 * 2  

let rpn3 = [ C 10 ; C 3 ; Op Add ; C 2 ; Op Prod  ]
// 10 3 + 2 * 
// (10 + 3) * 2  


let rpn4 = [ C 10 ; C 6 ; C 1 ; Op Minus ; Op Prod ; C 4 ; Op Minus ; C 2 ; C 5 ; Op Prod ;  Op Add ]
// 10 6 1 - * 4 - 2 5 * +
// 10 * (6 - 1)  - 4 +  2 * 5

let rec eval l (s : Stack<int>) =
    match l with
        | [] -> 
            if size s <> 1 then failwith "wrong list token"
            else top s                    
        | h::t -> match h with
                  | C x -> eval t (push x s) 
                  | Op Add -> 
                      let a,s1  = pop s
                      let b,s2  = pop s1
                      eval t (push (b + a) s2)
                  | Op Minus ->
                      let a,s1  = pop s
                      let b,s2  = pop s1
                      eval t (push (b - a) s2)
                  | Op Prod ->
                      let a,s1  = pop s
                      let b,s2  = pop s1
                      eval t (push (b * a) s2)
                  

let evalRpn l = eval l (empty : Stack<int>)

let a = evalRpn rpn1 //= 2 
let b = evalRpn rpn2 //= 16
let c = evalRpn rpn3 //= 26
let d = evalRpn rpn4 //= 56