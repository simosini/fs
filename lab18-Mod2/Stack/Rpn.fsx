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

let rpn5 = [ C 7; C 6; Op Add ; C 5; Op Prod; C 4; C 3; Op Prod; Op Add ]
// 7 6 + 5 * 4 3 * +
// (7 + 6) * 5 + 4 * 3

let toString exp = 
    match exp with
    | C x ->" " + string x + " "
    | Op Add -> " + "
    | Op Prod -> " * "
    | Op Minus -> " - "

let print_exp rpn = List.fold ( + ) "" (List.map toString rpn)

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

let a = "Eval of" + print_exp rpn1 + " is equal to " + string (evalRpn rpn1) //= 2 
let b = "Eval of" + print_exp rpn2 + " is equal to " + string (evalRpn rpn2) //= 16
let c = "Eval of" + print_exp rpn3 + " is equal to " + string (evalRpn rpn3) //= 26
let d = "Eval of" + print_exp rpn4 + " is equal to " + string (evalRpn rpn4) //= 56
let e = "Eval of" + print_exp rpn5 + " is equal to " + string (evalRpn rpn5) //= 77