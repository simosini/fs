#r "Stack.dll"
open Stack

let s1 = (empty : Stack<int>) |> push 3 
            |> push 5 |> push 10
let len = size s1 // 3
let a,s2 = pop s1 // 10,[5;3]
let b,s3 = pop s2 // 5,[3]
let c = top s3 // 3
let d,s4 = pop s3 //3,[]
let e,s5 = pop s4 //exception
let f = top s4 //exception
