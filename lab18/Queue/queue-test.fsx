#r "QueueN.dll"   
//#r "QueueP.dll"   
open Queue 


let q1 = ofList [3;5;10] 
// {front = [];rear = [10; 5; 3];}
let a,q2 = get q1
let b,q3 = get q2
let l1 = toList q3
let c,q4 = get q3
let value = isEmpty q3 //false
let value2 = isEmpty q4 //true
let q6 = q4 |> put 15 |> put 20 
let q7 = put_list [30;21] q6
let d,q8 = get q7
let e,q9 = get q8
let f,q10 = get q9
let g,q11 = get q10
let h,q12 = get q11 //exception
