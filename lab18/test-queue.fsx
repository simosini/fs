//#r "QueueN.dll"   
#r "QueueP.dll"   
open Queue 


let q1 = empty |> put 3 |> put 5 |> put 10 
let a,q2 = get q1
let b,q3 = get q2
let q4 = q3 |> put 15 |> put 20
let c,q4a = get q4
let d,q4b= get q4a 
let e,q5 = get q4b
let f,q6 = get q5 //exception
