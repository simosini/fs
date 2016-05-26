//tail recursive
let sum l =
    let rec aux acc = function
        | [] -> acc
        | h::tl -> aux (acc+h) tl
    aux 0 l

let a = sum [1;2;3;4]

//mutual recursive 
//define one function using another mutually
//basically a function calls the other mutually
//note that the keyword REC is used only in the first
//this function is not efficient at all but it shows how
//mutually recursion works
let rec even = function 
    | 0 -> true
    | n -> odd (n-1)
and odd = function
    | 0 -> false
    | n -> even (n-1)

//can use mut rec even when defining types

type FileSys = Element list
and Element  = | File of string
               | Dir of string * FileSys;;
let d1 =
  Dir("d1",[File "a1";
            Dir("d2", [File "a2"; Dir("d3", [File "a3"])]);
            File "a4";
            Dir("d3", [File "a5"])
           ]);;

let f1 = [d1;File("f")];;

//use mut rec to define
//namesFileSys : Element list -> string list
//namesElement : Element -> string list

let rec namesFileSys lst =
    match lst with
    | [] -> []
    | h::tl -> namesElement h @ namesFileSys tl
and namesElement el =
    match el with
    | File f -> [f]
    | Dir (d,l) -> d :: (namesFileSys l) 

let names1 = namesElement d1
let names2 = namesFileSys f1

