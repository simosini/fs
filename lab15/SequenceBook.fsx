let odd = Seq.initInfinite(fun x -> 2*x + 1);;
Seq.take 10 odd |> Seq.toList;;
let fact n = 
    let rec aux acc = function
        | x when x < 2. -> acc
        | x -> aux (acc*x) (x-1.)
    aux 1. n;;
let factSeq = Seq.initInfinite fact;;
Seq.take 6 factSeq |> Seq.toList;;

let factSeq2 =
    let rec aux acc n = seq{
        yield acc
        yield! aux (n*acc) (n+1)
        }
    aux 1 1;;
Seq.take 6 factSeq2 |> Seq.toList;;

let subSq i n sq = Seq.skip (i - 1) sq |> Seq.take n;;

subSq 6 3 factSeq2;;
(*
unfold takes a function, that given one argument compute (f x) where f is the first argument of the option tuple while 
the second argument is the next imput of the function f, and an argument which is the first input of the fun.
So in the next example unfold takes a closure (the fun x ....) and the starting argument 1, the closure must be of the type
fun x -> Some(f x, next input of f)   
*)
let tri_seq = Seq.unfold (fun x -> Some (x * (x + 1) / 2 , x + 1)) 1;; //seq of the sum of the first n int numbers

tri_seq |> Seq.take 10 |> Seq.toList;; // [1; 3; 6; 10; 15; 21; 28; 36; 45; 55]
let nextTerm x k =   x**k / fact k;;
let taylor f x = Seq.unfold (fun k -> Some (f x k , k + 1.)) 0.;; //seq of all terms of the expansion of e^x
taylor nextTerm 1.0 |> Seq.take 6 |> Seq.toList;; //[1.0; 1.0; 0.5; 0.1666666667; 0.04166666667; 0.008333333333]

let rec taylorExp n = seq{
    yield taylor nextTerm 1. |> Seq.take n |> Seq.sum
    yield! taylorExp (n + 1)
    };; //taylor expansion as the sum of the first n terms 

taylorExp 1 |> Seq.take 10 |> Seq.toList;; //[1.0; 2.0; 2.5; 2.666666667; 2.708333333; 2.716666667 ...]

//those 3 functions does exactly the same thing
Seq.reduce (+) (tri_seq |> Seq.take 10 );;
Seq.fold (+) 0 (tri_seq |> Seq.take 10 );;
Seq.sum (tri_seq |> Seq.take 10 );;

let rec alternate n = seq{
    yield -n
    yield n
    yield! alternate (n+1)
    };;

let cons x s = seq{
    yield x
    yield! s
    };;

alternate 1 |> cons 0 |> Seq.take 6 |> Seq.toList;;
