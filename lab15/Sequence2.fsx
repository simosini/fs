//es1
let sift n sq = Seq.filter (fun x -> x%n <> 0) sq;;
let nat = Seq.initInfinite (fun x -> x);;
let sq1 = sift 2 nat;;
Seq.take 10 sq1 |> Seq.toList;;
let sq2 = sift 3 nat;;
Seq.take 15 sq2 |> Seq.toList;;

//es2
let cons x s = seq { 
    yield x 
    yield! s
    };;

let rec sieve sq = Seq.delay(fun() ->
    let n = Seq.head sq in 
        cons n (sift n (sieve (Seq.tail sq))));; 

//es3
let rec intFrom n = Seq.delay (fun () -> cons n (intFrom (n+1)));;
Seq.take 10 (sieve (intFrom 2)) |> Seq.toList;; // [2; 3; 5...]

//es4
let siftC a sq = Seq.cache (sift a sq);;

let rec sieveC sq = Seq.delay(fun() ->
    let n = Seq.head sq in 
        cons n (siftC n (sieveC (Seq.tail sq))));;

let nat2 = intFrom 2;;
let primesC = Seq.cache (sieveC nat2);; 
Seq.item 200 primesC;; //very slow
Seq.item 202 primesC;; //fast due to caching of previous evaluation

//     ELENCO DEI FILE IN UNA DIRECTORY
open System.IO
let myDir = "/home/simosini/Documents/"
Directory.GetFiles myDir;;
Directory.GetDirectories myDir;;

let allFiles dir = seq{
    yield! (Directory.GetFiles dir) 
    yield! (Seq.collect (Directory.GetFiles) (Directory.GetDirectories dir))  
    };;
allFiles myDir |> Seq.length;;

//                  TAYLOR
let fact n =
    let rec aux acc = function
        | n when n < 2 -> acc
        | n -> aux (acc*n) (n-1)
    aux 1 n;;
fact 5;;

let f (x : float) (k : int) = (pown x k) / (float(fact k));;
let sum sq = Seq.fold (+) 0. sq;;

let sumSeq (sq : seq<float>)  = 
    let rec aux n =
        Seq.delay (fun() -> cons (sum (Seq.take n sq))(aux (n+1)))
     in aux 1;;
let seqT (n : float) = Seq.initInfinite(fun x -> f n x);;   
Seq.take 5 (seqT 2.) |> Seq.toList;;  
let apprTaylor (x : float) = sumSeq (seqT x);;
Seq.take 12(apprTaylor 1.0) |> Seq.toList;;
let tay10 = apprTaylor 1.0;;
Seq.item 4 tay10;;

let apprExp x delta = 
    let rec aux n =
        let a = apprTaylor x in
            if (Seq.item (n+1) a) - (Seq.item n a) < delta then Seq.item n a
            else aux (n+1)
    aux 1;;    

apprExp 1.0 0.01 ;; 
// val it : float = 2.708333333

apprExp 1.0 0.0001 ;;
// val it : float = 2.718253968

apprExp 1.0 0.0000001 ;;
// val it : float = 2.718281801

apprExp 2.5 0.0000001 ;;
// val it : float = 12.18249394


          