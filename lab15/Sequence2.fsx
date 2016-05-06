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
        cons n (sieve (sift n (Seq.skip 1 sq))));; 

let primes = sieve (Seq.initInfinite (fun n -> n + 2));;
Seq.item 700 primes;;
//es3
let rec intFrom n = Seq.delay (fun () -> cons n (intFrom (n+1)));;
Seq.take 10 (sieve (intFrom 2)) |> Seq.toList;; // [2; 3; 5...]

//es4
let siftC a sq = Seq.cache (sift a sq);;

let rec sieveC sq = 
    Seq.cache
        (Seq.delay(fun() ->
            let n = Seq.head sq in 
                cons n (sieveC (siftC n (Seq.skip 1 sq)))));;
                    
let primesC = Seq.cache primes;; 
Seq.item 200 primesC;; //very slow increase number slowly
Seq.item 1000 primesC;; //super slow if u strasrts from here
Seq.item 1001 primesC;; // immediate because of caching

//     ELENCO DEI FILE IN UNA DIRECTORY
open System.IO
let myDir = "/home/simosini/Documents/FS"
Directory.GetFiles myDir;;
Directory.GetDirectories myDir;;

let rec allFiles dir = seq{
    yield! Directory.GetFiles dir 
    yield! Seq.collect allFiles (Directory.GetDirectories dir)  
    };;
allFiles myDir |> Seq.length;;

//                  TAYLOR
let fact n =
    let rec aux acc = function
        | n when n < 2 -> acc
        | n -> aux (acc*n) (n-1)
    aux 1 n;;
fact 5;;

let taylEl x k = pown x k / (float(fact k));;


let sumSeq (sq : seq<float>)  = 
    let rec aux n =
        Seq.delay (fun() -> cons (Seq.reduce(+) (Seq.take n sq))(aux (n+1)))
     in aux 1;;
//same as sumSeq but using yield
let sumSeq2 (sq : seq<float>) =
    let rec aux n = seq{
        yield Seq.reduce(+) (Seq.take n sq)
        yield! aux (n + 1)
        }
    in aux 1;;
//same again but more efficient
//supposed the sequence is incremental starting from n 
let rec sums acc n = seq{ 
    yield acc
    yield! sums (acc + n) (n + 1.)
    };;         
//passing f to use it in taylor approx  
let rec sums2 f x (acc : float) n = seq{ 
    yield acc
    yield! sums2 f x (acc + (f x n)) (n + 1)
    };;
//first 1.is value of x in e^x
//second 1. is the starting value to accumulate which is x^0/0! first term of the expansion
//last 1 is the index of the series the index 0 is already contained in acc
Seq.take 10 (sums2 (taylEl) 1. 1. 1) |> Seq.toList;; 
                  

Seq.take 5 (sumSeq [0.; 1.; 2.; 3.; 4.; 5.]) |> Seq.toList;;
Seq.take 5 (sumSeq2 [0.; 1.; 2.; 3.; 4.; 5.]) |> Seq.toList;;
Seq.take 5 (sums 0. 1.) |> Seq.toList;; // [0.0; 1.0; 3.0; 6.0; 10.0]

let seqT n = Seq.initInfinite(fun x -> taylEl n x);;   
Seq.take 5 (seqT 2.) |> Seq.toList;;  
let apprTaylor x = sumSeq (seqT x);;
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

apprExp 2.5 0.0000001 ;;a<
// val it : float = 12.18249394
 
// LAZYAND

let t1 =  lazy ( 10 +  2 ) ;;

t1.Force ();;

//without lazy keyWord
let lazyAnd lb1  lb2 =
   match lb1() with
        | false -> lazyFalse
        | _ -> lb2;;

let e1 = (fun x ->   5 > 0) :   (unit -> bool);; 
let e2 = (fun x ->  10 < 0)  :  (unit -> bool);;
let e3 = (fun x ->  2/0 > 0) :  (unit -> bool);;

lazyAnd e1 e2 ();;  //false dont evaluate second term 

//with lazy keyword
let lazyTrue2 = lazy true;;
let lazyFalse2 = lazy false;;

lazyTrue2.Force ();;

let lazyAnd2 (lb1 : Lazy<bool>) (lb2 : Lazy<bool>) = 
    match lb1.Force () with
        | false -> lazyFalse2
        | _ -> lb2 ;;

let e1 = lazy(5 > 0);; 
let e2 = lazy(10 < 0);; 
let e3 = lazy(2/0 > 0);;

let w3 = lazyAnd2 e2 e3;; //val w3 : Lazy<bool> = Value is not created.
w3.Force ();; //val it : bool = false