#r "FsCheck"
open FsCheck
#nowarn "40"

let cons x s = seq { 
    yield x 
    yield! s
    };;

let sq1 = seq{
    yield 1
    yield 2
    yield 3
    };;

let sq2 = cons 0 sq1;;
Seq.item 1 sq1;;
Seq.item 1 sq2;;

let append s1 s2 = seq{
    yield! s1
    yield! s2
    };;

let sq3 = append sq1 sq2;;
Seq.item 5 sq3 = Seq.item 2 sq2;;

//exctracts 1st element from a seq
let head sq = Seq.item 0 sq;;
head sq1;;
head sq2;;

head sq1 = Seq.head sq1;;
//extracts all elements but 1st
let tail sq = Seq.skip 1 sq;;
tail sq1;;


let s1= seq {
  yield 0  // yield genera un elemento
  yield 1
  yield 2
  yield 3
  };;

// sq1 : seq<int> definisce la sequenza seq [ 0; 1; 2; 3 ]

let s2 = seq{
  yield 100
  yield! sq1  // yield! aggiunge tutti gli elementi di sq1
  yield 200
  yield! sq1
  };;

head s2;;
tail s2;;
append s1 s1;;
cons 100 s2;;
s2;;

//nat : sequenza dei numeri naturali 0, 1, 2, ...
let nat =  Seq.initInfinite (fun x -> x);;
Seq.take 3 nat;; //3
//nat1: sequenza dei numeri naturali senza il numero 5
let nat1 =  Seq.initInfinite (fun x -> x) |> Seq.filter (fun x -> x <> 5);; 
Seq.item 4 nat1;; //4
Seq.item 5 nat1;; //6
//nat2: sequenza dei numeri naturali in cui il numero 5 e' sostituito da -5
let nat2 = Seq.initInfinite (fun x -> if x = 5 then -x else x);;
Seq.item 5 nat;; //-5
Seq.item 6 nat2;; //6
//even10 : sequenza dei numeri pari n >= 10  
let even10 =  Seq.initInfinite (fun x -> x*2 + 10);; 
Seq.item 0 even10;; //10
//sqTrue : sequenza costante true, true, true, ....
let sqTrue =   Seq.initInfinite (fun x -> x = x);;
Seq.take 3 sqTrue;; //[true; true; true]
//sqTrueFalse: sequenza true, false, true, false, true, false, ...
let sqTrueFalse =  Seq.initInfinite (fun x -> x%2 = 0);; 
Seq.take 4 sqTrueFalse;; //[true; false; true; false]
(*
i) Definire la funzione ricorsiva

    intFrom : int -> seq<int>

che, dato un intero n,  genera la sequenza infinita degli interi k >= n.    

ii) Usando intFrom, definire la sequenza dei infinita dei numeri naturali 0, 1, 2, ...

iii) Usando intFrom, definire la sequenza infinita int10  degli elementi k >= -10.

Da int10, usando le funzioni sulle sequenze, estrarre la lista
 
  [-4; -3; -2; -1; 0; 1; 2; 3; 4]
 *)
//i
let rec intFrom n = Seq.delay (fun () -> cons n (intFrom (n+1)));; 
//ii
let rec natural = intFrom 0;;
Seq.take 3 natural;; //[0; 1; 2]
//iii
let fromMinus10 = intFrom -10;;
Seq.take 3 fromMinus10;; // [-4; -3; -2]
Seq.take 15 fromMinus10 |> Seq.skip 6;; // [-4; -3; -2; -1; 0; 1; 2; 3; 4]
//iv the same as iii but starting with 10
let int10 = intFrom 10;;
Seq.take 3 int10;; //[10; 11; 12]
Seq.take 9 int10 |> Seq.map (fun x -> x - 14);; // [-4; -3; -2; -1; 0; 1; 2; 3; 4]
(*
Ridefinire le sequenze infinite nat1, nat2, even10, sqTrue, sqTrueFalse
senza usare Seq.initInfinite e usando la ricorsione.

Per nat1, nat2, even10 vanno  definite delle opportune funzioni generatrici (analoghe a intFrom).
*)

// genera sequenza infinita n, n+1, n+2, ... senza il numero 5
let rec intFrom1 n = Seq.delay (fun () -> cons n (intFrom1 (n+1))) |> Seq.filter (fun x -> x <> 5);;  

let rnat1 = intFrom1 0;;
Seq.item 4 rnat1;; //4
Seq.item 5 rnat1;; //6
// genera sequenza infinita n, n+1, n+2, ... in cui 5 e' sostituito da -5
let rec intFrom2 n = Seq.delay (fun () -> if n <> 5 then cons n (intFrom2 (n+1))
                                          else cons -5 (intFrom2 (n+1)));;
  
let rnat2 = intFrom2 0;;
Seq.item 5 rnat2;; //-5
Seq.item 6 rnat2;; //6

// genera sequenza infinita n, n+2, n+4, ....
let rec intFrom3 n = Seq.delay (fun () -> cons n (intFrom3 (n+2)));;
   
let  reven10 = intFrom3 10;;
Seq.item 0 reven10;; //10
// genera sequenza infinita true, true, true, true, ...
let rec rsqTrue = Seq.delay(fun () -> cons true (rsqTrue));;
Seq.take 3 rsqTrue;; //[true; true; true]  
// genera sequenza infinita true, false, true, false, ...
let rec rsqTrueFalse =  Seq.delay(fun () -> cons true (cons false (rsqTrueFalse)));;
Seq.take 4 rsqTrueFalse;; //[true; false; true; false]

//        EXERCISES MAP

//i
let rec map  sq f = Seq.delay(fun() -> cons (f (Seq.head sq))(map (Seq.tail sq) f));;

//ii
let seq1 = map (intFrom 0) (fun x -> x * x);;
Seq.take 15 seq1;; // [0; 1; 4; 9; ...]

//iii
let rec finiteMap sq f = Seq.delay(fun() -> 
           if Seq.isEmpty sq then Seq.empty
           else cons (f (Seq.head sq))(map (Seq.tail sq) f));;
 
let seq2 = seq [1;2;3;4];;       
let seq3 = finiteMap seq2 (fun x -> x * x);;
Seq.take 4 seq3;;

//          EXERISES FILTER
//i
let rec filter f sq = Seq.delay(fun () -> 
    let el = Seq.head sq in
        if f el then cons el (filter f (Seq.tail sq))
        else  filter f (Seq.tail sq));;

//ii
let mulOf3 = filter (fun x -> x % 3 = 0) (intFrom 0);
Seq.take 20 mulOf3 |> Seq.toList;; // [0; 3; 6; 9; ...]
//use toList to display all thr numbers of the seq

//          FIBONACCI

//I

let rec fibFrom n1 n2 = Seq.delay (fun() ->
    cons n1 (cons n2 (fibFrom (n1+n2) (2*n2 + n1))));;
Seq.take 10 (fibFrom 5 10) |> Seq.toList;; //[5; 10; 15; 25; 40...]

//ii
let fib n = 
    if n = 0 then 1
    else Seq.item (n-1) (fibFrom 1 2);; 

fib 0 ;;   // 1
fib 1 ;;   // 1
fib 2 ;;   // 2
fib 3 ;;   // 3
fib 4 ;;   // 5
fib 10 ;;  // 89

//          SEQUENZA DELLE SOMME DI UNA SEQUENZA

//i
let sum sq = Seq.fold (+) 0 sq;;

let sumSeq sq = 
    let rec aux n =
        Seq.delay (fun() -> cons (sum (Seq.take n sq))(aux (n+1)))
     in aux 1;;

//ii
Seq.take 15 (sumSeq (intFrom 0)) |> Seq.toList;; //[0; 1; 3; 6; 10...]
