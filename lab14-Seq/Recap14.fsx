let seq1 = Seq.initInfinite(fun x -> true) 
seq1 |>Seq.take 6 |> Seq.toList

let seq2 = Seq.initInfinite(fun x -> x%2 = 0) 
seq2 |>Seq.take 6 |> Seq.toList

(*
i) Definire la funzione ricorsiva

    intFrom : int -> seq<int>

che, dato un intero n,  genera la sequenza infinita degli interi k >= n.    

ii) Usando intFrom, definire la sequenza dei infinita dei numeri naturali 0, 1, 2, ...

iii) Usando intFrom, definire la sequenza infinita int10  degli elementi k >= -10.

Da int10, usando le funzioni sulle sequenze, estrarre la lista
 
  [-4; -3; -2; -1; 0; 1; 2; 3; 4]
 *)

let intFrom n = Seq.initInfinite(fun x -> n+x)
let seq3 = intFrom 5
seq3 |> Seq.take 6 |> Seq.toList

let naturals = intFrom 0
naturals |> Seq.take 6 |> Seq.toList

let int10 = intFrom -10
int10 |> Seq.take 10 |> Seq.toList
//[-4; -3; -2; -1; 0; 1; 2; 3; 4]
int10 |> Seq.skipWhile(fun x -> x < -4) |> Seq.take 9 |> Seq.toList
//the same but usine Seq.Map
int10 |> Seq.map(fun x -> x + 6) |> Seq.take  9 |> Seq.toList
(*
Ridefinire le sequenze infinite nat1, nat2, even10, sqTrue, sqTrueFalse
senza usare Seq.initInfinite e usando la ricorsione.

Per nat1, nat2, even10 vanno  definite delle opportune funzioni generatrici (analoghe a intFrom).
*)
//funzione generatrice
let rec genFrom n = seq{
    yield n
    yield! genFrom (n+1)
    }
//naturals > 0
let nat1 = genFrom 1
nat1 |> Seq.take 5 |> Seq.toList //[1; 2; 3; 4; 5]
//naturals even numbers
let evens = genFrom 0 |> Seq.filter(fun x -> x%2 = 0)
evens |> Seq.take 5 |> Seq.toList //[0; 2; 4; 6; 8]
//evens >= 10
let even10 = evens |> Seq.filter(fun x -> x > 8)
even10 |> Seq.take 5 |> Seq.toList //[10; 12; 14; 16; 18]
#nowarn "40"
//trues
let rec sqTrue = seq{
    yield true
    yield! sqTrue
    }
sqTrue |> Seq.take 5 |> Seq.toList
//true false
let rec sqTrueFalse = seq{
    yield true
    yield false
    yield! sqTrueFalse
    }
sqTrueFalse |> Seq.take 5 |> Seq.toList

//exercises map from file es1.txt
//I define my own Seq.head and Seq.tail

let head sq = Seq.item 0 sq 
let tail sq = Seq.skip 1 sq 
//this is the same as Seq.map
let rec map f sq = seq{
    yield f (head sq)
    yield! map f (tail sq)
    } 

//using map to generate squares 
let squares = map (fun x -> x*x) (intFrom 1)
squares |> Seq.take 15 |> Seq.toList // [1; 4; 9; 16; 25; 36...

let cons el sq = seq{
    yield el
    yield! sq
    }
//using map on finite seq using Seq.empty and Seq.isEmpty
let rec finiteMap f sq = Seq.delay(fun() ->
             if Seq.isEmpty sq then Seq.empty
             else cons (f (head sq)) (finiteMap f (tail sq)))
let seqNat = [1;2;3;4]
seqNat |> finiteMap (fun x -> x*x)

//filter
//same as Seq.filter

let rec filter f sq = Seq.delay(fun() ->
    let el = head sq
    let tl = tail sq
    if f el then cons el (filter f tl)
    else filter f tl)
//the same using yield
let rec filter2 f sq = seq{
    let el = head sq
    let tl = tail sq
    if not (f el) then yield! filter2 f tl
    else 
        yield el
        yield! filter2 f tl
     }          
let mulOf3 = filter (fun x -> x % 3 = 0) (intFrom 0)            
mulOf3 |> Seq.take 10 |> Seq.toList
let mul3 = filter2 (fun x -> x % 3 = 0) (intFrom 0)            
mul3 |> Seq.take 10 |> Seq.toList

//fibonacci

let rec fibFrom a b = seq{
    yield a
    yield b
    yield! (fibFrom (a+b)(a+2*b))
    }
fibFrom 5 10 |> Seq.take 10 |> Seq.toList

let fib n = Seq.item n (fibFrom 0 1)

fib 0 ;;   // 0
fib 1 ;;   // 1
fib 2 ;;   // 1
fib 3 ;;   // 2
fib 4 ;;   // 3
fib 9 ;;  // 34

//sum of a seq
let sumSeq sq = 
    let rec aux acc index = seq{
        let n = Seq.item index sq
        yield acc + n
        yield! aux (acc+n) (index+1) 
        }
    aux 0 0

let nat = intFrom 0;;

let tot = sumSeq nat

tot |> Seq.take 15 |> Seq.toList //[0; 1; 3; 6; 10; 15; 21; 28....