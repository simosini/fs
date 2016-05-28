#r "FsCheck"
open FsCheck

type boolex = 
    | Const of bool
    | Var of char
    | Neg of boolex
    | And of boolex * boolex
   
type environment = char list

let rec eval exp (env : environment) =
    match exp with
    | Const c -> c
    | Var v -> List.exists (fun x -> x = v) env
    | Neg b -> not (eval b env)
    | And (b1,b2) -> (eval b1 env) && (eval b2 env)

type ifex =
    | K of bool
    | X of char
    | If of ifex * ifex * ifex

let rec ifeval exp (env : environment) =
    match exp with
    | K k -> k
    | X x -> List.exists (fun y -> y = x) env
    | If(e1,e2,e3) ->if ifeval e1 env then ifeval e2 env
                     else ifeval e3 env

let rec bool2if exp =
    match exp with
    | Const c -> K c
    | Var v -> X v
    | Neg e -> If(bool2if e, K false, K true)
    | And (e1,e2) -> If(bool2if e1, bool2if e2, K false)

let prop_boolex exp env =
    ifeval (bool2if exp) env = eval exp env

do Check.Quick prop_boolex;;

// SEQUENCE

//i
let nat = Seq.initInfinite id
nat |> Seq.take 10 |> Seq.toList

let seq1 = seq{
    yield! [0 ; 1 ; 2 ; 0 ; 3 ; 4 ; 4 ;  3 ; 1]
    yield! Seq.initInfinite (fun x -> x + 5)
    }
seq1 |> Seq.take 13 |> Seq.toList

let seq2 =
    let rec aux n = seq{
        yield n
        yield n
        yield! aux (n+1)
        }
    aux 0     
seq2 |> Seq.take 10 |> Seq.toList

let seq3 = Seq.collect(fun x -> Seq.take (x + 2) nat) nat  
seq3 |> Seq.take 20 |> Seq.toList

// ii

let rec distinct sq = seq{
   let el = Seq.item 0 sq
   yield el
   yield! distinct (Seq.filter(fun x -> x <> el) sq)
   }

distinct seq1 |> Seq.take 20 |> Seq.toList
distinct seq2 |> Seq.take 20 |> Seq.toList
distinct seq3 |> Seq.take 20 |> Seq.toList

// iii

let isEqual n seq1 seq2 =
    Seq.forall2(fun x y -> x = y) (Seq.take n seq1) (Seq.take n seq2) 

isEqual 20 nat (distinct seq1) ;;
isEqual 20 nat (distinct seq2) ;;
isEqual 20 nat (distinct seq3) ;;

