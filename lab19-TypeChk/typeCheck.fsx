
//exercise on type checking

type t = INT | LSTINT

type exp =
  K of int
  | Plus of exp * exp
  | Nil
  | Cons of exp * exp
  | Hd of exp
  | Tl of exp

let rec tpck e =
    match e with
    | K _ -> Some INT
    | Plus (e1,e2) -> 
        let (t1,t2) = (tpck e1, tpck e2)
        if t1 = Some INT && t2 = Some INT then Some INT
        else None
    | Nil -> Some LSTINT
    | Cons (e1,e2) -> 
        let (t1,t2) = (tpck e1, tpck e2)
        if t1 = Some INT && t2 = Some LSTINT then Some LSTINT
        else None
    | Hd e -> if (tpck e) = Some LSTINT then Some INT
              else None
    | Tl e -> if (tpck e) = Some LSTINT then Some LSTINT
              else None

#r "FsCheck"
open FsCheck

let test size len =
  let exps = (Gen.sample size len Arb.generate<exp>)   
  List.map2 (fun x y -> printf "%A has type %A\n" x y) exps (List.map tpck exps)

do Check.Quick (test 20 5);;

let myPrint x = 
    match (tpck x) with
    | Some t -> (printf "%A has type %A\n" x (Some t))
    | None ->   (printf "%A is not typable\n" x )

let test2 size len =
  let exps = (Gen.sample size len Arb.generate<exp>)   
  List.map myPrint exps 

do Check.Quick (test2 20 5);;

let myPrint2 x = 
    match (tpck x) with
    | Some t -> (printf "%A has type %A\n" x t)
    | None ->   (printf "%A is not typable\n" x )

let test3 size len =
  let exps = (Gen.sample size len Arb.generate<exp>)   
  List.map myPrint2 exps 

do Check.Quick (test3 20 5);;

//ok we do the same but using exceptions instead of options

exception TlErr of (exp * t)
exception HeadErr of (exp * t)
exception NotAInt of (exp * exp * t)
exception ConsErr of (exp * exp * t)

let rec tpckExc e =
    match e with
    | K _ -> INT
    | Plus (e1,e2) -> 
        let (t1,t2) = (tpckExc e1, tpckExc e2)
        if t1 = INT && t2 = INT then INT
        else raise (NotAInt (e1,e2,LSTINT))
    | Nil -> LSTINT
    | Cons (e1,e2) -> 
        let (t1,t2) = (tpckExc e1, tpckExc e2)
        if t1 = INT && t2 = LSTINT then LSTINT
        else raise (ConsErr (e1,e2,INT))
    | Hd e -> if (tpckExc e) = LSTINT then INT
              else raise (HeadErr (e, INT))
    | Tl e -> if (tpckExc e) = LSTINT then LSTINT
              else raise (TlErr (e, INT))

