#r "FsCheck"
open FsCheck
(*
Definire per mutua ricorsione le funzioni

  namesFileSys : Element list -> string list
  namesElement : Element -> string list

che restituiscono la lista dei nomi dei file e directory contenuti
nel termine passato come argomento.

Esempio:

  namesElement d1 = ["d1"; "a1"; "d2"; "a2"; "d3"; "a3"; "a4"; "d3"; "a5"]

  namesFileSys f1 = ["d1"; "a1"; "d2"; "a2"; "d3"; "a3"; "a4"; "d3"; "a5"; "f"]

Usare uno schema di mutua ricorsione della forma

 let rec namesFileSys fileSys =
    ...  posso usare namesElement ...
 and namesElement el =
    ...  posso usare namesFileSys ...


*)   
//with mutual recursion
type FileSys = Element list
and Element  = | File of string
               | Dir of string * FileSys;;

let d =
  Dir("d1",[File "a1";
            Dir("d2", [File "a2"; Dir("d3", [File "a3"])]);
            File "a4";
            Dir("d3", [File "a5"])
           ]);;
        
let f = [d;File("f")];;

let rec namesFileSys = function
    | [] -> []
    | h::tl      -> namesElement h @ namesFileSys tl
and namesElement = function
    | File f      -> [f]
    | Dir(d,elem) -> d::(namesFileSys elem);;

namesFileSys f;;
namesElement d;;

//without mutual recursion
type Elem = Files of string | Dirs of string * Elem list;;

let d1 =
  Dirs("d1",[Files "a1";
            Dirs("d2", [Files "a2"; Dirs("d3", [Files "a3"])]);
            Files "a4";
            Dirs("d3", [Files "a5"])
           ]);;
        
let f1 = [d1;Files("f")];;

let rec nameFilesys (l : Elem list) = 
    match l with
    | [] -> [] 
    | (Files f)::tl -> f::nameFilesys tl
    | (Dirs (d, any))::tl  -> (d::nameFilesys any)@ nameFilesys tl;;

let rec nameElem = function
    | Files f -> [f]
    | Dirs(d, rest) -> d::nameFilesys rest;;

nameElem d1;;
nameFilesys f1;; 

(*
Definire la funzione iterativa

    isuml :  int list -> int

che calcola la somma degli elementi di una lista di interi.*)

let isuml l =
    let rec sumlA = function
        | ([],acc)    -> acc
        | (h::tl,acc) -> sumlA (tl,h+acc)
    sumlA (l,0);;

let rec suml = function
    | [] -> 0
    | x :: xs -> x + suml xs;;

let prop_sum (l : int list) =
    suml l = isuml l;;
do Check.Quick prop_sum;;
//not iterative
let rec fact2 = function
    | 1 -> 1
    | n -> n* fact2 (n-1);;
//iterative
let ifact n =
    let rec fact = function
        | (1, acc) -> acc
        | (n,acc)  -> fact(n-1, n*acc)
    fact (n,1);;

ifact 4;;
fact2 4;;
(*
Nell'interprete dare il comando 



che, dopo ogni computazione, stampa alcuni dati sulle risorse utilizzate 
(tempo CPU, uso garbage collector, ecc.)

Provare ad eseguire delle chiamate della forma

  suml [ 1 ..K ]      
 isuml [ 1 .. K ]

con K intero grande a piacere.
Tenere presente che le liste sono costruite nello heap. *)

