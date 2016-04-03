#r "FsCheck";;
open FsCheck;;


type valV = {studente : string ; voto: int};; 
type valG = {nome : string     ; giudizio : string};;

let valuta vV =
    match vV.voto with
    | n when n < 18 -> {nome = vV.studente ; giudizio = "insufficiente"}
    | n when n < 23 -> {nome = vV.studente ; giudizio = "sufficiente"}
    | n when n < 27 -> {nome = vV.studente ; giudizio = "buono"}
    | _             -> {nome = vV.studente ; giudizio = "ottimo"};; 

let rec valutaList ( l : valV list) =
    match l with
    | []   -> []
    | h::t -> (valuta h)::valutaList t;;

let ``valutaList è una map di valuta`` (xs : valV list)  =
  List.map valuta xs = valutaList xs;;
do Check.Quick  ``valutaList è una map di valuta``;;

let rec creaValList (l1 : string list, l2 : int list) =
    match l1, l2 with
    | ([],[]) | ([],_) | (_,[]) -> []
    | (h1::t1, h2::t2)          -> {studente = h1 ; voto = h2}:: creaValList (t1, t2);;
    
let media (l : valV list) = 
    let rec sommaAndConta (tot, count) = function
        | []   -> (float tot) / (float count)
        | h::t -> sommaAndConta (tot + 1, count + h.voto) t
    if List.isEmpty l then 0.0 else sommaAndConta (0,0) l;;

let separa (l : valV list) = List.partition(fun x -> x.voto < 18) l;;

let ``due liste risultato hanno stessi elementi di vs`` (l : valV list) =
    let l1,l2 = separa l   
    List.sort l = List.sort (l1@l2);;

do Check.Quick ``due liste risultato hanno stessi elementi di vs``;;
