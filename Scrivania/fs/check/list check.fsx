#r "FsCheck";;

open FsCheck;;

let rec rmEven l =
    match l with
        | [] -> []
        | h::tl when (h%2)=0 -> rmEven tl
        | h::tl -> h::rmEven tl

let prop_rmEven (l:int list)=
    let x=rmEven l
    let rec p x=
        match x with
            | [] -> true
            | h::ls when h%2<>0 -> p ls
            | _ -> false
    in p x;;

let prop_rmEven1 (ls:int list)=
    rmEven(rmEven ls)=rmEven ls;;

do Check.Quick prop_rmEven1;;

let rec rmOddPos l =
    match l with
        | [] -> []
        | h1::h2::tl -> h1::rmOddPos tl
        | h::tl -> h::rmOddPos tl

let prop_rmOddPos (l:int list)=
    match (List.length l)%2 with
        | 0 -> (l |> rmOddPos |> List.length)=(List.length l)/2 
        | _ -> (l |> rmOddPos |> List.length)=((List.length l)/2)+1;;
 
do Check.Quick prop_rmOddPos;;

let rec split l=
    match l with
        | [] ->([],[])
        | [h] -> ([h],[])
        | h1::h2::ls -> 
            let (x1,x2)=split ls 
            (h1::x1, h2::x2)

//l=ricomposizione split l
let prop_split (l:int list)=  
    let rec rsplit (l1, l2)=
        match (l1, l2) with
            | ([],_)->[]
            | ([x],[])->[x]
            | (h1::l1, h2::l2) -> h1::h2::rsplit(l1,l2)
            | (_,[])->[] // IN TEORIA NON CI ENTRA MAI PER COME È FATTA LA SPLIT
    l=rsplit(split l);;

do Check.Quick prop_split;;



