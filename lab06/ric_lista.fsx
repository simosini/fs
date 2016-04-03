let rec rmEven l =
    match l with
        | [] -> []
        | h::tl when (h%2)=0 -> h::rmEven tl
        | h::tl -> rmEven tl

let rec rmOddPos l =
    match l with
        | [] -> []
        | h1::h2::tl -> h1::rmOddPos tl
        | h::tl -> h::rmOddPos tl

let rec split l =
    match l with
        | [] -> ([],[])
        | h::[] -> ([h],[])
        | h1::h2::tl -> 
            let (l1,l2)=split tl 
            (h1::l1, h2::l2)

let rec cmpLength(l1,l2) =
    match (l1,l2) with
        | ([],[]) -> 0
        | (ls,[]) -> 1
        | ([],ls) -> -1
        | (h1::tl1,h2::tl2) -> cmpLength(tl1,tl2)


let rec remove(x, l) = 
    match l with
        | [] -> []
        | h::tl when h=x -> remove(x,tl)
        | h::tl -> h::remove(x,tl)

let rec downto0 x =
    match x with
        | 0 -> [0]
        | _ -> xdownto0(x-1)

let rec upto x =
    match x with
        | 0 -> [0]
        | _ -> upto(x-1)@[x]



