#r "FsCheck";;
open FsCheck;;

type category =
    | Daycare
    | Nursery
    | Recreation;;
 
type name =  string;;
type childDes = Desc of name * category;;

let number (cat, l : childDes list) =
     let rec aux n l =
        match l with
        | []                        -> n
        | Desc(_,c)::t when c = cat -> aux (n+1) t
        | _::t                      -> aux n t
     aux 0 l;;

let partTestNumber (l : childDes list, cat : category) =
    number(cat, l) <= List.length l;;
do Check.Quick partTestNumber;;

let cost = function
    | Daycare    -> 225.
    | Nursery    -> 116.
    | Recreation -> 110.;;
//calculate price for extra children after first one
let rec payhalf (n : name, l : childDes list) =
    match l with
    | []                               -> 0.
    | Desc(name, cat)::t when name = n -> cost cat / 2. + payhalf (n, t)
    | _::t                             -> payhalf (n, t);;
       
let rec pay (n : name, l : childDes list) =
    match l with 
    | []                               -> 0.
    | Desc(name, cat)::t when name = n -> cost cat + payhalf (n, t)
    | _::t                             -> pay (n, t);; 

//create a list of all names of a childDes list
let takeList (lst  : childDes list) = 
    let rec helper mySet = function
        | []                 -> Set.toList mySet
        | Desc(name, cat)::t -> helper (Set.add name mySet) t
    helper Set.empty lst;;

// calculate total amount spent given a childDes list
let total (l : childDes list) =
    List.sum (List.map (fun x -> pay (x ,l)) (takeList l));;

//remove name from list
let rec removeName (n : name, l : childDes list) = 
    match l with
        | []                             -> []
        | Desc(name, _)::t when name = n -> removeName (n, t)
        | (Desc(name, cat) as m)::t      -> m::removeName (n, t);; 

//same as total but with a different implementation in order to test function pay
let rec totIncome = function
    | []                      -> 0.
    | Desc(name, cat)::t as l -> pay(name, l) + totIncome (removeName (name, l));; 

let payTest (ls : childDes list) =
    total ls = totIncome ls;;     
 
do Check.Quick payTest;;