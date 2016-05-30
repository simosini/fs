#r "FsCheck"
open FsCheck
let permutationOf (list : int list) =
    let exists item = List.exists ((=) item)
    let sorted = List.sort list

    (sorted.Length = list.Length)
        |@ "same length" .&.
    (sorted |> List.forall (fun x -> list |> (exists x)))
        |@ "all elements exists";;

Check.Quick permutationOf;;

let posNumberGenerator = Gen.sized <| fun s -> Gen.choose(1, s);;

let positiveIntegers =
    Arb.Default.Int32()
        |> Arb.mapFilter abs (fun n -> n > 0);;

Arb.from<int> |> Arb.mapFilter abs (fun n -> n > 0) |> Arb.toGen |> Gen.sample 50 4;; 

//generates 3 random numbers with abs value max 100 
//Arb.generate creates a generator for the primitive type given
Gen.sample 100 3 Arb.generate<int>;; //[15; 44; -41]
//now numbers are only positive
posNumberGenerator |> Gen.sample 100 3 ;; //[25; 5; 3] for example
//i can use Arb.generate for any type defined
type tree = Leaf of int | Branch of tree * tree;;

//generate 10 tree with leaf max size = 50
Gen.sample 50 10 (Arb.generate<tree>);;

//now we want that leaves hold only positive numbers so
//we gotta use combinators like Arb.filter that takes a bool function
//and a Arbitrary(not a gen) and yields an Arbitrary
//Arb.from is the same as Arb.generate but yields an Arbitrary<int>
Arb.from<int>;;
//filter only positive numbers 
let ordered = Arb.filter (fun x -> x > 0) Arb.from<int>;;
//now to see this numbers 

//Arb.toGen crea il generatore con le prop arbitrtarie definite
Gen.sample 500 10 (Arb.toGen ordered);; //[2; 41; 152; 9; 183; 264; 25; 325; 17; 47]
//now let's say we want numbers > 100
Gen.sample 500 5 (Arb.toGen(Arb.filter (fun x -> x > 100) Arb.from<int>));;
//or using pipes
Arb.from<int> |> Arb.filter (fun x -> x > 100) |> Arb.toGen |> Gen.sample 500 5

//ok now let's try with ordered lists
let rec order = function
    | [] -> true
    | [x] -> true
    | h1::h2::t -> h1 <= h2 && (order (h2::t));;  

order [1;2;5;4];; //false
order [1;2;3];; //true

//let's generate arb int list not empty with max 50 elements
Arb.filter (fun l -> List.isEmpty l |> not) Arb.from<int list>;;
Gen.sample 50 10 (Arb.toGen(Arb.filter (fun l -> List.isEmpty l |> not) Arb.from<int list>));;
//let's say we also want ordered lists
let filter l = order l && (List.isEmpty l |> not);;
//this is an Arb of int list with the given properties
let ordArb =
    Arb.from<int list> |> Arb.filter filter ;;

Gen.sample 50 10 (Arb.toGen ordArb);;
 //let's say now we want the list to have at least 5 elements

let filter2 l = order l && List.length l > 4;;
let filter2Arb = Arb.from<int list> 
                    |> Arb.mapFilter List.sort filter2;;

//dont want to repeat Gen all the time
let look arb = arb |> Arb.toGen |> Gen.sample 50 10;; 

look filter2Arb;;  //ok works but would be slow cause we want the list
//generated to be oredered and at least 5 elem long: very difficult 
//to achieve innit??? that's why we add the List.sort

//------------EXERCISE 1------------------
//ok now lets try some set 
//first we define a generator that emits only ordered not empty
//list w/o reps

//number 1 we need a function to remove dups
let rec remove x = function
    | [] -> []
    | h::t when h = x -> remove x t
    | h::t -> h::remove x t;;

let rec removeDup xs = 
    match xs with
    | [] -> []
    | y::ys -> y::(remove y ys |> removeDup);;    

// we define a filter that creates sorted list w/o dups
// then we want them to be at least 5 element long

let filter3 l = List.sort l |> removeDup;;
let filter4 l = List.length l > 4;; 
let orderedNoDupArb =
    Arb.from<int list> |> Arb.mapFilter filter3 filter4;; //first function can be anything the second must be boolean
 //in this ex use we also set the maximum length to 10
Gen.sample 10 5 (Arb.toGen orderedNoDupArb);;

//ok now we have the generator and we want to use it with FsCheck
//we have to use Prop.forAll that takes an Arb and a function ('a -> 'b)
let rec insert x = function
    | [] ->                     [x]
    | h::t as l when x <= h ->  x::l
    | h::t ->                   h::insert x t;;
        
//for each list generated form arb, 
//insert an element and check is ordered
let ``if ss is an ordered set then inserting x in ss is still ordered`` arb (x: int) =
    Prop.forAll arb (fun l -> insert x l |> order)
//ok now let's check it!! remember that the function insert
//does insert element in order 
do Check.Quick <| 
    ``if ss is an ordered set then inserting x in ss is still ordered`` orderedNoDupArb;;
//so basicly what we do is define a generator with some given properties
//than we check that for every element generated holds the property
//given!! Which in this case is checking that inserting a int
//in a int list mantain the list ordered: order(insert x l)
 
//---now we want length between n and m we need to filter that---

let filter5 l = List.sort l |> removeDup;;
let filter6 l n m = List.length l >= n && List.length l <= m;; 

let orderedNoDupWithLengthArb n m =
    Arb.from<int list> |> Arb.mapFilter filter5 (fun l -> filter6 l n m);;
//let's see an example of this i put 10 to check the filter
orderedNoDupWithLengthArb 4 6 |> Arb.toGen |> Gen.sample 10 5;;
  
let ``same as before but with given min a max length`` arb (x:int) =
    Prop.forAll arb (fun l -> insert x l |> order) 
        
do Check.Quick 
    <| ``same as before but with given min a max length`` 
        (orderedNoDupWithLengthArb 4 7)

//--------------EXERCISE 2-----------------------------

//we now want to generate pairs of list of int of the same length
let pairsSameLength =
    Arb.from<(int list * int list)> 
        |> Arb.filter (fun (l1, l2) -> List.length l1 = List.length l2);;  

//now let's validate zip and unzip
let checking (l1, l2) = 
    let l = List.zip l1 l2 in 
        List.unzip l = (l1, l2);; 
//for each pair in lists generated do the checking wanted
let ``zip is the inverse of unzip`` arb =
    Prop.forAll arb checking;;   
//we pass to FsCheck the pairs of list with same length
//generated from pairSameLength and the property to check
do Check.Quick <| ``zip is the inverse of unzip`` pairsSameLength;;
//basicly we are passing a boolean property to check which
//verifies that zip is the inverse of unzip
//and we provide the lists to check that, using an 
//arbitrary with given prop(in this case pair of lists of the same length)
//this Arb will be used by FsCheck to create the proper generator
