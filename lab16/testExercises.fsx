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
//generates 3 random numbers with abs value max 100 
//Arb.generate creates a generator for the primitive type given
Gen.sample 100 3 Arb.generate<int>;; //[15; 44; -41]
//now numbers are only positive
Gen.sample 100 3 posNumberGenerator;; //[25; 5; 3] for example
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

//ok now let's try with ordered lists
let rec order = function
    | [] -> true
    | [x] -> true
    | h1::h2::t -> h1 <= h2 && (order t);;  

order [1;2;5;4];; //false
order [1;2;3];; //true

//let's generate arb int list not empty with max 50 elements
Gen.sample 50 10 (
