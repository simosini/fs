//es1
let sift n sq = Seq.filter (fun x -> x%n <> 0) sq;;
let nat = Seq.initInfinite (fun x -> x);;
let sq1 = sift 2 nat;;
Seq.take 10 sq1 |> Seq.toList;;
let sq2 = sift 3 nat;;
Seq.take 15 sq2 |> Seq.toList;;

//es2
let rec sieve sq = Seq.delay(fun() ->
    let n = Seq.head sq in 
        cons n (sift n (sieve Seq.tail)));; 