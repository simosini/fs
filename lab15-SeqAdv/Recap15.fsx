let nat = Seq.initInfinite (fun x -> x)
//given n it filters out all his multiples
let sift n sq = Seq.filter (fun x -> x%n <> 0) sq
let sq1 = sift 2 nat
let sq2 = sift 3 nat
sq1 |> Seq.take 10 |> Seq.toList //[1; 3; 5; 7...
sq2 |> Seq.take 10 |> Seq.toList //[1; 2; 4; 5; 7...

//use sift to implement Eratosthen sieve
let rec sieve sq =seq{
    let first = Seq.head sq 
    let rest =  Seq.tail sq
    yield first
    yield! sieve (sift first rest)
    }

//takes ages.....
sieve (Seq.initInfinite(fun x -> x + 2))
    |> Seq.take 1000 |> Seq.toList 

//files exercises
open System.IO
let myDir = "/home/simosini/Documents/FS/lab18-Mod2"
Directory.GetFiles(myDir)
Directory.GetDirectories(myDir)


let rec allFiles dir = seq{
    yield! Directory.GetFiles dir 
    yield! Seq.collect allFiles (Directory.GetDirectories dir) 
    }
allFiles myDir |> Seq.toList