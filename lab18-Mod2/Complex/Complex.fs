module Complex

type Complex = 
    | Real of float
    | Imm  of float
    | Compl of float * float

let completeCompl = function
    | Real x  -> Compl (x, 0.)
    | Imm  x  -> Compl (0., x)
    | any -> any

let scomposeCompl c = 
    match c with
    | Compl (0., x) -> Imm x
    | Compl (x, 0.) -> Real x
    | any -> any
      
let sum a b = 
    match completeCompl a, completeCompl b with
    | Compl(x1, y1), Compl(x2, y2) -> scomposeCompl (Compl(x1+x2, y1+y2))
    | _ -> failwith "Wrong Input"


let sub a b = 
    match completeCompl a, completeCompl b with
    | Compl(x1, y1), Compl(x2, y2) -> scomposeCompl (Compl(x1-x2, y1-y2))
    | _ -> failwith "Wrong Input"


let mul a b = 
    match completeCompl a, completeCompl b with
    | Compl(x1, y1), Compl(x2, y2) -> scomposeCompl (Compl((x1*x2 - y1*y2),(x1*y2 + y1*x2)))
    | _ -> failwith "Wrong Input"     

let coniug a = 
    match completeCompl a with
    | Compl(x, y) -> scomposeCompl (Compl(x, -y))
    | _ -> failwith "Wrong input"

let div a b = 
    match completeCompl a, completeCompl b with
    | Compl(x1, y1), Compl(x2, y2) -> 
        let n = x2**2. - y2**2.
        let c = mul a (coniug b)
        match c,n with
            | Compl(x,y), m -> scomposeCompl (Compl(x/m,y/m))
            | _ -> failwith "Wrong input"
    | _ -> failwith "Wrong Input"



