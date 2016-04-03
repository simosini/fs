type figura =
	| Rettangolo of float*float
	| Quadrato of float
	| Triangolo of float*float;;


let areaOpt = function
     | Rettangolo (x,y) when x > 0. && y > 0. -> Some (x * y)
     | Rettangolo (_,_) -> None
     | Quadrato l when l > 0. -> Some (l * l)
     | Quadrato _ -> None
     | Triangolo (b,h) when b > 0. && h > 0. -> Some (b * h / 2.0)
     | Triangolo _ -> None;;

let sommaArea (f1, f2) =
	match (areaOpt f1, areaOpt f2) with
		| None, _ | _, None -> None
		| Some x, Some y -> Some (x + y);;
