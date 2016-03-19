type figura = 
   | Rettangolo of float * float
   | Quadrato of float   
   | Triangolo of float * float;;

let area fig =
   match fig with
   | Rettangolo(b,h) ->  
        match (b,h) with
            | (b, h) when b>0. && h>0. -> Some ( b * h )
            | (_,_) -> None
    | Quadrato lato ->
        match lato with
            | l when l>0. -> Some ( l * l )
            | _ -> None
    | Triangolo(b,h)  ->  
        match (b,h) with
            | (b, h) when b>0. && h>0. -> Some (( b * h ) / 2.)
            | (_,_) -> None;;
  
let printArea fig=  
    match area fig with
        | Some k -> "area = "+(string k)
        | None -> "errore" ;;


let sommaArea (fig1, fig2)=
    match (area fig1, area fig2) with 
        | (Some x, Some y) -> Some (x+y)
        | (_,_) -> None ;;  
          