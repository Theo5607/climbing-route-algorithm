type arbre = Char of int | Node of arbre * arbre


let rec priorite t l =
    match t with
    |Char i -> l.(i)
    |Node(x, y) ->  (priorite x l) + (priorite y l)
;;



let construit_arbre occ =
    let f = FilePriorite.cree 256 (Char 0) 0 in 
    for i=0 to 255 do 
        if occ.(i) > 0 then
            FilePriorite.ajoute f (Char i) (-occ.(i)) 
    done;
    while (FilePriorite.taille f) >= 2 do 
        let x = FilePriorite.retire f in 
        let y = FilePriorite.retire f in 
        FilePriorite.ajoute f (Node(fst x, fst y)) (snd x + snd y)
    done;
    fst (FilePriorite.retire f )
;;


let codes t =
    let a = Array.make 256 [] in
    let rec aux tree tab =
        match tree with
        |Char i -> a.(i) <- List.rev tab
        |Node (x, y) -> ( (aux x (0::tab)); (aux y (1::tab)) )
    in aux t [];
    a 
;;