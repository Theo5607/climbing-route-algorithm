
type graphe = (int*float) list array  (*array de liste d'adjacence avec des couples (voisin, poids) *)


let initialise_estimation (g: graphe) (s: int) : float array=   
    let a = Array.make (Array.length g) infinity in
    a.(s) <- 0.;
    a

let est_tendue d x y w =  (* regarde si le chemin s --> x --(w)--> y est meilleur que celui s-->y  *)
    d.(y) > d.(x) +. w

let relache d x y w =     (* remplace le chemin s --> y par le chemin s --> x --(w)--> y qui est suposé meilleur *)
    d.(y) <- d.(x) +. w

let dijkstra (g: graphe) (s:int) =   
    (* renvoie un couple d * pred avec d les distances et pred les predecesseurs indiquant les sommets accessibles depuis s*)

    let d = initialise_estimation g s in
    let n = Array.length g in
    let a_visiter = Fileprio.cree n 0 0. in
    for i=0 to n-1 do
        Fileprio.ajoute a_visiter i infinity
    done;
    Fileprio.diminue_priorite a_visiter s 0.;
    let pred = Array.make n None in

    while not (Fileprio.est_vide a_visiter) do
        let x = Fileprio.retire a_visiter in
        List.iter (fun y -> if est_tendue d x (fst y) (snd y) then (
                Fileprio.diminue_priorite a_visiter (fst y) (d.(x) +. (snd y));
                relache d x (fst y) (snd y) ;
                pred.(fst y) <- Some x  ) ) g.(x) ;
    done;
    
    d, pred





exception Trouve of int list;;

let poids_arete g i j =
    List.fold_left (fun acc (vois,w) -> if vois=j then min w acc else acc) max_float g.(i)

let greedy_backtracking (g : graphe) s f =
    let vu = Array.make (Array.length g) false in
    let rec aux x = 
        vu.(s) <- true;
        if x = f then 
            raise (Trouve [])
        else
            List.iter (fun (y,_) -> 
                try
                    if not vu.(y) then begin
                        vu.(y) <- true;
                        aux y
                    end
                with
                |Trouve c -> raise (Trouve (y::c))
            ) (List.sort (fun x y -> compare (snd x) (snd y)) g.(x))
    in
    try
        aux s;
        failwith "pas de chemin gars"
    with
    |Trouve l -> l

;;



exception PasDeChemin;;

let chemin pred x y : int list = (*renvoie le chemin allant x à y avec les distances*)
    let rec aux s =
        if x=s then
            [s]
        else
            match pred.(s) with
            |None -> raise PasDeChemin
            |Some s' -> s::(aux s')
    in
    aux y