type graphe = (int*float) list array  (*array de liste d'adjacence avec des couples (voisin, poids) *)


let initialise_estimation (g: graphe) (s: int) : float array=   
    let a = Array.make (Array.length g) infinity in
    a.(s) <- 0.;
    a


let est_tendue d x y w =  (* regarde si le chemin s --> x --(w)--> y est meilleur que celui s-->y  *)
    d.(y) > d.(x) +. w

let relache d x y w =     (* remplace le chemin s --> y par le chemin s --> x --(w)--> y qui est suposé meilleur *)
    d.(y) <- d.(x) +. w

let dijkstra (g: graphe) (s:int) =            (*renvoie un couple d * pred avec d les distances et pred les predecesseurs indiquant les sommets accessibles depuis s*)
    let d = initialise_estimation g s in
    let n = Array.length g in
    let a_visiter = Fileprio.cree n 0 0. in
    for i=0 to n-1 do
        Fileprio.ajoute a_visiter i infinity
    done;
    Fileprio.diminue_priorite a_visiter s 0.;
    let pred = Array.make n None in
    while not (Tas.est_vide a_visiter) do
        let x = Fileprio.retire a_visiter in
        List.iter (fun y -> if est_tendue d x (fst y) (snd y) then (
                Fileprio.diminue_priorite a_visiter (fst y) (d.(x) +. (snd y));
                relache d x (fst y) (snd y) ;
                pred.(fst y) <- Some x  ) ) g.(x) ;
        
    done;
    d, pred
