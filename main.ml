type prise = { x : float; y : float}

let t_p = [|{x = 0.; y = 0.}; {x = 1.; y = 1.}; {x = 5.; y = 2.}; {x=0.; y=2.}; {x=1.; y=3.}|]

let heuristique i j t_p =              (*renvoie un float correspondant à la difficulté d'un déplacement entre deux prises*)
    let p1 = t_p.(i) and p2 = t_p.(j) in
    let p = sqrt ((p2.x -. p1.x) ** 2. +. (p2.y -. p1.y) ** 2.) in
    if p < 1.7 && p2.y > p1.y then Some p else None

let init_graphe t_p =                      (*initialise un graphe où seuls les sommets assez proches sont reliés et uniquement de bas en haut*)
    let g = Array.make (Array.length t_p) [] in
    for i = 0 to Array.length g - 1 do
        for j = 0 to Array.length g - 1 do
            if i <> j then begin
                match (heuristique i j t_p) with
                | Some x -> g.(i) <- (j, x)::g.(i)
                | None -> ()
            end
        done;
    done;
    g


let txt_to_tab file =    (*parcours le fichier contenant les coordonnées des prises pour en faire une liste et sortir la prise de début et de fin*)
    let f = open_in file in
    let l = ref [] in
    let i = ref (-1) in
    let fin = ref None in
    let deb = ref None in
    begin
        try
          while true do
            incr i;
            match String.split_on_char ' ' (input_line f) with      
            |[px; py; d] -> (l := {x = float_of_string px; y = float_of_string py}::(!l); if d= "0" then deb := Some !i else if d="5" then fin := Some !i )
            |_ -> ()
            


          done
        with End_of_file -> close_in f
    end;
    !l,!deb,!fin

let meilleur_chemin g d f =      (*renvoie une liste des arretes à emprunter pour aller du sommet d à f*)
    let dist, next = dijkstra g f in 
    let rec aux s =
        if s = f then Some []
        else
            match next.(s) with
            |None -> None
            |Some s' -> match aux s' with |None -> None | Some l -> Some (s'::l)
    in aux d




let main () =
    let t_p, d, f = txt_to_tab "click_detection/liste_prises.txt" in 
    let g = init_graphe (Array.of_list t_p) in 
    match d, f with
    |None, _ | _, None -> failwith "erreur, pas de prise de départ ou d'arrivée"
    |Some d', Some f' ->
        meilleur_chemin g d' f'
    



