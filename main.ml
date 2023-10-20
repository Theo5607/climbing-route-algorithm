type prise = { x : float; y : float}    (*les coordonnées sont exprimées en m*)

let t_p = [|{x = 0.; y = 0.}; {x = 1.; y = 1.}; {x = 5.; y = 2.}; {x=0.; y=2.}; {x=1.; y=3.}|]

let heuristique (i:int) (j:int) (t_p: prise array) : float option =   (*renvoie un float correspondant à la difficulté d'un déplacement entre deux prises
                                                                            ou None si le mouvement est impossible *)
    let p1 = t_p.(i) and p2 = t_p.(j) in
    let p = sqrt ((p2.x -. p1.x) ** 2. +. (p2.y -. p1.y) ** 2.) in
    if p < 1.7 && p2.y > p1.y then Some p else None


let init_graphe (t_p : prise array) : graphe =        (*initialise un graphe où seuls les sommets assez proches sont reliés et uniquement de bas en haut*)
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


let txt_to_tab file : prise array*(int option)*(int option)= (*parcours le fichier contenant les coordonnées des prises pour en faire un prise array et sortir la prise de début et de fin*)
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
    (Array.of_list !l),!deb,!fin

let meilleur_chemin (g : graphe) (d : int) (f : int) : int list option = (*renvoie une liste option (à l'envers) des sommets à emprunter pour aller du sommet d à f*)
    let dist, pred = dijkstra g d in 
    let rec aux s =
        if s = d then Some [s]
        else
            match pred.(s) with
            |None -> None
            |Some s' -> match aux s' with |None -> None | Some l -> Some (s::l)
    in aux f


let chemin_to_aretes_liste (c : int list option) (a : prise array) =
    let rec aux l f=
        match l with 
        |[] -> ()
        |[_] -> ()
        |i::j::q -> Printf.fprintf f "%f %f %f %f\n" a.(i).x a.(i).y a.(j).x a.(j).y; aux (j::q) f
    in
    match c with 
    |None -> failwith "pas de chemin possible"
    |Some l -> let f = open_out "liste_aretes.txt" in aux l f; close_out f 



let main () =
    let t_p, d, f = txt_to_tab "click_detection/liste_prises.txt" in 
    let g = init_graphe t_p in 
    match d, f with
    |None, _ | _, None -> failwith "erreur, pas de prise de départ ou d'arrivée"
    |Some d', Some f' ->
        chemin_to_aretes_liste (meilleur_chemin g d' f') t_p
    



