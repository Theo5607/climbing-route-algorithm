type prise = { x : float; y : float; diff : float; teta : float }    (*les coordonnées sont exprimées en m, diff est un entier entre 1 et 5, teta un angle entre 0 et 359 représentant l'angle par rapport au sol de la prise.*)
type graphe = (int*float) list array  (*array de liste d'adjacence avec des couples (voisin, poids) *)


let t_p = [|{x = 0.; y = 0.; diff = 1.; teta = 0.}; {x = 1.; y = 1.; diff = 1.; teta = 0.}; {x = 5.; y = 2.; diff = 1.; teta = 0.}; {x=0.; y=2.; diff = 1.; teta = 0.}; {x=1.; y=3.; diff = 1.; teta = 0.}|]

let heuristique (i:int) (j:int) (t_p: prise array) : float option =   (*renvoie un float entre 0 et 1 correspondant à la difficulté d'un déplacement entre deux prises
                                                                            ou None si le mouvement est impossible *)
    let p1 = t_p.(i) and p2 = t_p.(j) in
    let p = sqrt ((p2.x -. p1.x) ** 2. +. (p2.y -. p1.y) ** 2.) in
    if p < 1.7 (*&& p2.y > p1.y*) then Some ((p /. 1.7) *. ((p2.teta -. p1.teta) /. 360.) *. ((p1.diff +. p2.diff) /. 10.)) else None


let init_graphe (t_p : prise array) : graphe =      (*initialise un graphe où seuls les sommets assez proches sont reliés et uniquement de bas en haut*)
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


let txt_to_tab file : prise array= (*parcours le fichier contenant les coordonnées des prises pour en faire un prise array*)
    let f = open_in file in
    let l = ref [] in
    let i = ref (-1) in
    begin
        try
          while true do
            incr i;
            match String.split_on_char ' ' (input_line f) with      
            |[px; py; d; teta] -> l := {x = float_of_string px; y = float_of_string py; diff = float_of_string d; teta = teta}::(!l)
            |_ -> ()
            


          done
        with End_of_file -> close_in f
    end;
    (Array.of_list !l)

let meilleur_chemin (g : graphe)  (d : int) (f : int) : int list option = (*renvoie une liste option (à l'envers) des sommets à emprunter pour aller du sommet d à f*)
    let _, pred = Dijkstra.dijkstra g d in 
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
    |Some l -> let f = open_out "click_detection/liste_aretes.txt" in aux l f; close_out f 

let prises_depart_fin t_p =
    let max, min = ref 0 in
    for i = 0 to Array.length t_p - 1 do
        if t_p.(i).x > t_p.(!max).x then !max = i
        else if t_p.(i).x < t_p.(!min).x then !min = i
    done;
    !max, !min

let main () =
    let t_p = txt_to_tab "click_detection/liste_prises.txt" in
    let f, d = prises_depart_fin t_p in
    let g = init_graphe t_p in 
    chemin_to_aretes_liste (meilleur_chemin g d f) t_p
;;
main ()


