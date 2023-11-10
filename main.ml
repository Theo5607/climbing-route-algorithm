type prise = { x : float; y : float; diff : float; teta : float }    (*les coordonnées sont exprimées en m, diff est un entier entre 1 et 5, teta un angle entre 0 et 359 représentant l'angle par rapport au sol de la prise.*)
type graphe = (int*float) list array  (*array de liste d'adjacence avec des couples (voisin, poids) *)
type etat = Droite | Gauche | Ramener
type bloc = { mutable state : etat; mutable prise : int; mutable chemin : (int * etat) list; mutable diff_min : float }


let t_p = [|{x = 0.; y = 0.; diff = 1.; teta = 0.}; {x = 1.; y = 1.; diff = 1.; teta = 0.}; {x = 5.; y = 2.; diff = 1.; teta = 0.}; {x=0.; y=2.; diff = 1.; teta = 0.}; {x=1.; y=3.; diff = 1.; teta = 0.}|]

let heuristique (i:int) (j:int) (e1:etat) (e2:etat) (t_p: prise array) : float option =   (*renvoie un float entre 0 et 1 correspondant à la difficulté d'un déplacement entre deux prises
                                                                            ou None si le mouvement est impossible *)
    let p1 = t_p.(i) and p2 = t_p.(j) in
    let p = sqrt ((p2.x -. p1.x) ** 2. +. (p2.y -. p1.y) ** 2.) in
    if p < 1.7 (*&& p2.y > p1.y*)
    && e1 != e2
    then Some ((p /. 1.7) *. ((p2.teta -. p1.teta) /. 360.) *. ((p1.diff +. p2.diff) /. 10.)) else None

(*backtracking*)
let prises_depart_fin t_p =
    let max, min = ref 0, ref 0 in
    for i = 0 to Array.length t_p - 1 do
        if t_p.(i).x > t_p.(!max).x then max := i
        else if t_p.(i).x < t_p.(!min).x then min := i
    done;
    !max, !min

let moov_poss bloc i e t_p =
    match heuristique bloc.prise i bloc.state e t_p with
    | None -> false
    | Some x -> if not (List.mem i (List.map (fun e -> fst e) bloc.chemin)) then true else false

let moovs bloc t_p =
    let l = ref [] in
    for i = 0 to Array.length t_p - 1 do
        for j = 0 to 2 do
            match j with
            | 0 -> if moov_poss bloc i Droite t_p then l := i::(!l)
            | 1 -> if moov_poss bloc i Gauche t_p then l := i::(!l)
            | 2 -> if moov_poss bloc i Ramener t_p then l := i::(!l)
            | _ -> failwith "erreur"
        done;
    done;
    List.sort_uniq (Stdlib.compare) !l

let complet bloc t_p =
    let f, d = prises_depart_fin t_p in
    if bloc.prise = f then true else false

let defaire bloc t_p =
    bloc.prise <- fst (List.hd (List.tl bloc.chemin));
    bloc.chemin <- List.tl bloc.chemin

let applique bloc i e t_p =
    bloc.prise <- i;
    bloc.state <- e;
    bloc.chemin <- (i, e)::(bloc.chemin)

let print_state e =
    match e with
        | Droite -> print_string "Droite)"
        | Gauche -> print_string "Gauche)"
        | Ramener -> print_string "Ramener)"

let affiche_chemin b =
    print_char '[';
    let rec aux l =
        match l with
        | [] -> ()
        | [x] ->  print_char '('; print_int (fst x); print_char ','; print_state (snd x); print_string "]\n"
        | t::q -> print_char '('; print_int (fst t); print_char ','; print_state (snd t); print_char ','; aux q
    in aux b.chemin

let calcul_diff chemin =
    let rec aux c diff =
        match c with
        | [] -> failwith "erreur de chemin"
        | [x] -> diff
        | t::q -> let x = heuristique (fst (List.hd q)) (fst t) (snd (List.hd q)) (snd t) t_p
                  in match x with
                  | None -> aux q diff
                  | Some y -> aux q (diff +. y)
    in (aux chemin 0.)/.(float_of_int (List.length chemin))

(*let init_graphe (t_p : prise array) : graphe =      (*initialise un graphe où seuls les sommets assez proches sont reliés et uniquement de bas en haut*)
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
    g*)

let chemin_optimal t_p =
    let sol = ref [] in
    let diff = ref 1. in
    let f, d = prises_depart_fin t_p in
    let b = { state = Ramener; prise = d; chemin = []; diff_min = 1. } in
    let rec aux moov e =
        affiche_chemin b;
        applique b moov e t_p;
        if calcul_diff b.chemin < !diff then
            if complet b t_p && calcul_diff b.chemin < !diff then sol := b.chemin; diff := calcul_diff b.chemin;
            for i = 0 to 2 do
                match i with
                | 0 -> List.iter (fun e -> aux e Droite) (moovs b t_p)
                | 1 -> List.iter (fun e -> aux e Gauche) (moovs b t_p)
                | 2 -> List.iter (fun e -> aux e Ramener) (moovs b t_p)
                | _ -> failwith "erreur"
            done;
        if List.length b.chemin > 1 then
            defaire b t_p;
    in aux d Ramener; !sol

let txt_to_tab file : prise array= (*parcours le fichier contenant les coordonnées des prises pour en faire un prise array*)
    let f = open_in file in
    let l = ref [] in
    let i = ref (-1) in
    begin
        try
          while true do
            incr i;
            match String.split_on_char ' ' (input_line f) with      
            |[px; py; d; teta] -> l := {x = float_of_string px; y = float_of_string py; diff = float_of_string d; teta = float_of_string teta}::(!l)
            |_ -> ()
            


          done
        with End_of_file -> close_in f
    end;
    (Array.of_list !l)


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


