type prise = { x : float; y : float; diff : float; teta : float }    (*les coordonnées sont exprimées en m, diff est un entier entre 1 et 5, teta un angle entre 0 et 359 représentant l'angle par rapport au sol de la prise.*)
type etat = Droite | Gauche | Ramener
type bloc = { mutable state : etat; mutable prise : int; mutable chemin : (int * etat) list }

(*Fonction heuristique: renvoie un float option correspondant à la difficulté si le moov est possible, et None sinon
Prend en compte les limitations suivantes:
-La distance euclidienne entre la prise actuelle et la prochaine prise est < à 1.7m
-On ne peut pas ramener la main si on déjà ramené au moov précédent
-Les deux états sont différents : évite les relances*)
let heuristique (i:int) (j:int) (e1:etat) (e2:etat) (t_p: prise array) : float option =   (*renvoie un float entre 0 et 1 correspondant à la difficulté d'un déplacement entre deux prises
                                                                            ou None si le mouvement est impossible *)
    let p1 = t_p.(i) and p2 = t_p.(j) in
    let d = sqrt ((p2.x -. p1.x) ** 2. +. (p2.y -. p1.y) ** 2.) in
    if d < 1.7 && p2.y >= p1.y
    && ((e1 <> Ramener) || (e2 <> Ramener)) && e1 <> e2
    then Some (2.*.d**5. *. (exp (p1.diff +. p2.diff) /. 5.)) else None

(*backtracking
 *************)

(*renvoie les prises de départ et de fin (pour l'instant les prises les plus basses et hautes respectivement)*)
let prises_depart_fin (t_p: prise array) : int * int =
    let max, min = ref 0, ref 0 in
    for i = 0 to Array.length t_p - 1 do
        if t_p.(i).y > t_p.(!max).y then max := i
        else if t_p.(i).y < t_p.(!min).y then min := i
    done;
    !max, !min

(*renvoie une liste avec un seul élément, un tuple avec le moov et sa difficulté, si il est possible, une liste vide sinon*)
let moov_poss bloc (i: int) (e: etat) (t_p: prise array) : ((int * etat) * float) =
    match heuristique bloc.prise i bloc.state e t_p with
    | Some x when i <> bloc.prise
      && e <> Ramener
      && not (bloc.chemin |> List.map fst |> List.mem i) -> ((i, e),x)
    | Some x when i = bloc.prise
      && e = Ramener 
      && bloc.state <> Ramener -> ((i, e), x)
    | _ -> ((0, Ramener), 0.)

(*renvoie la liste des moovs possibles à partir de l'état d'un bloc*)
let moovs (bloc: bloc) (t_p: prise array) : ((int * etat) * float) list =
    t_p |> Array.to_seq |> Seq.fold_lefti (fun l i e ->
      Seq.cons (moov_poss bloc i Ramener t_p) l
      |> Seq.cons (moov_poss bloc i Droite t_p)
      |> Seq.cons (moov_poss bloc i Gauche t_p)) Seq.empty
    |> Seq.filter (fun e -> e <> ((0, Ramener), 0.))
    |> List.of_seq

(*Renvoie true si le bloc est terminé, false sinon*)
let complet bloc (t_p: prise array) : bool =
    (prises_depart_fin t_p |> fst) = bloc.prise && bloc.state = Ramener

(*Permet d'annuler le dernier moov effectué*)
let defaire bloc (t_p: prise array) : unit =
    bloc.prise <- bloc.chemin |> List.tl |> List.hd |> fst;
    bloc.chemin <- bloc.chemin |> List.tl

(*Permet d'appliquer un moov à partir de l'état d'un bloc*)
let applique bloc (i: int) (e: etat) (t_p: prise array) : unit =
    bloc.prise <- i; bloc.state <- e; bloc.chemin <- (i, e)::(bloc.chemin)

(*Fonction pour afficher l'état d'un moov*)
let print_state (e: etat) : unit =
    match e with
        | Droite -> print_string "Droite)"
        | Gauche -> print_string "Gauche)"
        | Ramener -> print_string "Ramener)"

(*Fonction pour afficher une méthode*)
let affiche_chemin (c: (int * etat) list) : unit =
    print_char '[';
    let rec aux l =
        match l with
        | [] -> ()
        | [x] ->  print_char '('; print_int (fst x); print_char ','; print_state (snd x); print_string "]\n"
        | t::q -> print_char '('; print_int (fst t); print_char ','; print_state (snd t); print_char ';'; aux q
    in aux c

(*Fonction qui effectue le backtracking, renvoie un tuple avec le chemin sous forme de liste et la difficulté du bloc*)
let chemin_optimal (t_p: prise array) : (int * etat) list * float =
    let sol = ref [] in
    let diff_min = ref 1000. in
    let f, d = prises_depart_fin t_p in
    let b = { state = Ramener; prise = d; chemin = [] } in
    let rec aux moov diff =
        applique b (fst moov) (snd moov) t_p;
        if diff < !diff_min then
            if complet b t_p then (sol := b.chemin; diff_min := diff);
            List.iter (fun e -> aux (fst e) (diff +. (snd e))) (moovs b t_p);
        if List.length b.chemin > 1 then
            defaire b t_p;
    in aux (d, Ramener) 0.; (List.rev !sol), (!diff_min)/.(float_of_int (List.length !sol))

let txt_to_tab file : prise array = (*parcours le fichier contenant les coordonnées des prises pour en faire un prise array*)
    let f = open_in file in
    let n = int_of_string (input_line f) in 
    let t = Array.make n {x = 0.; y = 0.; diff = 0.; teta = 0.} in
    for i=0 to n-1 do
        match String.split_on_char ' ' (input_line f) with      
            |[px; py; d; teta] -> (t.(i) <- {x = float_of_string px; y = float_of_string py; diff = float_of_string d; teta = float_of_string teta})
            |_ -> ()
    done;
    t


let chemin_to_aretes_liste (c : (int * etat) list)  =
    let rec aux l main_d main_g f=
        match l with 
        |[] -> ()
        |(p, e)::q -> (
            match e with 
            |Gauche -> (Printf.fprintf f "g %d %d\n" main_g p; aux q main_d p f)
            |Droite -> (Printf.fprintf f "d %d %d\n" main_d p; aux q p main_g f)
            |Ramener -> (Printf.fprintf f "r %d %d\n" p p; aux q p p f)
            )
    in let f = open_out "click_detection/liste_aretes.txt" in aux c 0 0 f; close_out f 


;;


let t_p = txt_to_tab "click_detection/liste_prises.txt" in
chemin_to_aretes_liste (fst (chemin_optimal t_p));
