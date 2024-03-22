open Yojson;;

(*matrice des difficultés des prises sur la moonboard 2017 (Franck Stapel)*)
let t = [|[|2.5;0.6;7.2;4.4;4.7;3.8;8.1;1.9;7.2;1.9;7.5|];
[|7.8;9.4;8.1;5.3;8.1;5.0;4.7;6.3;10.0;9.4;4.7|];
[|3.4;10.0;0.3;6.6;4.7;8.8;6.3;7.5;5.3;1.3;7.5|];
[|7.2;6.6;8.8;4.1;7.8;7.5;7.8;6.9;9.4;7.2;1.3|];
[|2.8;9.1;2.2;9.4;4.4;7.2;5.6;5.9;6.9;8.8;7.5|];
[|8.1;3.4;3.8;7.8;1.3;5.6;2.5;7.8;4.4;4.1;2.2|];
[|5.3;5.9;6.6;5.9;6.6;3.4;6.6;8.4;6.3;7.5;8.8|];
[|3.1;9.4;6.6;6.6;4.4;8.8;3.1;5.6;6.9;1.9;4.4|];
[|5.9;6.3;3.4;1.9;8.8;7.2;3.8;3.1;6.3;8.8;5.6|];
[|8.1;5.9;6.9;2.8;6.9;5.0;7.5;6.3;6.9;2.5;6.3|];
[|4.1;4.4;9.4;7.2;4.4;2.5;4.1;5.0;1.3;6.9;8.1|];
[|7.2;6.9;5.0;7.5;6.3;4.7;1.9;6.9;1.3;6.3;7.5|];
[|2.5;7.5;6.6;6.6;5.0;5.0;4.1;8.1;6.3;6.9;6.9|];
[|0.6;8.1;9.4;3.8;7.5;3.1;7.2;2.5;6.9;8.1;4.4|];
[|2.2;4.4;3.4;2.5;5.0;5.6;4.4;5.0;6.3;4.4;5.6|];
[|4.1;3.8;9.4;7.5;3.1;8.1;6.9;7.2;6.6;6.6;7.8|];
[|5.6;3.8;9.4;2.8;7.8;3.8;7.2;5.3;5.9;3.1;5.6|];
[|3.4;4.4;8.1;5.0;7.8;4.1;6.9;6.3;6.6;6.6;2.5|]|]

(*Renvoie l'entier correspondant à un caractère*)
let char_to_int c =
  Char.code c - 65

(*convertit une cotation en entier*)
let garde_to_int g =
  match g with
  | "5+" -> 1
  | "6A" -> 2
  | "6A+" -> 3
  | "6B" -> 4
  | "6B+" -> 4
  | "6C" -> 5
  | "6C+" -> 5
  | "7A" -> 6
  | "7A+" -> 7
  | "7B" -> 8
  | "7B+" -> 8
  | "7C" -> 9
  | "7C+" -> 10
  | "8A" -> 11
  | "8A+" -> 12
  | "8B" -> 13
  | "8B+" -> 14

(*Transforme les coordonnées MoonBoard en coordonnées classiques*)
let calc_coords mv =
  let s = String.sub mv 1 (String.length mv - 1) in
  17 - (int_of_string s - 1), char_to_int mv.[0]

(*Calcule la distance de Manhatann entre deux couples*)
let dist_manh p1 p2 =
  let x1, y1 = p1 in
  let x2, y2 = p2 in
  abs(x2 - x1) + abs(y2 - y1)

(*Fonction de comparaison pour deux couples par rapport à leur première composante*)
let comp_c1_cpl a b =
  let x1, y1 = a in
  let x2, y2 = b in
  if x1 < x2 then -1
  else if x1 > x2 then 1
  else 0

type bloc = { cote: int; diff_moy: float; dist_moy: float; nb_prises: int }

let read_json filename =
  let json = Yojson.Basic.from_file filename in

  let open Yojson.Basic.Util in
  
  (*transforme le json en tableau de blocs*)
  (*on ne garde que ceux avec + de 10 repeats*)
  let blocs = json |> member "data" |> to_list |>
    List.filter (fun e ->
      member "repeats" e |> to_int >= 10
      (*&& member "isBenchmark" e |> to_bool*)) |> Array.of_list in

  let total = Array.length blocs in

  let tab_blocs = Array.make total { cote = 0; diff_moy = 0.; dist_moy = 0.; nb_prises = 0; } in
    
  (*boucle pour parcourir tout les blocs*)
  for i = 0 to total - 1 do
    let tab_moovs = member "moves" blocs.(i) |> to_list |> Array.of_list in
    let nb_prises = Array.length tab_moovs in
    let coor_prises = ref [] in

    (*calcul de la difficulté moyenne*)
    let diff_moy = ref 0. in

    for j = 0 to nb_prises - 1 do
      let mv = member "description" tab_moovs.(j) |> to_string in
      let c1, c2 = calc_coords mv in
      coor_prises := (c1, c2) :: !coor_prises;
      diff_moy := !diff_moy +. t.(c1).(c2);
    done;
    diff_moy := !diff_moy /. (nb_prises |> float_of_int);

    (*calcul de la distance moyenne*)
    let rec aux l dists =
      match l with
      | [] -> dists
      | [x] -> dists
      | t::q -> aux q ((List.fold_left 
        (fun acc e ->
          let d = dist_manh t e in
          if d < acc then d else acc) max_int q) :: dists)
    in let dist_moy = (aux (List.sort comp_c1_cpl !coor_prises) [] |> List.fold_left (fun acc e -> float_of_int e +. acc) 0.) /. (nb_prises |> float_of_int) in

    (*Printf.printf 
    "Difficulté: %s\nDifficulté moyenne: %.2f\nDistance moyenne: %.2f\nNombre de prises: %d\n---\n" (member "grade" blocs.(i) |> to_string) !diff_moy dist_moy nb_prises*)
    tab_blocs.(i) <- { cote = (member "grade" blocs.(i) |> to_string |> garde_to_int); diff_moy = !diff_moy; dist_moy = dist_moy; nb_prises = nb_prises}
  done;
  tab_blocs

let tab_blocs = read_json "problems_tipe.json"

(*---------------------*)
(*algorithme K-NN*)

(*distance entre deux blocs selon certains axes*)
let distance b1 b2 tab =
  tab.(0) *. (b2.diff_moy -. b1.diff_moy) *. (b2.diff_moy -. b1.diff_moy) +.
  tab.(1) *. (b2.dist_moy -. b1.dist_moy) *. (b2.dist_moy -. b1.dist_moy) +.
  tab.(2) *. (((b2.nb_prises - b1.nb_prises) * (b2.nb_prises - b1.nb_prises)) |> float_of_int)

(*renvoie une matrice avec les distances de chaque bloc deux à deux selon a*)
let mat_distances a tab_blocs =
  let n = Array.length tab_blocs in
  let mat = Array.make_matrix n n 0. in
  for i = 0 to ((n + 1) / 2) - 1 do
    for j = 0 to ((n + 1) / 2) - 1 do
      let d = distance tab_blocs.(i) tab_blocs.(j) a in
      mat.(i).(j) <- d;
      mat.(j).(i) <- d
    done
  done;
  mat

(*Algorithe K-nn*)
let knn k mat_distances tab_blocs i =
  let l = Array.to_list mat_distances.(i) in
  let l_triee = List.sort Stdlib.compare l in 
  let tab = Array.make 14 0 in
  List.iteri (fun i e ->
    if i <= k then tab.(e.cote - 1) <- tab.(e.cote - 1) + 1) (List.tl l_triee);
  (*
  let maxi = Array.fold_left max min_int tab in
  let g = ref 0 in
  for i = 0 to 13 do
    if tab.(i) = maxi then g := i
  done;
  !g*)
  let g = ref 0 in
  for i = 0 to 13 do
    g := !g + tab.(i) * (i + 1)
  done;
  ((float_of_int !g) /. (float_of_int k) |> int_of_float) - 1

(*Fonction swap*)
let swap t i j =
  let temp = t.(j) in
  t.(j) <- t.(i);
  t.(i) <- temp

(*split les données pour la matrice de confusion de test*)
let split tab_blocs p =
  Random.self_init ();
  let n = Array.length tab_blocs in
  let mat = Array.init n Fun.id in
  (*Mélange de Knuth*)
  for i = 0 to Array.length tab_blocs - 1 do
    let j = Random.int (i + 1) in
    swap tab_blocs i j
  done;
  let nb = int_of_float ((float_of_int p) /. 100. *. (float_of_int n)) in
  Array.init (nb - 1) (fun i -> mat.(i)), Array.init (n - nb) (fun i -> mat.(i))

let confusion k mat tab_blocs p tab =
  let t, d = split tab_blocs p in
  let conf = Array.make_matrix 14 14 0 in
  for k = 0 to Array.length t - 1 do
    let i = tab_blocs.(k).cote - 1 in
    let j = (knn k tab d k) in
    conf.(i).(j) <- conf.(i).(j) + 1
  done;
  let reussi = ref 0 in
  for i = 0 to 13 do
    reussi := !reussi + conf.(i).(i)
  done;
  ((float_of_int !reussi) /. ((float_of_int (Array.length tab_blocs)) *. (float_of_int p) /. 100.))

let main =
  let maxi = ref min_float in
  let k_opti = ref 0 in
  for i = 2 to 3 do
    let moy = ref 0. in
    for j = 0 to 29 do
      moy := !moy +. (confusion i tab_blocs 10 [|1.;1.;1.|]);
    done;
    moy := !moy /. 30.;
    if !moy > !maxi then (maxi := !moy; k_opti := i)
  done;
  !k_opti, !maxi
