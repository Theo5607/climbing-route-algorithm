open Yojson.Basic.Util;;
(* 
  taille du moonboard : 11x18 prises
  version : 2017 25 

*)

let iof = int_of_float;;
let foi = float_of_int;;




(*[|[|0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.|];
  [|0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.|];
  [|0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.|];
  [|0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.|];
  [|0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.|];
  [|0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.|];
  [|0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.|];
  [|0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.|];
  [|0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.|];
  [|0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.|];
  [|0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.|];
  [|0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.|];
  [|0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.|];
  [|0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.|];
  [|0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.|];
  [|0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.|];
  [|0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.|];
  [|0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.|]|]*)

let diff_tab = [|                  (*la matrice des diffs pour le bloc jaune*)
  [|0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.|];
  [|0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.|];
  [|0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.|];
  [|0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.|];
  [|0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.|];
  [|0.; 0.; 0.; 0.; 1.; 0.; 0.; 0.; 0.; 0.; 0.|];
  [|0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.|];
  [|0.; 0.; 0.; 1.; 0.; 0.; 0.; 0.; 0.; 0.; 0.|];
  [|0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.|];
  [|0.; 0.; 3.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.|];
  [|0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.|];
  [|0.; 1.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.|];
  [|0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.|];
  [|0.; 0.; 0.; 1.; 0.; 0.; 0.; 0.; 0.; 0.; 0.|];
  [|0.; 0.; 0.; 7.; 0.; 0.; 0.; 0.; 0.; 0.; 0.|];
  [|0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.|];
  [|0.; 0.; 7.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.|];
  [|0.; 0.; 0.; 7.; 0.; 0.; 0.; 0.; 0.; 0.; 0.|]
  |]
;;


let grade_to_int = function
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
  |_ -> 0


let id_to_coord (s : string) : int*int =  (*renvoie les coordonnees de la prise ex : A4 -> (0,3)=(x,y) avec origine en bas a gauche *)
  (-65 + int_of_char s.[0]) , (String.sub s 1 (-1 + String.length s) |> int_of_string) - 1



let liste_prises pb : (int*int) list = (*renvoie la liste des coordonnees des prises d'un probleme *)
  List.map (fun p -> p |> member "description" |> to_string |> id_to_coord) (pb |> member "moves" |> to_list)


let dist_prise (x1,y1) (x2,y2) =  (*renvoie la distance euclidienne de la prise 1 à la prise 2 *)
  let d1 = (x1 - x2) |> abs |> foi in
  let d2 = (y1 - y2) |> abs |> foi in
  sqrt ((d1*.d1) +. (d2*.d2))


let ecartement_vertical p pos_tab m i =  
  let temp = pos_tab.(m) in    (*on regarde la qualite de la position future*)
  pos_tab.(m) <- i;
  let v = abs ((max (snd p.(pos_tab.(0))) (snd p.(pos_tab.(1)))) - (min (snd p.(pos_tab.(2))) (snd p.(pos_tab.(3)))))
  in
  let delta = 7. -. (foi v) in
  pos_tab.(m) <- temp;
  1. -. (Float.exp ( (-0.5)*.delta*.delta))



let ecartement_horizontal p pos_tab m i =
  let temp = pos_tab.(m) in    (*on regarde la qualite de la position future*)
  pos_tab.(m) <- i;
  let v = abs ((max (fst p.(pos_tab.(0))) (fst p.(pos_tab.(2)))) - (min (fst p.(pos_tab.(1))) (fst p.(pos_tab.(3))))) in
  let delta = 5. -. (foi v) in
  pos_tab.(m) <- temp;
  1. -. (Float.exp ( (-0.5)*.delta*.delta))



let centre p pos_tab : int*int = (*renvoie la pos moyenne des 4 prises tenues*)
  ((Array.fold_left (fun acc i -> acc + fst p.(i)) 0 pos_tab) / 4) , ((Array.fold_left (fun acc i -> acc + snd p.(i)) 0 pos_tab) / 4) 


let faisable p pos_tab m (x2, y2) = (*bool si le move n'est pas aberant*)

  dist_prise (centre p pos_tab) (x2,y2) <= 6.5  && (*on verifie si la prise est atteignable *)

  (snd p.(pos_tab.(m))) <= y2 &&                     (*on garde que les mouvements vers le haut ie y2 >= y1 *)    
  
  if m=2 || m=3 then  (*si on bouge un pied*)
    y2 <= (snd p.(pos_tab.(0))) - 2 && y2 < (snd p.(pos_tab.(1))) - 2   (*on interdit d'avoir les pieds trop haut par rapport aux mains*)
  else 
    true


let dist_poids (x1,y1) (x2,y2) =   (*renvoie un poids entre 0 et 1 de combien l'ecart entre les 2 prises est proche d'un déplacement habituel*)
                                    (*calculé comme ...*)
  let delta = (dist_prise (x1,y1) (x2,y2)) -. 2.5 in
  1. -. (Float.exp ( (-1.)*.delta*.delta))


let croise_poids p pos_tab m (x2,_) =   (*malus de 1 si on croise les mains ou les pieds*)
  if (m = 0 && x2 < fst p.(pos_tab.(1))) 
  || (m = 1 && x2 > fst p.(pos_tab.(0))) 
  || (m = 2 && x2 < fst p.(pos_tab.(3)))  
  || (m = 3 && x2 > fst p.(pos_tab.(2)))  then
    1.
  else
    0.


let prise_poids m (x,y) = 
  let diff = diff_tab.(18 - y -1).(x) /. 10. in
  if m = 0 || m = 1 then 
    diff
  else
    0.5 *. diff


let poids p pos_tab m i =  (* p : coordonnees des prises du bloc
                              pos_tab : [md, mg, pd, pg]
                              m : indice du membre deplacé dans [0,3]
                              i : indice dans p de la nouvelle prise utilisee
*)

  3. *. (dist_poids p.(pos_tab.(m)) p.(i)) +. 10. *. (croise_poids p pos_tab m p.(i)) 
  +. 2. *. (ecartement_vertical p pos_tab m i) +. 4. *. (ecartement_horizontal p pos_tab m i) +. 4. *. (prise_poids m p.(i)) 



let int_of_pos_tab n t =  (*renvoie l'entier representé en base n par t*)
  let k = ref 1 in
  let acc = ref 0 in
  for i=0 to 3 do
    acc := !acc + !k * t.(i);
    k := !k * n
  done;
  !acc

let pos_tab_of_int n pos = (*renvoie pos en base n avec le bit de poids faible à gauche, t = [md, mg, pd, pg] *)
  let v = ref pos in
  let t = Array.make 4 0 in
  for i=0 to 3 do
    t.(i) <- !v mod n;
    v := !v / n
  done;
  t


let prises_mains_depart pb = (*renvoie les coordonnees des prises de depart et de fin du bloc*)
  let l = pb |> member "moves" |> to_list in
  let depart = ref [] in
  List.iter(fun p -> 
    if p |> member "isStart" |> to_bool then
      depart := (p |> member "description" |> to_string |> id_to_coord)::!depart
  ) l;
  let d' = match List.sort (fun a b -> if fst a > fst b then 1 else -1) !depart with
    |[a] -> (a,a)
    |[a;b] -> (a,b)
    |_ -> failwith "prises de départ non comformes"
  in d'

let prises_mains_fin pb =
  let l = pb |> member "moves" |> to_list in
  let fin = ref [] in
  List.iter(fun p -> 
    if p |> member "isEnd" |> to_bool then
      fin := (p |> member "description" |> to_string |> id_to_coord)::!fin
  ) l;
  let f' = match List.sort (fun a b -> if fst a > fst b then 1 else -1) !fin with
    |[a] -> (a,a)
    |[a;b] -> (a,b)
    |_ -> failwith "prises de fin non comformes"
  in f'


let emonde g = (*retire les voisins qui apparaissent plusieurs fois ainsi que les aretes s -> s *)
  let n = Array.length g in
  for x=0 to n-1 do 
    let vu = Array.make n false in
    vu.(x) <- true;
    g.(x) <- List.filter (fun (y,_) -> if vu.(y) then false else (vu.(y) <- true; true)) g.(x)
  done


let liste_prises_augmentee pb = (*rajoute une prise de pieds pour le départ*)
  let l = liste_prises pb in
  let d1,d2 = prises_mains_depart pb in
  ((fst d1 + fst d2) / 2, 0)::l


let creer_graphe pb : (int * float) list array * (int * int) array = 
  (*prend un probleme et renvoie le graph des positions dont les aretes sont les mouvements possibles*)
  let l = liste_prises_augmentee pb in
  let p = Array.of_list l in  (*array des prises du bloc*)

  let n = List.length l in
  let npow4 = n*n*n*n in  (*theoriquement il y a n⁴ positions possibles sur le mur en ayant tous les membres sur une des n prises*)
  let g = Array.make npow4 [] in   
  (*on represente chaque position par un array [md, mg, pd, pg] qui donne l'indice dans p de la prise sur laquelle chacun des 4 membres
  est situé, cette position est ensuite encodée par un int entre 0 et n⁴-1 avec int_of_pos_tab pour limiter la taille du graphe et faciliter l'implementation*)

  for pos=0 to npow4 - 1 do    (*position en base n *)
    let pos_tab = pos_tab_of_int n pos in
    for m=0 to 3 do           (*m = indice du membre déplacé*)
      for i=0 to n-1 do       (*i = indice de la nouvelle prise dans p*)
        if faisable p pos_tab m p.(i) then begin
          let dist = poids p pos_tab m i in
          let v = pos_tab.(m) in
          pos_tab.(m) <- i;
          g.(pos) <- ((int_of_pos_tab n pos_tab), dist)::g.(pos);      (*on ajoute au graphe une arete de pos à pos' ou pos` est la position apres avoir deplacé le membre m sur la i-eme prise *)
          pos_tab.(m) <- v
        end
      done;
    done;
  done;
  emonde g;
  g,p


let pos_depart pb p n = (*renvoie l'int indiquant la position de depart*)
  let d1,d2 = prises_mains_depart pb in
  (*d1 : main gauche (x plus petit)  d2 : main droite*)
  let p1 = ref (-1) in
  let p2 = ref (-1) in  (*indice dans p de d1 et d2*)
  Array.iteri (fun i c -> 
  if c = d1 then
    p1 := i
  ;
  if c = d2 then
    p2 := i
  ) p;
  [|!p2; !p1; 0; 0|] |> int_of_pos_tab n


let find_best_end_pos pb p n dist =  (*renvoie la position de fin la plus proche trouvée depuis l'array des distances dist*)
  let f1,f2 = prises_mains_fin pb in
  (*f1 : main gauche (x plus petit)  f2 : main droite*)
  let p1 = ref (-1) in
  let p2 = ref (-1) in  (*indice dans p de d1 et d2*)
  Array.iteri (fun i c -> 
  if c = f1 then
    p1 := i
  ;
  if c = f2 then
    p2 := i
  ) p;   (*trouve l'indice associé a ces prises*)
  let i_min = ref 0 in
  let n2 = n*n in
  let n4 = n2*n2 in
  for k=0 to (n4 - 1 - !p2 - n*(!p1)) / n2 do  (*i mod n² = p2 + n*p1*)
    let i = !p2 + n*(!p1) + k * n2 in    (*toutes les pos où les mains sont sur les prises de fin*)
    if dist.(i) < dist.(!i_min) then begin
      i_min := i
    end
  done;
  !i_min




let affiche_pb pb =  (*calcul l'array des positions (x,y) , p et l'array des poids des mouvements depuis ces positions puis affiche*) 
  let g, p = creer_graphe pb in
  let n = Array.length p in
  let dep = pos_depart pb p n in
  let dist, pred = Dijkstra.dijkstra g dep in
  let fin = find_best_end_pos pb p n dist in
  let c = (Dijkstra.chemin pred dep fin) |> List.rev |> Array.of_list in
  let arr_sommets = c |> Array.map (pos_tab_of_int n)  in
  let nm = Array.length arr_sommets in
  let arr_poids = Array.init nm (fun i -> Dijkstra.poids_arete g c.(i) c.( (i+1) mod nm )) in


  Affiche.loop arr_sommets p arr_poids 
;;

let calcul_arr_diff pb =
  let g, p = creer_graphe pb in
  let n = Array.length p in
  let dep = pos_depart pb p n in
  let dist, pred = Dijkstra.dijkstra g dep in
  let fin = find_best_end_pos pb p n dist in
  let c = (Dijkstra.chemin pred dep fin) |> List.rev |> Array.of_list in
  let nm = Array.length c in
  let arr_poids = Array.init (nm-1) (fun i -> Dijkstra.poids_arete g c.(i) c.(i+1)) in
  arr_poids

let diff_bloc pb =
  try 
    let a = calcul_arr_diff pb in
    let n = a |> Array.length |> foi in
    let s = Array.fold_left (fun acc w -> acc +. w) 0. a in
    s /. n
  with
    |Dijkstra.PasDeChemin -> 0.

let diff_blocs i json : (float*int) list =
  let rec aux l i r = match l,i with
    |_,0 -> r
    |[],_ -> r
    |pb::q, i -> aux q (i-1) (((diff_bloc pb), (pb |> (member "grade") |> to_string |> grade_to_int))::r)
  in aux (json |> member "data" |> to_list) i []
;;


let fwrite l =
  let f = open_out "data.txt" in
  List.iter (fun (d,grade) -> Printf.fprintf f "%f %d\n" d grade) l;
  close_out f
;;


(* let json = Yojson.Basic.from_file "blocs/problems MoonBoard Masters 2017 25.json" in
json |> diff_blocs 500 |> fwrite; *)

let json = Yojson.Basic.from_file "blocs/jaunecov.json" in
  json |> affiche_pb;