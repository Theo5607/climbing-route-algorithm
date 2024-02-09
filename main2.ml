open Yojson;;
open Yojson.Basic.Util;;
(* 
  taille du moonboard : 11x18 prises
  version : 2017 25 

*)
let dmax = 5




let json = Yojson.Basic.from_file "test.json";;
let data = json |> member "data";;

let id_to_coord (s : string) : int*int =  (*renvoie les coordonnees de la prise ex : A4 -> (0,3)=(x,y) avec origine en bas a gauche *)
  (-65 + int_of_char s.[0]) , (String.sub s 1 (-1 + String.length s) |> int_of_string) - 1



let liste_prises pb : (int*int) list = (*renvoie la liste des coordonnees des prises d'un probleme *)
  List.map (fun p -> p |> member "description" |> to_string |> id_to_coord) (pb |> member "moves" |> to_list)


let dist_prise (x1,y1) (x2,y2) =  (*renvoie la distance de manhattan de la prise 1 à la prise 2 *)
  (abs (x1 - x2)) + (abs (y1 - y2))


let faisable p pos_tab m (x2, y2) =
  dist_prise p.(pos_tab.(m)) (x2,y2) <= dmax &&       (*on verifie si la prise est atteignable *)
  (snd p.(pos_tab.(m))) <= y2 &&                     (*on garde que les mouvements vers le haut ie y2 >= y1 *)
  if m=2 || m=3 then  (*si on bouge un pied*)
    y2 <= snd p.(pos_tab.(0)) && y2 <= snd p.(pos_tab.(1))   (*on interdit d'avoir les pieds plus haut que la tete *)
  else 
    true


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

let emonde g = (*retire les voisins qui apparaissent plusieurs fois ainsi que les aretes s -> s *)
  let n = Array.length g in
  for x=0 to n-1 do 
    let vu = Array.make n false in
    vu.(x) <- true;
    g.(x) <- List.filter (fun y -> if vu.(y) then false else (vu.(y) <- true; true)) g.(x)
  done


let creer_graphe (l : (int*int) list) = (*prend une liste de coordonnées de prises et renvoie le graph des positions dont les aretes sont les mouvements possibles*) 
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
          let v = pos_tab.(m) in
          pos_tab.(m) <- i;
          g.(pos) <- (int_of_pos_tab n pos_tab)::g.(pos);      (*on ajoute au graphe une arete de pos à pos' ou pos` est la position apres avoir deplacé le membre m sur la i-eme prise *)
          pos_tab.(m) <- v
        end
      done;
    done;
  done;
  emonde g;
  g,p




