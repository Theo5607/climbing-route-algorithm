type prise = { x : float; y : float; diff : float; teta: float }
type etat = Droite | Gauche

(*prises de départ (pieds et mains), et prise de fin*)
let pd = (0, 0, 0, 0);;
let pf = 0;; 

(*distance euclidienne*)
let d_eucli a b =
  sqrt((b.x -. a.x) ** 2. +. (b.y -. a.y) ** 2.)

(*récupérer la dernière prise*)
let get_p b =
  b |> List.hd |> fst

(*récupérer le dernier état*)
let get_e b =
  b |> List.hd |> snd

(*fonction heuristique*)
let h i j e1 e2 t =
  let p1, p2 = t.(i), t.(j) in
  let d = d_eucli p1 p2 in
  if d < 1.7 then Some (d ** 2. *. (exp (p1.diff +. p2.diff) /. 2.))
  else None

let creer_graph n = 
  Array.make n []

let remplit_graph g h t =
  Array.iteri
    (fun i a ->
      Array.iteri
        (fun j b ->
          let diff = h i j Droite Gauche t in
          match diff with
          | Some x -> g.(i) <- (j, x, diff |> Option.get) :: g.(i)
          | None -> ()
        ) g
    ) g

let backtrack (t: prise array) : (int * etat) list * float =
  let sol = ref [] in
  let diff_min = ref max_float in
  let rec aux diff =
    if diff < !diff_min then 
      if get_p sol = pf

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
