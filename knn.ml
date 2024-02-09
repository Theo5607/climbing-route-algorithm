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

let char_to_int c =
  Char.code c - 65

let calc_coords mv =
  let s = String.sub mv 1 (String.length mv - 1) in
  17 - (int_of_string s - 1), char_to_int mv.[0]

type bloc = { cote: string; diff_moy: float; dist_moy: float; nb_prises: int }

let read_json filename =
  let json = Yojson.Basic.from_file filename in

  let open Yojson.Basic.Util in

  let total = json |> member "total" |> to_int in

  let blocs = json |> member "data" |> to_list |> Array.of_list in
  let tab_blocs = Array.make total { cote = ""; diff_moy = 0.; dist_moy = 0.; nb_prises = 0; } in
  
  for i = 0 to total - 1 do
    let tab_moovs = member "moves" blocs.(i) |> to_list |> Array.of_list in
    let nb_prises = Array.length tab_moovs in
    let diff_moy = ref 0. in
    for j = 0 to nb_prises - 1 do
      let mv = member "description" tab_moovs.(j) |> to_string in
      let c1, c2 = calc_coords mv in
      diff_moy := !diff_moy +. t.(c1).(c2);
    done;
    diff_moy := !diff_moy /. (nb_prises |> float_of_int);

    let dist_moy = 0. in
    for j = 0 to nb_prises - 1 do
      for k = 0 to nb_prises - 1 do
        if i <> k then (

        )
      done;
    done;

    Printf.printf "Difficulté: %s\nDifficulté moyenne: %.2f\nNombre de prises: %d\n---\n" (member "grade" blocs.(i) |> to_string) !diff_moy nb_prises
  done;

    

