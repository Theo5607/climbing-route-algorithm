open Raylib

type prise = { x : float; y : float}

let t_p = [|{x = 0.; y = 0.}; {x = 0.; y = 1.}; {x = 1.; y = 1.}; {x = 2.; y = 2.}|]

let heuristique i j t_p =
    let p1 = t_p.(i) and p2 = t_p.(j) in
    let p = sqrt((p2.x -. p1.x) ** 2. +. (p2.y -. p1.y) ** 2.) in
    if p < 1.7 && p2.y > p1.y then Some p else None

let init_graphe =
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

let setup () =
  init_window (l*32) ((l*32)+32) "Flood it";
  set_target_fps 60;
  colors ();
  begin_drawing ();
  grid ();
  end_drawing ()

let () = setup ()