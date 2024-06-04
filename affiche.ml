open Raylib;;

let w = 430;;
let h = 700;;

let dx = w / 11;;
let dy = h / 18;;

let diff_tab = [|              (*la matrice des diffs pour la moonboard*)
[|2.5;0.6;7.2;4.4;4.7;3.8;8.1;1.9;7.2;1.9;7.5|];
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
;;

(* let diff_tab = [|                  (*la matrice des diffs pour le bloc jaune*)
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
;; *)

(* let diff_tab = [|            (*bloc violet*)
  [|0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.|];
  [|0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.|];
  [|0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.|];
  [|0.; 2.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.|];
  [|0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.|];
  [|0.; 0.; 0.; 2.; 0.; 0.; 0.; 0.; 0.; 0.; 0.|];
  [|0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.|];
  [|0.; 0.; 0.; 1.; 0.; 0.; 0.; 0.; 0.; 0.; 0.|];
  [|0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.|];
  [|0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.|];
  [|0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.|];
  [|0.; 0.; 0.; 2.; 0.; 0.; 0.; 0.; 0.; 0.; 0.|];
  [|0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.|];
  [|0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.|];
  [|0.; 0.; 2.; 0.; 2.; 0.; 0.; 0.; 0.; 0.; 0.|];
  [|0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.|];
  [|0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.; 0.|];
  [|0.; 0.; 0.; 4.; 0.; 0.; 0.; 0.; 0.; 0.; 0.|]
|] *)


let foi = float_of_int;;
let iof = int_of_float;;



let centre (p : (int*int) array) (pos_tab : int array) = 
  ((Array.fold_left (fun acc i -> acc + fst p.(i)) 0 pos_tab) / 4) , ((Array.fold_left (fun acc i -> acc + snd p.(i)) 0 pos_tab) / 4)

let affiche_mur (p : (int*int) array) =
	clear_background Color.raywhite;
	for i=0 to 11 do
		draw_line (i*dx) 15 (i*dx) h Color.gray;
	done;
	for j=0 to 18 do
		draw_line 0 (j*dy) w (j*dy) Color.gray;
	done;
	draw_rectangle_lines (10*dx) (17*dy) dx dy Color.gray;
	draw_text "20cm" (10*dx) (17*dy) 1 Color.gray;
	Array.iter (fun (x,y) -> 
		draw_circle (x*dx) ((18-y)*dy) 10. Color.orange;
		draw_text (Float.to_string (diff_tab.(18 - y -1).(x))) (x*dx + 10) ((18-y)*dy) 10 Color.black
	) p

let affiche_pos (p : (int*int) array) (pos_tab : int array) (w : float) = (*p : coordonnees des prises du bloc , pos_tab : [md, ...]   , w : poids du mouvement prochain*)  
	let xm,ym = centre p pos_tab in
	let mh = Vector2.create (foi (xm*dx)) (foi ((18 - ym - 1)*dy)) in
	let mb = Vector2.create (foi (xm*dx)) (foi ((18 - ym + 1)*dy)) in

	draw_line_ex mb mh 4. Color.brown;
	draw_line_ex mh (Vector2.create (2.5 +. foi ((fst p.(pos_tab.(0)))*dx)) (foi ((18-(snd p.(pos_tab.(0))))*dy))) 4. Color.blue;
	draw_line_ex mh (Vector2.create (-2.5 +. foi ((fst p.(pos_tab.(1)))*dx)) (foi ((18-(snd p.(pos_tab.(1))))*dy))) 4. Color.red;
	draw_line_ex mb (Vector2.create (2.5 +. foi ((fst p.(pos_tab.(2)))*dx)) (foi ((18-(snd p.(pos_tab.(2))))*dy))) 4. (Color.create 20 40 130 255);
	draw_line_ex mb (Vector2.create (-2.5 +. foi ((fst p.(pos_tab.(3)))*dx)) (foi ((18-(snd p.(pos_tab.(3))))*dy))) 4. (Color.create 160 30 30 255);

	

	draw_text (String.cat "poids du prochain movement : " (Float.to_string (w))) (5*dx) 0 10 Color.black



let loop (pos_tab_arr : int array array) (p : (int*int) array) (arr_poids : float array) =
	init_window w h "ClimbingRouteAlgorithm";
	set_target_fps 60;
	let n = Array.length pos_tab_arr in
	let i = ref 0 in
	while not ( window_should_close () ) do
		if is_key_pressed Key.Right then
			i := (!i + 1) mod n
		else if is_key_pressed Key.Left then
			i := (n + !i - 1) mod n
		;
		begin_drawing ();
		affiche_mur p;
		affiche_pos p pos_tab_arr.(!i) arr_poids.(!i);
		end_drawing ();
	done;
	close_window () 


(* let plot_diff (l : (int*int) list) =
	init_window 400 400 "ClimbingRouteAlgorithm";
	set_target_fps 60;
	begin_drawing ();
	clear_background Color.raywhite;
	List.iter (fun (d,grade) -> if d <> 0 then draw_circle (d*400/25) (400 - grade*400/15) 5. Color.black) l;
	end_drawing ();
	while not (window_should_close ()) do
		()
	done;
	close_window () *)
