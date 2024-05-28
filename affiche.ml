open Raylib;;

let w = 430;;
let h = 700;;

let dx = w / 11;;
let dy = h / 18;;

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
	draw_line_ex mb (Vector2.create (2.5 +. foi ((fst p.(pos_tab.(2)))*dx)) (foi ((18-(snd p.(pos_tab.(2))))*dy))) 4. Color.black;
	draw_line_ex mb (Vector2.create (-2.5 +. foi ((fst p.(pos_tab.(3)))*dx)) (foi ((18-(snd p.(pos_tab.(3))))*dy))) 4. Color.black;

	

	draw_text (String.cat "poids du prochain movement : " (Float.to_string (w *. 100. /. 23.))) (5*dx) 0 10 Color.black



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
