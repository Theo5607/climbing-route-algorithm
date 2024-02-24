open Raylib

let w = 550;;
let h = 600;;

let dx = w / 11;;
let dy = h / 18;;

let foi = float_of_int;;
let iof = int_of_float;;



let centre p pos_tab = 
  ((Array.fold_left (fun acc i -> acc + fst p.(i)) 0 pos_tab) / 4) , ((Array.fold_left (fun acc i -> acc + snd p.(i)) 0 pos_tab) / 4)

let affiche_mur p =
	clear_background Color.raywhite;
	Array.iter (fun (x,y) -> draw_circle (x*dx) ((18-y)*dy) 10. Color.orange) p

let affiche_pos p pos_tab =
	let xm,ym = centre p pos_tab in
	let m = Vector2.create (foi (xm*dx)) (foi ((18 - ym)*dy)) in

	draw_line_ex m (Vector2.create (foi ((fst p.(pos_tab.(0)))*dx)) (foi ((18-(snd p.(pos_tab.(0))))*dy))) 4. Color.blue;
	draw_line_ex m (Vector2.create (foi ((fst p.(pos_tab.(1)))*dx)) (foi ((18-(snd p.(pos_tab.(1))))*dy))) 4. Color.red;
	draw_line_ex m (Vector2.create (foi ((fst p.(pos_tab.(2)))*dx)) (foi ((18-(snd p.(pos_tab.(2))))*dy))) 4. Color.black;
	draw_line_ex m (Vector2.create (foi ((fst p.(pos_tab.(3)))*dx)) (foi ((18-(snd p.(pos_tab.(3))))*dy))) 4. Color.black




let loop p pos_tab_arr =
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
		affiche_pos p pos_tab_arr.(!i);
		end_drawing ()
	done;
	close_window () 

