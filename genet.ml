open Backtracking (*utiliser mod_use*)
let xmax = 1.;;
let ymax = 1.;;
let xmin = 0.1;;
let ymin = 0.1;;
Random.self_init ();;   

let generer_mur (n : int) : prise array =
	let mur = Array.make (n) {x = xmin; y = ymin; diff = 0.; teta = 0.} in
	for i=1 to n-2 do
		let xp = xmin +. Random.float (xmax -. xmin) in
		let yp = ymin +. Random.float (ymax -. ymin) in
		let dp = float_of_int (Random.int 6) in
		let tp = Random.float 360. in
		mur.(i) <- {x = xp; y = yp; diff = dp; teta = tp}
	done;
	mur.(n-1) <- {x = xmax; y = ymax; diff = 5.; teta = 0.};
	mur

let generer_pop (m:int) (n:int) : (prise array * float) array = (*genere m murs de n prises aléatoirs*)
	let pop = Array.make m ([||], 0.) in
	for i=0 to m-1 do
		let m = generer_mur n in
		let c, d = chemin_optimal m in
		pop.(i) <- m, d
	done;
	pop

let to_mur (m : prise array) : unit =
	let f = open_out "mur.txt" in
	Printf.fprintf f "%d\n" (Array.length m);
	Array.iter (fun p ->
		Printf.fprintf f "%f %f %d %f\n" p.x p.y (int_of_float p.diff) p.teta
	) m;
	close_out f


let compare_score d1 d2 diff =
	if abs_float (d1 -. diff) > abs_float (d2 -. diff) then
		1
	else if abs_float (d1 -. diff) < abs_float (d2 -. diff) then
		-1
	else
		0


let garde_moitie pop diff= 
	Array.sort (fun (m1, d1) (m2, d2) -> compare_score d1 d2 diff) pop;
	let n = Array.length pop in
	for i= n/2 to n-1 do
		pop.(i) <- ([||], 0.)
	done;


let croisement m1 m2 =
	

	



