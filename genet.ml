open Backtracking (*utiliser mod_use*)
let xmax = 3.8;;
let ymax = 3.8;;
let xmin = 0.1;;
let ymin = 0.1;;
Random.self_init ();;   

exception Stop

let bonne_distance (mur : prise array) (n : int) (x  : float) (y : float) (dmin : float) (dmax : float) =
	let pas_trop_proche = ref true in
	let pas_trop_loin = ref false in
	for i=0 to n-1 do
		let d = sqrt ((x -. mur.(i).x) ** 2. +. (y -. mur.(i).y) ** 2.) in
		pas_trop_proche := !pas_trop_proche && (d >= dmin);                (*la nouvelle prise doit etre espacée d'au moins dmin avec toutes les prises*)
		pas_trop_loin := !pas_trop_loin || (d <= dmax)		               (*la nouvelle prise doit etre espacée de moins de dmax pour au moins une prise*)
	done;
	!pas_trop_loin && !pas_trop_proche


let generer_mur (n : int) (dmin : float) (dmax : float) : prise array =
	let mur = Array.make (n) {x = xmin; y = ymin; diff = 0.; teta = 0.} in
	mur.(n-1) <- {x = xmax; y = ymax; diff = 5.; teta = 0.};
	let nprises = ref 1 in
	while !nprises < n-1 do
		let xp = xmin +. Random.float (xmax -. xmin) in
		let yp = ymin +. Random.float (ymax -. ymin) in
		let diffp = float_of_int (Random.int 5) in
		let tetap = Random.float 360. in
		if bonne_distance mur (!nprises + 1) xp yp dmin dmax then begin
			mur.(!nprises) <- {x = xp; y = yp; diff = diffp; teta = tetap};
			incr nprises
		end
	done;
	mur


let generer_pop (m:int) (n:int) (dmin : float) (dmax : float) : (prise array * float) array = (*genere m murs de n prises aléatoirs*)
	let pop = Array.make m ([||], 0.) in
	for i=0 to m-1 do
		let m = generer_mur n dmin dmax in
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



	

	



