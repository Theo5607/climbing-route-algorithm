open Backtracking (*utiliser mod_use*)
let xmax = 1.;;
let ymax = 1.;;
Random.self_init ();;   

let generer_mur (n : int) : prise array =
	let mur = Array.make (n) {x = 0.; y = 0.; diff = 0.; teta = 0.} in
	for i=1 to n-2 do
		let xp = Random.float xmax in
		let yp = Random.float ymax in
		let dp = float_of_int (Random.int 6) in
		let tp = Random.float 360. in
		mur.(i) <- {x = xp; y = yp; diff = dp; teta = tp}
	done;
	mur.(n-1) <- {x = xmax; y = ymax; diff = 5.; teta = 0.};
	mur

let generer_pop (m:int) (n:int) : (prise array * float) array =
	let pop = Array.make m ([||], 0.) in
	for i=0 to m-1 do
		let m = generer_mur n in
		let c, d = chemin_optimal m in
		pop.(i) <- m, d
	done;
	pop

let chemin_glouton (m : prise array) =
	



