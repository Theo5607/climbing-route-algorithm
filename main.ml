type prise = { x : float; y : float}

let t_p = [|{x = 0.; y = 0.}; {x = 1.; y = 1.}; {x = 5.; y = 2.}; {x=0.; y=2.}; {x=1.; y=3.}|]

let heuristique i j t_p =
    let p1 = t_p.(i) and p2 = t_p.(j) in
    let p = sqrt ((p2.x -. p1.x) ** 2. +. (p2.y -. p1.y) ** 2.) in
    if p < 1.7 && p2.y > p1.y then Some p else None

let init_graphe =
    let d = 1.7 in
    let t = Array.sort (fun a b -> if a.x > a.y then 1 else if a.x < a.y else 0) t_p in
