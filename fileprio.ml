type ('a, 'b) t = ('b * 'a) Tas.t

let est_vide file =
    Tas.est_vide file


let cree taille_max defval defprio =
    Tas.cree taille_max (defprio, defval)
    
let taille file = file.Tas.taille

let ajoute file x p =
    Tas.ajoute (p, x) file

let retire file =
    let _, x = Tas.extrait_minimum file in
    x
    
let diminue_priorite file x p =
    let i = Tas.recherche file (fun (_, y) -> x = y) in
    Tas.remplace file i (p, x)
