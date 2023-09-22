module Tas = struct 

    type 'a t = {
    elements : 'a array;
    mutable taille : int
    }

    let cree taille_max defaut =
        {
            elements = Array.make taille_max defaut;
            taille = 0
        }

    let est_vide tas = tas.taille = 0

    let taille tas = tas.taille

    let pere a =
        if a = 0
        then failwith "racine"
        else (a+1)/2 - 1

    let gauche a = 2*(a+1) - 1
    let droite a = 2*(a+1)

    let minimum tas = tas.elements.(0)

    let nth tas i = tas.elements.(i)

    let swap v x y =
        let t = v.(x) in
        v.(x) <- v.(y);
        v.(y) <- t

    let ajoute x tas =
        let pos = ref tas.taille in
        tas.elements.(tas.taille) <- x;
        tas.taille <- tas.taille + 1;
        while !pos > 0 && nth tas (pere !pos) > nth tas !pos do
            let p = pere !pos in
            swap tas.elements p !pos;
            pos := p
        done

    let min_pere_fils tas a =
        let min_courant = ref a in

        if tas.taille > gauche a
            && nth tas (gauche a) < nth tas !min_courant
        then min_courant := gauche a;
        if tas.taille > droite a
            && nth tas (droite a) < nth tas !min_courant
        then min_courant := droite a;
        !min_courant

    let extrait_minimum tas =
        assert (tas.taille > 0);
        tas.taille <- tas.taille - 1;
        let x = tas.elements.(0) in
        if tas.taille > 0
        then begin
            let pos = ref 0 in
            tas.elements.(0) <- tas.elements.(tas.taille);
            while min_pere_fils tas !pos <> !pos do
                let min_fils = min_pere_fils tas !pos in
                swap tas.elements !pos min_fils;
                pos := min_fils
            done
        end;
        x
        
    let remplace tas i p =
        tas.elements.(i) <- p;
        let j = ref i in
        while (!j <> 0 && tas.elements.(pere !j) > p) do
            swap tas.elements !j (pere !j);
            j := pere !j;
        done;

    exception Trouve of int
    let recherche tas f =
        try
            for i = 0 to tas.taille - 1 do
                if f (nth tas i)
                then raise (Trouve i)
            done;
            raise Not_found
        with Trouve i -> i
end
