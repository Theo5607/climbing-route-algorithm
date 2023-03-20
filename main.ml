type tprise = Bac | Reglette | Pince | Plat;;

type mouv = Basique | Croise | Jete | Epaule | Relance;;

type prise = {
  difficulte: float;
  t: tprise;
  angle: int;
};;

let a = Array.make 3 (Array.make 3 {difficulte = 0.0; t = Bac; angle = 0}) in

let p = [|{difficulte = 5.5; t = Pince; angle = 0};
{difficulte = 3.0; t = Bac; angle = 1};
{difficulte = 6.0; t = Reglette; angle = 0};
{difficulte = 2.0; t = Bac; angle = 4};
{difficulte = 7.0; t = Plat; angle = 0};
{difficulte = 1.0; t = Bac; angle = 2};
{difficulte = 2.0; t = Bac; angle = 0};
{difficulte = 6.0; t = Reglette; angle = 1};
{difficulte = 3.0; t = Bac; angle = 7}|] in

for i = 0 to Array.length a - 1 do
  for j = 0 to Array.length a.(0) - 1 do
    a.(i).(j) <- p.(i + j);
  done;
done;