type tprise = Bac | Reglette | Pince | Plat;;

type prise = {
  difficulte: float;
  t: tprise;
  angle: int;
};;

let a = [|{difficulte = 5.5; t = Pince; angle = 0}|]